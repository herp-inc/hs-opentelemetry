{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module OpenTelemetry.Instrumentation.GRPC (
  propagatableTraceableServer,
  propagatableTraceableClient,
  Propagatable (..),
  Traceable (..),
  convertToGrpcPropagator,
) where

import Control.Exception (assert, bracket)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char (toLower)
import qualified Data.Text as Text
import GHC.Exts (IsList (fromList, toList))
import qualified GHC.Generics as G
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Network.GRPC.HighLevel.Client as GRPC
import qualified Network.GRPC.HighLevel.Server as GRPC
import qualified Network.GRPC.LowLevel.Call as GRPC
import qualified Network.HTTP.Types.Header as HTTP
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Propagator as Otel
import qualified OpenTelemetry.Trace.Core as Otel


propagatableTraceableServer :: (Traceable service, Propagatable service, HasCallStack) => Otel.Tracer -> Otel.SpanArguments -> service -> service
propagatableTraceableServer tracer args = withFrozenCallStack $ propagatableService tracer . traceableService tracer args


propagatableTraceableClient :: (Traceable service, Propagatable service, HasCallStack) => Otel.Tracer -> Otel.SpanArguments -> service -> service
propagatableTraceableClient tracer args = withFrozenCallStack $ traceableService tracer args . propagatableService tracer


class Traceable service where
  -- | Wrap each rpc with 'Otel.inSpan'.
  --
  -- For example if you have a service like:
  --
  -- @
  -- data Service = Service { rpc1 :: Request -> 'IO' Responce } deriving Generic
  -- instance 'Traceable' Service
  -- @
  --
  -- then 'traceableService' is equivalent to:
  --
  -- @
  -- 'traceableService' tracer Service { rpc1 } = Service { rpc1 = 'inSpan' tracer "Service.rpc1" rpc1 }
  -- @
  traceableService :: HasCallStack => Otel.Tracer -> Otel.SpanArguments -> service -> service
  default traceableService :: (G.Generic service, GTraceable (G.Rep service), HasCallStack) => Otel.Tracer -> Otel.SpanArguments -> service -> service
  traceableService tracer args = withFrozenCallStack $ G.to . gTraceableService tracer args . G.from


class GTraceable rep where
  gTraceableService :: HasCallStack => Otel.Tracer -> Otel.SpanArguments -> rep a -> rep a


class GTraceableSelectors rep where
  gTraceableSelectors :: HasCallStack => Otel.Tracer -> String -> Otel.SpanArguments -> rep a -> rep a


instance (GTraceableSelectors f, G.Datatype dc, G.Constructor cc) => GTraceable (G.M1 G.D dc (G.M1 G.C cc f)) where
  gTraceableService tracer args datatypeRep@(G.M1 conRep@(G.M1 selsRep)) =
    assert (G.datatypeName datatypeRep == G.conName conRep) $
      G.M1 $
        G.M1 $
          gTraceableSelectors tracer (G.datatypeName datatypeRep) args selsRep


instance (G.Selector c) => GTraceableSelectors (G.M1 G.S c (G.K1 G.R (request -> IO response))) where
  gTraceableSelectors tracer serviceName args rep@(G.M1 (G.K1 rpc)) =
    assert (map toLower serviceName == map toLower (take (length serviceName) $ G.selName rep)) $
      let spanName = Text.pack serviceName <> "." <> Text.pack (drop (length serviceName) $ G.selName rep)
       in G.M1 $ G.K1 $ Otel.inSpan tracer spanName args . rpc


instance (GTraceableSelectors f, GTraceableSelectors g) => GTraceableSelectors (f G.:*: g) where
  gTraceableSelectors tracer serviceName args (rep1 G.:*: rep2) =
    let rep1' = gTraceableSelectors tracer serviceName args rep1
        rep2' = gTraceableSelectors tracer serviceName args rep2
     in rep1' G.:*: rep2'


{- | Convert a propagator for http-types headers to one for grpc-haskell headers.

Note: This is a lossy conversion. The http-types headers are case-insensitive, but the grpc-haskell headers are case-sensitive.
-}
convertToGrpcPropagator :: Otel.Propagator Otel.Context HTTP.RequestHeaders HTTP.ResponseHeaders -> Otel.Propagator Otel.Context GRPC.MetadataMap GRPC.MetadataMap
convertToGrpcPropagator Otel.Propagator {propagatorNames, extractor, injector} =
  Otel.Propagator
    { propagatorNames
    , extractor = \carrier context -> do
        let carrier' = grpcMetadataMapToHttpHeaders carrier
        extractor carrier' context
    , injector = \context carrier -> do
        let carrier' = grpcMetadataMapToHttpHeaders carrier
        httpHeadersToGrpcMetadataMap <$> injector context carrier'
    }


httpHeadersToGrpcMetadataMap :: [(CI ByteString, ByteString)] -> GRPC.MetadataMap
httpHeadersToGrpcMetadataMap = fromList . fmap (first CI.foldedCase)


grpcMetadataMapToHttpHeaders :: GRPC.MetadataMap -> [(CI ByteString, ByteString)]
grpcMetadataMapToHttpHeaders = fmap (first CI.mk) . toList


class Propagatable service where
  -- | Extract propagation headers from each rpc request on a server or inject propagation headers into each rpc request on a client.
  propagatableService :: HasCallStack => Otel.Tracer -> service -> service
  default propagatableService :: (G.Generic service, GPropagatable (G.Rep service), HasCallStack) => Otel.Tracer -> service -> service
  propagatableService tracer = withFrozenCallStack $ G.to . gPropagatableService tracer . G.from


class GPropagatable rep where
  gPropagatableService :: HasCallStack => Otel.Tracer -> rep a -> rep a


instance GPropagatable f => GPropagatable (G.M1 G.D dc (G.M1 G.C cc f)) where
  gPropagatableService tracer (G.M1 (G.M1 selsRep)) =
    G.M1 $ G.M1 $ gPropagatableService tracer selsRep


instance GPropagatable (G.M1 G.S c (G.K1 G.R (GRPC.ServerRequest streamType request response -> IO (GRPC.ServerResponse streamType response)))) where
  gPropagatableService tracer (G.M1 (G.K1 rpc)) =
    G.M1 $ G.K1 $ extractor tracer rpc


instance GPropagatable (G.M1 G.S c (G.K1 G.R (GRPC.ClientRequest streamType request response -> IO (GRPC.ClientResult streamType response)))) where
  gPropagatableService tracer (G.M1 (G.K1 rpc)) =
    G.M1 $ G.K1 $ injector tracer rpc


instance (GPropagatable f, GPropagatable g) => GPropagatable (f G.:*: g) where
  gPropagatableService tracer (rep1 G.:*: rep2) =
    let rep1' = gPropagatableService tracer rep1
        rep2' = gPropagatableService tracer rep2
     in rep1' G.:*: rep2'


extractor ::
  Otel.Tracer ->
  (GRPC.ServerRequest streamType request response -> IO (GRPC.ServerResponse streamType response)) ->
  GRPC.ServerRequest streamType request response ->
  IO (GRPC.ServerResponse streamType response)
extractor tracer action request = do
  let
    metadata = getMetadata request
    propagator = convertToGrpcPropagator $ Otel.getTracerProviderPropagators $ Otel.getTracerTracerProvider tracer
  bracket
    do
      context <- Otel.getContext
      newContext <- Otel.extractor propagator metadata context
      Otel.attachContext newContext
    do
      \case
        Nothing -> void Otel.detachContext
        Just previousContext -> void (Otel.attachContext previousContext)
    do const $ action request


getMetadata :: GRPC.ServerRequest streamType request response -> GRPC.MetadataMap
getMetadata (GRPC.ServerNormalRequest GRPC.ServerCall {GRPC.metadata} _) = metadata
getMetadata (GRPC.ServerReaderRequest GRPC.ServerCall {GRPC.metadata} _) = metadata
getMetadata (GRPC.ServerWriterRequest GRPC.ServerCall {GRPC.metadata} _ _) = metadata
getMetadata (GRPC.ServerBiDiRequest GRPC.ServerCall {GRPC.metadata} _ _) = metadata


injector ::
  Otel.Tracer ->
  (GRPC.ClientRequest streamType request response -> IO (GRPC.ClientResult streamType response)) ->
  GRPC.ClientRequest streamType request response ->
  IO (GRPC.ClientResult streamType response)
injector tracer action request = do
  let propagator = convertToGrpcPropagator $ Otel.getTracerProviderPropagators $ Otel.getTracerTracerProvider tracer
  context <- Otel.getContext
  let inject = Otel.injector propagator context
  case request of
    GRPC.ClientNormalRequest request' timeout metadata -> do
      newMetadata <- inject metadata
      action $ GRPC.ClientNormalRequest request' timeout newMetadata
    GRPC.ClientWriterRequest timeout metadata writer -> do
      newMetadata <- inject metadata
      action $ GRPC.ClientWriterRequest timeout newMetadata writer
    GRPC.ClientReaderRequest request' timeout metadata reader -> do
      newMetadata <- inject metadata
      action $ GRPC.ClientReaderRequest request' timeout newMetadata reader
    GRPC.ClientBiDiRequest timeout metadata bidi -> do
      newMetadata <- inject metadata
      action $ GRPC.ClientBiDiRequest timeout newMetadata bidi
