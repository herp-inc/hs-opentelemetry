{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OpenTelemetry.Instrumentation.GRPC.Client (
  Traceable (..),
  inSpan,
) where

import Control.Exception (assert)
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.Generics as G
import GHC.Stack (HasCallStack)
import qualified OpenTelemetry.Trace.Core as Otel


inSpan :: HasCallStack => Otel.Tracer -> Text -> (request -> IO response) -> request -> IO response
inSpan tracer name f r =
  Otel.inSpan tracer name Otel.defaultSpanArguments $ f r


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
  traceableService :: Otel.Tracer -> service -> service
  default traceableService :: (G.Generic service, GTraceable (G.Rep service)) => Otel.Tracer -> service -> service
  traceableService tracer = G.to . gTraceableService tracer . G.from


class GTraceable rep where
  gTraceableService :: Otel.Tracer -> rep a -> rep a


class GTraceableSelectors rep where
  gTraceableSelectors :: Otel.Tracer -> String -> rep a -> rep a


instance (GTraceableSelectors f, G.Datatype dc, G.Constructor cc) => GTraceable (G.M1 G.D dc (G.M1 G.C cc f)) where
  gTraceableService tracer datatypeRep@(G.M1 conRep@(G.M1 selsRep)) =
    assert (G.datatypeName datatypeRep == G.conName conRep) $
      G.M1 $
        G.M1 $
          gTraceableSelectors tracer (G.datatypeName datatypeRep) selsRep


instance G.Selector c => GTraceableSelectors (G.M1 G.S c (G.K1 G.R (request -> IO response))) where
  gTraceableSelectors tracer serviceName rep@(G.M1 (G.K1 rpc)) =
    assert (map toLower serviceName == map toLower (take (length serviceName) $ G.selName rep)) $
      let spanName = Text.pack serviceName <> "." <> Text.pack (drop (length serviceName) $ G.selName rep)
       in G.M1 $ G.K1 $ inSpan tracer spanName rpc


instance (GTraceableSelectors f, GTraceableSelectors g) => GTraceableSelectors (f G.:*: g) where
  gTraceableSelectors tracer serviceName (rep1 G.:*: rep2) =
    let rep1' = gTraceableSelectors tracer serviceName rep1
        rep2' = gTraceableSelectors tracer serviceName rep2
     in rep1' G.:*: rep2'
