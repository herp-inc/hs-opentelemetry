{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OpenTelemetry.Instrumentation.GRPC (
  Traceable (..),
) where

import Control.Exception (assert)
import Data.Char (toLower)
import qualified Data.Text as Text
import qualified GHC.Generics as G
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified OpenTelemetry.Trace.Core as Otel


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
