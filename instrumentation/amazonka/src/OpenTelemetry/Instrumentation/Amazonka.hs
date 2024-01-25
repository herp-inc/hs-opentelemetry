{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenTelemetry.Instrumentation.Amazonka (
  createHooks,
  ClientRequestHookUpdate (..),
  ClientResponseHookUpdate (..),
) where

import qualified Amazonka as Orig
import qualified Amazonka.Env.Hooks as Orig
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.TLS.GHC as TLS
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import qualified Network.HTTP.Client as HTTP
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_amazonka (version)
import qualified OpenTelemetry.Attributes.Map as Otel
import Control.Exception (assert)


-- | Wrapper to avoid impredicative polymorphism.
newtype ClientRequestHookUpdate
  = ClientRequestHookUpdate (Orig.Hook Orig.ClientRequest -> Orig.Hook Orig.ClientRequest)


-- | Wrapper to avoid impredicative polymorphism.
newtype ClientResponseHookUpdate
  = ClientResponseHookUpdate
      ( forall a.
        (Orig.AWSRequest a, Typeable a) =>
        Orig.Hook_ (Orig.Request a, Orig.ClientResponse ()) ->
        Orig.Hook_ (Orig.Request a, Orig.ClientResponse ())
      )


createHooks ::
  Otel.TracerProvider ->
  IO
    ( ClientRequestHookUpdate
    , ClientResponseHookUpdate
    )
createHooks tracerProvider = do
  let
    tracer =
      Otel.makeTracer
        tracerProvider
        ( Otel.InstrumentationLibrary
            "hs-opentelemetry-instrumentation-amazonka"
            $ Text.pack
            $ showVersion version
        )
        Otel.tracerOptions
  tls <- makeThreadLocalStorage
  pure
    ( ClientRequestHookUpdate $ clientRequestHook tracer tls
    , ClientResponseHookUpdate $ clientResponseHook tls
    )


type ThreadLocalStorage = TLS.TLS (IORef (Maybe Otel.Span))


clientRequestHook :: Otel.Tracer -> ThreadLocalStorage -> Orig.Hook Orig.ClientRequest -> Orig.Hook Orig.ClientRequest
clientRequestHook tracer tls hook env request = do
  context <- Otel.getContext
  span <- Otel.createSpan tracer context "request" Otel.defaultSpanArguments
  spanRef <- TLS.getTLS tls
  writeIORef spanRef $ Just span
  hook env request


clientResponseHook ::
  ThreadLocalStorage ->
  forall a.
  (Orig.AWSRequest a, Typeable a) =>
  Orig.Hook_ (Orig.Request a, Orig.ClientResponse ()) ->
  Orig.Hook_ (Orig.Request a, Orig.ClientResponse ())
clientResponseHook tls hook env request = do
  hook env request
  spanRef <- TLS.getTLS tls
  span <- readIORef spanRef
  writeIORef spanRef Nothing
  case span of
    Nothing -> assert False $ pure () -- something went wrong
    Just span -> Otel.endSpan span Nothing


makeThreadLocalStorage :: IO ThreadLocalStorage
makeThreadLocalStorage = TLS.mkTLS $ newIORef Nothing

makeAttributes :: Orig.ClientRequest -> Otel.AttributeMap
makeAttributes request =
  let requestHeaders = HTTP.requestHeaders request
  in mempty
