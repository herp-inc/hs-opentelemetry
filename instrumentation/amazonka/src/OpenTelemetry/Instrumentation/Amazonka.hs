{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- | This is an instrumentation to add the OpenTelemetry tracings to the amazonka library.

You must use hs-opentelemetry-instrumentation-http-client also to trace the HTTP requests and to propagate the tracings.
Use amazonka's 'Amazonka.Env.newEnvNoAuthFromManager' that makes a specified 'HTTP.Manager' to be used for the HTTP requests.
-}
module OpenTelemetry.Instrumentation.Amazonka (
  appendHooksToEnv,
) where

import Amazonka (
  AWSRequest (AWSResponse),
  ClientResponse,
  Env' (Env, hooks),
  Error (SerializeError, ServiceError, TransportError),
  Request (Request, service),
  Service (Service, abbrev),
 )
import Amazonka.Data (ToText (toText))
import qualified Amazonka.Env.Hooks as Hooks
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (fold)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.TLS.GHC as TLS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Network.HTTP.Client as HTTP
import qualified OpenTelemetry.Attributes.Key as Otel
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_amazonka (version)


appendHooksToEnv :: (MonadIO m, HasCallStack) => Otel.TracerProvider -> Env' withAuth -> m (Env' withAuth)
appendHooksToEnv tracerProvider env@Env {hooks} = withFrozenCallStack $ liftIO $ do
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
  pure $
    env
      { hooks =
          hooks
            & Hooks.configuredRequestHook (configuredRequestHook tracer tls)
            & Hooks.responseHook (responseHook tls)
            & Hooks.errorHook (errorHook tls)
      }


type ThreadLocalStorage = TLS.TLS (IORef (Maybe Otel.Span))


configuredRequestHook :: HasCallStack => Otel.Tracer -> ThreadLocalStorage -> Hooks.Hook (Request a) -> Hooks.Hook (Request a)
configuredRequestHook tracer tls hook env request = do
  context <- Otel.getContext
  let attributes = HashMap.unions [makeAwsRequestAttributes request]
  span <- Otel.createSpan tracer context "AWS request" Otel.defaultSpanArguments {Otel.kind = Otel.Client, Otel.attributes}
  spanRef <- TLS.getTLS tls
  maybeSpan <- readIORef spanRef
  case maybeSpan of
    Nothing -> writeIORef spanRef $ Just span
    Just _ -> assert False $ pure () -- something went wrong
  hook env request


responseHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Request a, ClientResponse (AWSResponse a)) ->
  Hooks.Hook_ (Request a, ClientResponse (AWSResponse a))
responseHook tls hook env (request, response) = do
  hook env (request, response)
  spanRef <- TLS.getTLS tls
  maybeSpan <- readIORef spanRef
  case maybeSpan of
    Just span -> do
      Otel.addAttributes span $ makeAwsResponseAttributes request response
      Otel.endSpan span Nothing
      writeIORef spanRef Nothing
    _ -> assert False $ pure () -- something went wrong


errorHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Hooks.Finality, Request a, Error) ->
  Hooks.Hook_ (Hooks.Finality, Request a, Error)
errorHook tls hook env (finality, request, error) = do
  hook env (finality, request, error)
  spanRef <- TLS.getTLS tls
  maybeSpan <- readIORef spanRef
  case maybeSpan of
    Just span -> do
      Otel.addAttributes span $ makeFinalErrorAttributes error
      Otel.endSpan span Nothing
      writeIORef spanRef Nothing
    _ -> assert False $ pure () -- something went wrong


makeThreadLocalStorage :: IO ThreadLocalStorage
makeThreadLocalStorage = TLS.mkTLS $ newIORef Nothing


makeAwsRequestAttributes :: Request a -> Otel.AttributeMap
makeAwsRequestAttributes Request {service = Service {abbrev}} =
  mempty
    -- AWS attributes
    -- attributes to dismiss: rpc.method
    & Otel.insertByKey Otel.rpc_service (toText abbrev)
    & Otel.insertByKey Otel.rpc_system "aws-api"


makeAwsResponseAttributes :: Request a -> ClientResponse (AWSResponse a) -> Otel.AttributeMap
makeAwsResponseAttributes _ clientResponse =
  let maybeRequestId = Text.decodeLatin1 <$> fold (flip lookup (HTTP.responseHeaders clientResponse) <$> ["x-amz-request-id", "x-amzn-requestid"])
   in mempty & maybe id (Otel.insertByKey Otel.aws_requestId) maybeRequestId


makeFinalErrorAttributes :: Error -> Otel.AttributeMap
makeFinalErrorAttributes error =
  let
    errorType :: Text
    errorType =
      case error of
        TransportError _ -> "transport error"
        SerializeError _ -> "serialize error"
        ServiceError _ -> "service error"
   in
    mempty & Otel.insertByKey Otel.error_type errorType
