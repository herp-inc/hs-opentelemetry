{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenTelemetry.Instrumentation.Amazonka (
  appendHooksToEnv,
  createHooks,
  ClientRequestHookUpdate (..),
  ClientResponseHookUpdate (..),
) where

import Amazonka (
  AWSRequest,
  ClientRequest,
  ClientResponse,
  Env' (Env, hooks),
  Request,
 )
import qualified Amazonka.Env.Hooks as Hooks
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (fold)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Monoid (Endo (Endo, appEndo))
import qualified Data.TLS.GHC as TLS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified OpenTelemetry.Attributes.Key as Otel
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_amazonka (version)


-- | Wrapper to avoid impredicative polymorphism.
newtype ClientRequestHookUpdate
  = ClientRequestHookUpdate (Hooks.Hook ClientRequest -> Hooks.Hook ClientRequest)


-- | Wrapper to avoid impredicative polymorphism.
newtype ClientResponseHookUpdate
  = ClientResponseHookUpdate
      ( forall a.
        (AWSRequest a, Typeable a) =>
        Hooks.Hook_ (Request a, ClientResponse ()) ->
        Hooks.Hook_ (Request a, ClientResponse ())
      )


appendHooksToEnv :: (MonadIO m, HasCallStack) => Otel.TracerProvider -> Env' withAuth -> m (Env' withAuth)
appendHooksToEnv tracerProvider e@Env {hooks} = withFrozenCallStack $ liftIO $ do
  (ClientRequestHookUpdate clientRequestHook, ClientResponseHookUpdate clientResponseHook) <- liftIO $ createHooks tracerProvider
  pure $ e {hooks = hooks & Hooks.clientRequestHook clientRequestHook & Hooks.clientResponseHook clientResponseHook}


createHooks ::
  HasCallStack =>
  Otel.TracerProvider ->
  IO
    ( ClientRequestHookUpdate
    , ClientResponseHookUpdate
    )
createHooks tracerProvider = withFrozenCallStack $ do
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


clientRequestHook :: HasCallStack => Otel.Tracer -> ThreadLocalStorage -> Hooks.Hook ClientRequest -> Hooks.Hook ClientRequest
clientRequestHook tracer tls hook env request = do
  context <- Otel.getContext
  let
    attributes =
      HashMap.unions
        [ Otel.makeCodeAttributes callStack
        , makeRequestAttributes request
        ]
  span <- Otel.createSpanWithoutCallStack tracer context "request" Otel.defaultSpanArguments {Otel.kind = Otel.Client, Otel.attributes}
  spanRef <- TLS.getTLS tls
  writeIORef spanRef $ Just span
  hook env request


clientResponseHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Request a, ClientResponse ()) ->
  Hooks.Hook_ (Request a, ClientResponse ())
clientResponseHook tls hook env (request, response) = do
  hook env (request, response)
  spanRef <- TLS.getTLS tls
  span <- readIORef spanRef
  writeIORef spanRef Nothing
  case span of
    Nothing -> assert False $ pure () -- something went wrong
    Just span -> do
      Otel.addAttributes span $ makeResponseAttributes response
      Otel.endSpan span Nothing


makeThreadLocalStorage :: IO ThreadLocalStorage
makeThreadLocalStorage = TLS.mkTLS $ newIORef Nothing


makeRequestAttributes :: ClientRequest -> Otel.AttributeMap
makeRequestAttributes request =
  let
    -- instrumentation/http-client に寄せることを検討する
    requestHeaders =
      bimap
        (Text.decodeLatin1 . CI.foldedCase)
        ((Text.dropAround (== ' ') <$>) . Text.split (== ',') . Text.decodeLatin1)
        <$> HTTP.requestHeaders request
    methodOriginal = Text.decodeLatin1 $ HTTP.method request
    method :: Text
    method =
      case CI.foldCase methodOriginal of
        "get" -> "GET"
        "head" -> "HEAD"
        "post" -> "POST"
        "put" -> "PUT"
        "delete" -> "DELETE"
        "connect" -> "CONNECT"
        "options" -> "OPTIONS"
        "trace" -> "TRACE"
        "patch" -> "PATCH"
        _ -> "_OTHER"
    resendCount :: Int64
    resendCount = fromIntegral $ HTTP.redirectCount request
    address = Text.decodeLatin1 $ HTTP.host request
    port :: Int64
    port = fromIntegral $ HTTP.port request
    url = Text.pack $ show $ HTTP.getUri request
   in
    mempty
      -- HTTP attributes
      -- attributes to dismiss: http.request.body.size, http.response.body.size, network.protocol.version
      & appEndo (fold $ Endo . (\(k, v) -> Otel.insertByKey (Otel.http_request_header k) v) <$> requestHeaders)
      & Otel.insertByKey Otel.http_request_method method
      & Otel.insertByKey Otel.http_request_methodOriginal methodOriginal
      & Otel.insertByKey Otel.http_request_resendCount resendCount
      & Otel.insertByKey Otel.network_peer_address address
      & Otel.insertByKey Otel.network_peer_port port
      & Otel.insertByKey Otel.network_protocol_name "http"
      & Otel.insertByKey Otel.network_transport "tcp"
      & Otel.insertByKey Otel.server_address address
      & Otel.insertByKey Otel.server_port port
      & Otel.insertByKey Otel.url_full url
      -- AWS attributes
      -- attributes to dismiss: rpc.method, rpc.service
      & Otel.insertByKey Otel.rpc_system "aws-api"


makeResponseAttributes :: ClientResponse () -> Otel.AttributeMap
makeResponseAttributes response =
  let
    responseHeaders =
      bimap
        (Text.decodeLatin1 . CI.foldedCase)
        ((Text.dropAround (== ' ') <$>) . Text.split (== ',') . Text.decodeLatin1)
        <$> HTTP.responseHeaders response
    statusCode :: Int64
    statusCode = fromIntegral $ HTTP.statusCode $ HTTP.responseStatus response
    maybeRequestId = Text.decodeLatin1 <$> fold (flip lookup (HTTP.responseHeaders response) <$> ["x-amz-request-id", "x-amzn-requestid"])
   in
    mempty
      & appEndo (fold $ Endo . (\(k, v) -> Otel.insertByKey (Otel.http_response_header k) v) <$> responseHeaders)
      & Otel.insertByKey Otel.http_response_statusCode statusCode
      & maybe id (Otel.insertByKey Otel.aws_requestId) maybeRequestId
