{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module OpenTelemetry.Instrumentation.Amazonka (
  appendHooksToEnv,
) where

import Amazonka (
  AWSRequest (AWSResponse),
  ClientRequest,
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
import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (fold)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Monoid (Endo (Endo, appEndo))
import qualified Data.TLS.GHC as TLS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified OpenTelemetry.Attributes.Key as Otel
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_amazonka (version)


-- | Wrapper to avoid impredicative polymorphism.
newtype ConfiguredRequestHookUpdate
  = ConfiguredRequestHookUpdate (forall a. (AWSRequest a, Typeable a, HasCallStack) => Hooks.Hook (Request a) -> Hooks.Hook (Request a))


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


-- | Wrapper to avoid impredicative polymorphism.
newtype ResponseHookUpdate
  = ResponseHookUpdate
      ( forall a.
        (AWSRequest a, Typeable a) =>
        Hooks.Hook_ (Request a, ClientResponse (AWSResponse a)) ->
        Hooks.Hook_ (Request a, ClientResponse (AWSResponse a))
      )


-- | Wrapper to avoid impredicative polymorphism.
newtype ErrorHookUpdate
  = ErrorHookUpdate
      ( forall a.
        (AWSRequest a, Typeable a) =>
        Hooks.Hook_ (Hooks.Finality, Request a, Error) ->
        Hooks.Hook_ (Hooks.Finality, Request a, Error)
      )


appendHooksToEnv :: (MonadIO m, HasCallStack) => Otel.TracerProvider -> Env' withAuth -> m (Env' withAuth)
appendHooksToEnv tracerProvider e@Env {hooks} = withFrozenCallStack $ liftIO $ do
  ( ConfiguredRequestHookUpdate configuredRequestHook
    , ClientRequestHookUpdate clientRequestHook
    , ClientResponseHookUpdate clientResponseHook
    , ResponseHookUpdate responseHook
    , ErrorHookUpdate errorHook
    ) <-
    liftIO $ createHooks tracerProvider
  pure $
    e
      { hooks =
          hooks
            & Hooks.configuredRequestHook configuredRequestHook
            & Hooks.clientRequestHook clientRequestHook
            & Hooks.clientResponseHook clientResponseHook
            & Hooks.responseHook responseHook
            & Hooks.errorHook errorHook
      }


createHooks ::
  HasCallStack =>
  Otel.TracerProvider ->
  IO
    ( ConfiguredRequestHookUpdate
    , ClientRequestHookUpdate
    , ClientResponseHookUpdate
    , ResponseHookUpdate
    , ErrorHookUpdate
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
    ( ConfiguredRequestHookUpdate $ configuredRequestHook tracer tls
    , ClientRequestHookUpdate $ clientRequestHook tracer tls
    , ClientResponseHookUpdate $ clientResponseHook tls
    , ResponseHookUpdate $ responseHook tls
    , ErrorHookUpdate $ errorHook tls
    )


type ThreadLocalStorage = TLS.TLS (IORef (UpTo2Lifo Otel.Span))


configuredRequestHook :: HasCallStack => Otel.Tracer -> ThreadLocalStorage -> Hooks.Hook (Request a) -> Hooks.Hook (Request a)
configuredRequestHook tracer tls hook env request = do
  context <- Otel.getContext
  let attributes = HashMap.unions [makeAwsRequestAttributes request]
  span <- Otel.createSpan tracer context "AWS request" Otel.defaultSpanArguments {Otel.kind = Otel.Client, Otel.attributes}
  spansRef <- TLS.getTLS tls
  modifyIORef' spansRef $ u2Cons span
  hook env request


clientRequestHook :: HasCallStack => Otel.Tracer -> ThreadLocalStorage -> Hooks.Hook ClientRequest -> Hooks.Hook ClientRequest
clientRequestHook tracer tls hook env request = do
  context <- Otel.getContext
  let attributes = makeHttpRequestAttributes request
  span <- Otel.createSpan tracer context "HTTP request" Otel.defaultSpanArguments {Otel.kind = Otel.Client, Otel.attributes}
  spansRef <- TLS.getTLS tls
  modifyIORef' spansRef $ u2Cons span
  hook env request


clientResponseHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Request a, ClientResponse ()) ->
  Hooks.Hook_ (Request a, ClientResponse ())
clientResponseHook tls hook env (request, response) = do
  hook env (request, response)
  spansRef <- TLS.getTLS tls
  spans <- readIORef spansRef
  case u2Uncons spans of
    Just (span, spans') -> do
      Otel.addAttributes span $ makeHttpResponseAttributes response
      Otel.endSpan span Nothing
      writeIORef spansRef spans'
    _ -> assert False $ pure () -- something went wrong


responseHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Request a, ClientResponse (AWSResponse a)) ->
  Hooks.Hook_ (Request a, ClientResponse (AWSResponse a))
responseHook tls hook env (request, response) = do
  hook env (request, response)
  spansRef <- TLS.getTLS tls
  spans <- readIORef spansRef
  case u2Uncons spans of
    Just (span, spans') -> do
      Otel.addAttributes span $ makeAwsResponseAttributes request response
      Otel.endSpan span Nothing
      writeIORef spansRef spans'
    _ -> assert False $ pure () -- something went wrong


errorHook ::
  ThreadLocalStorage ->
  forall a.
  (AWSRequest a, Typeable a) =>
  Hooks.Hook_ (Hooks.Finality, Request a, Error) ->
  Hooks.Hook_ (Hooks.Finality, Request a, Error)
-- errorHook _ hook env (Hooks.NotFinal, request, error) = hook env (Hooks.NotFinal, request, error)
errorHook tls hook env (finality, request, error) = do
  hook env (finality, request, error)
  spansRef <- TLS.getTLS tls
  spans <- readIORef spansRef
  case u2Uncons spans of
    Just (span, spans') -> do
      Otel.addAttributes span $ makeFinalErrorAttributes error
      Otel.endSpan span Nothing
      writeIORef spansRef spans'
    _ -> assert False $ pure () -- something went wrong


makeThreadLocalStorage :: IO ThreadLocalStorage
makeThreadLocalStorage = TLS.mkTLS $ newIORef $ UpTo2Lifo []


makeAwsRequestAttributes :: Request a -> Otel.AttributeMap
makeAwsRequestAttributes Request {service = Service {abbrev}} =
  mempty
    -- AWS attributes
    -- attributes to dismiss: rpc.method
    & Otel.insertByKey Otel.rpc_service (toText abbrev)
    & Otel.insertByKey Otel.rpc_system "aws-api"


makeHttpRequestAttributes :: ClientRequest -> Otel.AttributeMap
makeHttpRequestAttributes request =
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
      -- attributes to dismiss: error.type, http.request.body.size, http.response.body.size, network.protocol.version
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


makeHttpResponseAttributes :: ClientResponse () -> Otel.AttributeMap
makeHttpResponseAttributes response =
  let
    responseHeaders =
      bimap
        (Text.decodeLatin1 . CI.foldedCase)
        ((Text.dropAround (== ' ') <$>) . Text.split (== ',') . Text.decodeLatin1)
        <$> HTTP.responseHeaders response
    statusCode :: Int64
    statusCode = fromIntegral $ HTTP.statusCode $ HTTP.responseStatus response
   in
    mempty
      & appEndo (fold $ Endo . (\(k, v) -> Otel.insertByKey (Otel.http_response_header k) v) <$> responseHeaders)
      & Otel.insertByKey Otel.http_response_statusCode statusCode


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


newtype UpTo2Lifo a = UpTo2Lifo [a] deriving stock (Show, Read, Eq, Ord)


u2Cons :: a -> UpTo2Lifo a -> UpTo2Lifo a
u2Cons a (UpTo2Lifo []) = UpTo2Lifo [a]
u2Cons a (UpTo2Lifo (b : _)) = UpTo2Lifo [a, b]


u2Uncons :: UpTo2Lifo a -> Maybe (a, UpTo2Lifo a)
u2Uncons (UpTo2Lifo []) = Nothing
u2Uncons (UpTo2Lifo (a : as)) = Just (a, UpTo2Lifo as)
