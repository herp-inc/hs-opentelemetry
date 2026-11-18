{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.HttpClient (
  appendTraceHeadersToSettings,
  withHttpSpan,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (bimap)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Network.HTTP.Client as Orig
import qualified Network.HTTP.Types.Status as HT
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Propagator as Otel
import qualified OpenTelemetry.SemanticConventions as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_http_client (version)


{- | Add trace headers taken from the current context to every request.

The modifier does not start a span and does not store anything per thread.
Use 'withHttpSpan' around the call that should become a span.
-}
appendTraceHeadersToSettings ::
  (MonadIO m) =>
  Otel.TracerProvider ->
  Orig.ManagerSettings ->
  m Orig.ManagerSettings
appendTraceHeadersToSettings tracerProvider settings = liftIO $ do
  let
    propagator = Otel.getTracerProviderPropagators tracerProvider
    managerModifyRequest = Orig.managerModifyRequest settings
  pure
    settings
      { Orig.managerModifyRequest = \request -> do
          context <- Otel.getContext
          headers <- Otel.injector propagator context $ Orig.requestHeaders request
          managerModifyRequest request {Orig.requestHeaders = headers}
      }


{- | Run one HTTP call in a client span.

The span records the request attributes before the action runs, and the
response attributes when the action returns a response. The span is ended
if the action throws.
-}
withHttpSpan ::
  (HasCallStack) =>
  Otel.TracerProvider ->
  Orig.Request ->
  IO (Orig.Response body) ->
  IO (Orig.Response body)
withHttpSpan tracerProvider request action = withFrozenCallStack $ do
  let
    tracer = makeTracer tracerProvider
    arguments =
      Otel.defaultSpanArguments
        { Otel.kind = Otel.Client
        , Otel.attributes = makeRequestAttributes request
        }
  Otel.inSpan' tracer "request" arguments $ \span_ -> do
    response <- action
    Otel.addAttributes span_ $ makeResponseAttributes response
    pure response


makeTracer :: Otel.TracerProvider -> Otel.Tracer
makeTracer tracerProvider =
  Otel.makeTracer
    tracerProvider
    (Otel.InstrumentationLibrary "hs-opentelemetry-instrumentation-http-client" $ Text.pack $ showVersion version)
    Otel.tracerOptions


makeRequestAttributes :: Orig.Request -> Otel.AttributeMap
makeRequestAttributes request =
  let
    requestHeaders =
      bimap
        (Text.decodeLatin1 . CI.foldedCase)
        ((Text.dropAround (== ' ') <$>) . Text.split (== ',') . Text.decodeLatin1)
        <$> Orig.requestHeaders request
    methodOriginal = Text.decodeLatin1 $ Orig.method request
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
    resendCount = fromIntegral $ Orig.redirectCount request
    address = Text.decodeLatin1 $ Orig.host request
    port :: Int64
    port = fromIntegral $ Orig.port request
    url = Text.pack $ show $ Orig.getUri request
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


makeResponseAttributes :: Orig.Response a -> Otel.AttributeMap
makeResponseAttributes response =
  let
    responseHeaders =
      bimap
        (Text.decodeLatin1 . CI.foldedCase)
        ((Text.dropAround (== ' ') <$>) . Text.split (== ',') . Text.decodeLatin1)
        <$> Orig.responseHeaders response
    statusCode :: Int64
    statusCode = fromIntegral $ HT.statusCode $ Orig.responseStatus response
   in
    mempty
      & appEndo (fold $ Endo . (\(k, v) -> Otel.insertByKey (Otel.http_response_header k) v) <$> responseHeaders)
      & Otel.insertByKey Otel.http_response_statusCode statusCode
