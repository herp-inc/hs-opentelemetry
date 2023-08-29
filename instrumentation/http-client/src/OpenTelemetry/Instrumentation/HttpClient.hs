{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

{- | Offer a few options for HTTP instrumentation

- Add attributes via 'Request' and 'Response' to an existing span (Best)
- Use internals to instrument a particular callsite using modifyRequest, modifyResponse (Next best)
- Provide a middleware to pull from the thread-local state (okay)
- Modify the global manager to pull from the thread-local state (least good, can't be helped sometimes)
-}
module OpenTelemetry.Instrumentation.HttpClient (
  Manager (..),
  newManager,
  ManagerSettings (..),
  defaultManagerSettings,
  managerConnCount,
  managerRawConnection,
  managerTlsConnection,
  managerResponseTimeout,
  managerRetryableException,
  managerWrapException,
  managerIdleConnectionCount,
  managerModifyRequest,
  managerModifyResponse,
  managerSetProxy,
  managerSetInsecureProxy,
  managerSetSecureProxy,
  withResponse,
  httpLbs,
  httpNoBody,
  responseOpen,
  httpClientInstrumentationConfig,
  HttpClientInstrumentationConfig (..),
  module X,
) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy as L
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Network.HTTP.Client as X hiding (
  Manager,
  ManagerSettings,
  defaultManagerSettings,
  httpLbs,
  httpNoBody,
  managerConnCount,
  managerIdleConnectionCount,
  managerModifyRequest,
  managerModifyResponse,
  managerRawConnection,
  managerResponseTimeout,
  managerRetryableException,
  managerSetInsecureProxy,
  managerSetProxy,
  managerSetSecureProxy,
  managerTlsConnection,
  managerWrapException,
  newManager,
  responseOpen,
  withResponse,
 )
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client
import Network.Socket (HostAddress)
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Instrumentation.HttpClient.Raw (
  HttpClientInstrumentationConfig (..),
  httpClientInstrumentationConfig,
  httpTracerProvider,
  instrumentRequest,
  instrumentResponse,
  makeTracer,
 )
import OpenTelemetry.Trace.Core (
  SpanArguments (kind),
  SpanKind (Client),
  Tracer,
  TracerProvider,
  defaultSpanArguments,
  inSpan,
 )
import UnliftIO (MonadUnliftIO, askRunInIO)


data Manager = Manager
  { originalManager :: Client.Manager
  , tracer :: Tracer
  , config :: HttpClientInstrumentationConfig
  }


instance Client.HasHttpManager Manager where
  getHttpManager = originalManager


data ManagerSettings = ManagerSettings
  { originalSettings :: Client.ManagerSettings
  , tracerProvider :: Maybe TracerProvider
  -- ^ A used tracer provider. When you want to use the global tracer provider, set 'Nothing'.
  , config :: HttpClientInstrumentationConfig
  }


defaultManagerSettings :: ManagerSettings
defaultManagerSettings =
  ManagerSettings
    { originalSettings = Client.defaultManagerSettings
    , tracerProvider = Nothing
    , config = httpClientInstrumentationConfig
    }


managerConnCount :: ManagerSettings -> Int
managerConnCount = Client.managerConnCount . originalSettings


managerRawConnection :: ManagerSettings -> IO (Maybe HostAddress -> String -> Int -> IO Client.Connection)
managerRawConnection = Client.managerRawConnection . originalSettings


managerTlsConnection :: ManagerSettings -> IO (Maybe HostAddress -> String -> Int -> IO Client.Connection)
managerTlsConnection = Client.managerTlsConnection . originalSettings


managerResponseTimeout :: ManagerSettings -> ResponseTimeout
managerResponseTimeout = Client.managerResponseTimeout . originalSettings


managerRetryableException :: ManagerSettings -> SomeException -> Bool
managerRetryableException = Client.managerRetryableException . originalSettings


managerWrapException :: ManagerSettings -> forall a. Request -> IO a -> IO a
managerWrapException settings = Client.managerWrapException $ originalSettings settings


managerIdleConnectionCount :: ManagerSettings -> Int
managerIdleConnectionCount = Client.managerIdleConnectionCount . originalSettings


managerModifyRequest :: ManagerSettings -> Request -> IO Request
managerModifyRequest = Client.managerModifyRequest . originalSettings


managerModifyResponse :: ManagerSettings -> Response BodyReader -> IO (Response BodyReader)
managerModifyResponse = Client.managerModifyResponse . originalSettings


managerSetProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetProxy o settings@ManagerSettings {originalSettings} =
  settings {originalSettings = Client.managerSetProxy o originalSettings}


managerSetInsecureProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetInsecureProxy o settings@ManagerSettings {originalSettings} =
  settings {originalSettings = Client.managerSetInsecureProxy o originalSettings}


managerSetSecureProxy :: ProxyOverride -> ManagerSettings -> ManagerSettings
managerSetSecureProxy o settings@ManagerSettings {originalSettings} =
  settings {originalSettings = Client.managerSetSecureProxy o originalSettings}


newManager :: ManagerSettings -> IO Manager
newManager ManagerSettings {originalSettings, tracerProvider, config} = do
  originalManager <- Client.newManager originalSettings
  tracer <-
    case tracerProvider of
      Just tp -> pure $ makeTracer tp
      Nothing -> httpTracerProvider
  pure $ Manager {originalManager, tracer, config}


spanArgs :: SpanArguments
spanArgs = defaultSpanArguments {kind = Client}


{- | Instrumented variant of @Network.HTTP.Client.withResponse@

 Perform a @Request@ using a connection acquired from the given @Manager@,
 and then provide the @Response@ to the given function. This function is
 fully exception safe, guaranteeing that the response will be closed when the
 inner function exits. It is defined as:

 > withResponse req man f = bracket (responseOpen req man) responseClose f

 It is recommended that you use this function in place of explicit calls to
 'responseOpen' and 'responseClose'.

 You will need to use functions such as 'brRead' to consume the response
 body.
-}
withResponse ::
  (MonadUnliftIO m, HasCallStack) =>
  Client.Request ->
  Manager ->
  (Client.Response Client.BodyReader -> m a) ->
  m a
withResponse req Manager {originalManager, tracer, config} f =
  withFrozenCallStack $ do
    inSpan tracer "withResponse" spanArgs $ do
      ctxt <- getContext
      -- TODO would like to capture the req/resp time specifically
      -- inSpan "http.request" (defaultSpanArguments { startingKind = Client }) $ \httpReqSpan -> do
      req' <- instrumentRequest tracer config ctxt req
      runInIO <- askRunInIO
      liftIO $ Client.withResponse req' originalManager $ \resp -> do
        _ <- instrumentResponse tracer config ctxt resp
        runInIO $ f resp


{- | A convenience wrapper around 'withResponse' which reads in the entire
 response body and immediately closes the connection. Note that this function
 performs fully strict I\/O, and only uses a lazy ByteString in its response
 for memory efficiency. If you are anticipating a large response body, you
 are encouraged to use 'withResponse' and 'brRead' instead.
-}
httpLbs :: (MonadUnliftIO m, HasCallStack) => Client.Request -> Manager -> m (Client.Response L.ByteString)
httpLbs req Manager {originalManager, tracer, config} =
  withFrozenCallStack $ do
    inSpan tracer "httpLbs" spanArgs $ do
      ctxt <- getContext
      req' <- instrumentRequest tracer config ctxt req
      resp <- liftIO $ Client.httpLbs req' originalManager
      _ <- instrumentResponse tracer config ctxt resp
      pure resp


{- | A convenient wrapper around 'withResponse' which ignores the response
 body. This is useful, for example, when performing a HEAD request.
-}
httpNoBody :: (MonadUnliftIO m, HasCallStack) => Client.Request -> Manager -> m (Client.Response ())
httpNoBody req Manager {originalManager, tracer, config} =
  withFrozenCallStack $ do
    inSpan tracer "httpNoBody" spanArgs $ do
      ctxt <- getContext
      req' <- instrumentRequest tracer config ctxt req
      resp <- liftIO $ Client.httpNoBody req' originalManager
      _ <- instrumentResponse tracer config ctxt resp
      pure resp


{- | The most low-level function for initiating an HTTP request.

 The first argument to this function gives a full specification
 on the request: the host to connect to, whether to use SSL,
 headers, etc. Please see 'Request' for full details.  The
 second argument specifies which 'Manager' should be used.

 This function then returns a 'Response' with a
 'BodyReader'.  The 'Response' contains the status code
 and headers that were sent back to us, and the
 'BodyReader' contains the body of the request.  Note
 that this 'BodyReader' allows you to have fully
 interleaved IO actions during your HTTP download, making it
 possible to download very large responses in constant memory.

 An important note: the response body returned by this function represents a
 live HTTP connection. As such, if you do not use the response body, an open
 socket will be retained indefinitely. You must be certain to call
 'responseClose' on this response to free up resources.

 This function automatically performs any necessary redirects, as specified
 by the 'redirectCount' setting.

 When implementing a (reverse) proxy using this function or relating
 functions, it's wise to remove Transfer-Encoding:, Content-Length:,
 Content-Encoding: and Accept-Encoding: from request and response
 headers to be relayed.
-}
responseOpen :: (MonadUnliftIO m, HasCallStack) => Client.Request -> Manager -> m (Client.Response Client.BodyReader)
responseOpen req Manager {originalManager, tracer, config} =
  withFrozenCallStack $ do
    inSpan tracer "responseOpen" spanArgs $ do
      ctxt <- getContext
      req' <- instrumentRequest tracer config ctxt req
      resp <- liftIO $ Client.responseOpen req' originalManager
      _ <- instrumentResponse tracer config ctxt resp
      pure resp
