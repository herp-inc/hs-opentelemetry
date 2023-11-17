{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.HttpClient.Simple (
  httpBS,
  httpBS',
  httpLBS,
  httpLBS',
  httpNoBody,
  httpNoBody',
  httpJSON,
  httpJSON',
  httpJSONEither,
  httpJSONEither',
  httpSink,
  httpSink',
  httpSource,
  httpSource',
  withResponse,
  withResponse',
  httpClientInstrumentationConfig,
  HttpClientInstrumentationConfig (..),
  module X,
) where

import Conduit (MonadResource, lift)
import Data.Aeson (FromJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Conduit (ConduitM, Void)
import Network.HTTP.Simple as X hiding (httpBS, httpJSON, httpJSONEither, httpLBS, httpNoBody, httpSink, httpSource, withResponse)
import qualified Network.HTTP.Simple as Simple
import OpenTelemetry.Context.ThreadLocal
import qualified OpenTelemetry.Instrumentation.Conduit as Conduit
import OpenTelemetry.Instrumentation.HttpClient.Raw
import OpenTelemetry.Trace.Core
import UnliftIO


spanArgs :: SpanArguments
spanArgs = defaultSpanArguments {kind = Client}


httpBS :: (MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response B.ByteString)
httpBS httpConf req = httpTracerProvider >>= \tracer -> httpBS' tracer httpConf req


httpBS' :: (MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response B.ByteString)
httpBS' tracer httpConf req = do
  inSpan' tracer "httpBS" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    resp <- Simple.httpBS req'
    _ <- instrumentResponse tracer httpConf ctxt resp
    pure resp


httpLBS :: (MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response L.ByteString)
httpLBS httpConf req = httpTracerProvider >>= \tracer -> httpLBS' tracer httpConf req


httpLBS' :: (MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response L.ByteString)
httpLBS' tracer httpConf req = do
  inSpan' tracer "httpLBS" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    resp <- Simple.httpLBS req'
    _ <- instrumentResponse tracer httpConf ctxt resp
    pure resp


httpNoBody :: (MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response ())
httpNoBody httpConf req = httpTracerProvider >>= \tracer -> httpNoBody' tracer httpConf req


httpNoBody' :: (MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response ())
httpNoBody' tracer httpConf req = do
  inSpan' tracer "httpNoBody" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    resp <- Simple.httpNoBody req'
    _ <- instrumentResponse tracer httpConf ctxt resp
    pure resp


httpJSON :: (MonadUnliftIO m, FromJSON a) => HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response a)
httpJSON httpConf req = httpTracerProvider >>= \tracer -> httpJSON' tracer httpConf req


httpJSON' :: (MonadUnliftIO m, FromJSON a) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response a)
httpJSON' tracer httpConf req = do
  inSpan' tracer "httpJSON" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    resp <- Simple.httpJSON req'
    _ <- instrumentResponse tracer httpConf ctxt resp
    pure resp


httpJSONEither :: (FromJSON a, MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response (Either Simple.JSONException a))
httpJSONEither httpConf req = httpTracerProvider >>= \tracer -> httpJSONEither' tracer httpConf req


httpJSONEither' :: (FromJSON a, MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> m (Simple.Response (Either Simple.JSONException a))
httpJSONEither' tracer httpConf req = do
  inSpan' tracer "httpJSONEither" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    resp <- Simple.httpJSONEither req'
    _ <- instrumentResponse tracer httpConf ctxt resp
    pure resp


httpSink :: (MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response () -> ConduitM B.ByteString Void m a) -> m a
httpSink httpConf req f = httpTracerProvider >>= \tracer -> httpSink' tracer httpConf req f


httpSink' :: (MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response () -> ConduitM B.ByteString Void m a) -> m a
httpSink' tracer httpConf req f = do
  inSpan' tracer "httpSink" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    Simple.httpSink req' $ \resp -> do
      _ <- instrumentResponse tracer httpConf ctxt resp
      f resp


httpSource :: (MonadUnliftIO m, MonadResource m) => HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response (ConduitM i B.ByteString m ()) -> ConduitM i o m r) -> ConduitM i o m r
httpSource httpConf req f = httpTracerProvider >>= \tracer -> httpSource' tracer httpConf req f


httpSource' :: (MonadUnliftIO m, MonadResource m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response (ConduitM i B.ByteString m ()) -> ConduitM i o m r) -> ConduitM i o m r
httpSource' tracer httpConf req f = do
  Conduit.inSpan tracer "httpSource" spanArgs $ \_s -> do
    ctxt <- lift getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    Simple.httpSource req' $ \resp -> do
      _ <- instrumentResponse tracer httpConf ctxt resp
      f resp


withResponse :: (MonadUnliftIO m) => HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response (ConduitM i B.ByteString m ()) -> m a) -> m a
withResponse httpConf req f = httpTracerProvider >>= \tracer -> withResponse' tracer httpConf req f


withResponse' :: (MonadUnliftIO m) => Tracer -> HttpClientInstrumentationConfig -> Simple.Request -> (Simple.Response (ConduitM i B.ByteString m ()) -> m a) -> m a
withResponse' tracer httpConf req f = do
  inSpan' tracer "withResponse" spanArgs $ \_s -> do
    ctxt <- getContext
    req' <- instrumentRequest tracer httpConf ctxt req
    Simple.withResponse req' $ \resp -> do
      _ <- instrumentResponse tracer httpConf ctxt resp
      f resp
