{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | This is herp-logger with connections of OpenTelemetry Traces and Logs.

Datadog functionality about connections of Traces and Logs is described in
<https://docs.datadoghq.com/tracing/other_telemetry/connect_logs_and_traces/opentelemetry/ Connect OpenTelemetry Traces and Logs>.

This logger requires 'Otel.Tracer' to retrieve OpenTelemetry context additionally.
-}
module OpenTelemetry.Instrumentation.Herp.Logger.Datadog (
  (Orig..=),
  Logger (..),
  HasLogger (..),
  ToLogger (..),
  Orig.LogLevel (..),
  Orig.LoggerConfig (..),
  newLogger,
  withLogger,
  makeLogger,
  Orig.defaultLoggerConfig,
  logM,
  logOtherM,
  logDebugM,
  logInfoM,
  logNoticeM,
  logWarnM,
  logErrorM,
  logCritM,
  logAlertM,
  logEmergM,
  logIO,
  Orig.urgentLog,
  flush,

  -- * Payload
  Orig.Payload,
  Orig.level,
  Orig.message,
  Orig.object,
  Orig.messageString,
  Orig.messageShow,
  Orig.messageException,

  -- * monad-logger
  runLoggingT,
  toLoggerIO,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Monad.Logger as ML
import Control.Monad.Reader.Class (MonadReader (ask), asks)
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Herp.Logger ((.=))
import qualified Herp.Logger as Orig
import qualified Herp.Logger.LogLevel as Orig
import qualified Herp.Logger.Payload as Orig
import qualified OpenTelemetry.Attributes as Otel
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Resource as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.Vendor.Datadog as Datadog


data Logger = Logger {original :: Orig.Logger, tracer :: Otel.Tracer} deriving stock (Generic)


makeLogger :: Orig.Logger -> Otel.TracerProvider -> Logger
makeLogger original provider =
  Logger
    { original
    , tracer = Otel.makeTracer provider "hs-opentelemetry-instrumentation-herp-logger-datadog" Otel.tracerOptions
    }


newLogger :: Orig.LoggerConfig -> Otel.TracerProvider -> IO Logger
newLogger config provider = do
  original <- Orig.newLogger config
  pure $ makeLogger original provider


withLogger :: Orig.LoggerConfig -> Otel.TracerProvider -> (Logger -> IO a) -> IO a
withLogger config provider f =
  Orig.withLogger config $ \original -> f $ makeLogger original provider


class HasLogger a where
  toLogger :: a -> Logger


instance HasLogger Logger where
  toLogger = id


instance Orig.HasLogger Logger where
  toLogger = original . toLogger


instance Otel.HasTracer Logger where
  tracerL f Logger {tracer, original} = (\tracer -> Logger {original, tracer}) <$> f tracer


-- | This wrapper is intended to be used with /deriving via/.
newtype ToLogger a = ToLogger {getToLogger :: a}


instance HasLogger a => Orig.HasLogger (ToLogger a) where
  toLogger = original . toLogger . getToLogger


logIO :: MonadIO m => Logger -> Orig.Payload -> m ()
logIO Logger {original = logger, tracer} payload = do
  context <- Otel.getContext
  payload' <- datadogPayload (Otel.getTracerTracerProvider tracer) $ Otel.lookupSpan context
  Orig.logIO logger (payload' <> payload)
{-# INLINE logIO #-}


logM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logM payload = do
  logger <- toLogger <$> ask
  logIO logger payload
{-# INLINE logM #-}


logOtherM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.LogLevel -> Orig.Payload -> m ()
logOtherM logLevel payload = logM $ Orig.level logLevel <> payload


logDebugM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logDebugM = logOtherM Orig.Debug


logInfoM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logInfoM = logOtherM Orig.Informational


logNoticeM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logNoticeM = logOtherM Orig.Notice


logWarnM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logWarnM = logOtherM Orig.Warning


logErrorM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logErrorM = logOtherM Orig.Error


logCritM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logCritM = logOtherM Orig.Critical


logAlertM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logAlertM = logOtherM Orig.Alert


logEmergM :: (MonadIO m, MonadReader r m, HasLogger r) => Orig.Payload -> m ()
logEmergM = logOtherM Orig.Emergency


flush :: (MonadReader r m, HasLogger r, MonadIO m) => m ()
flush = asks toLogger >>= liftIO . Orig.loggerFlush . original


runLoggingT :: Logger -> ML.LoggingT m a -> m a
runLoggingT logger (ML.LoggingT run) = run $ toLoggerIO logger


toLoggerIO :: Logger -> ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()
toLoggerIO logger loc logSrc lv logStr = do
  let msg = Text.decodeUtf8 $ ML.fromLogStr $ ML.defaultLogStr loc logSrc lv logStr
  logIO
    logger
    [ Orig.message msg
    , case Orig.convertLogLevel lv of
        Right x -> Orig.level x
        Left other -> [#warn, "level" .= other]
    ]


datadogPayload :: MonadIO m => Otel.TracerProvider -> Maybe Otel.Span -> m Orig.Payload
datadogPayload tracerProvider maybeSpan = do
  (maybeSpanId, maybeTraceId) <-
    case maybeSpan of
      Nothing -> pure (Nothing, Nothing)
      Just span -> do
        Otel.SpanContext {Otel.spanId, Otel.traceId} <- Otel.getSpanContext span
        pure
          ( Just $ Text.pack $ show $ Datadog.convertOpenTelemetrySpanIdToDatadogSpanId spanId
          , Just $ Text.pack $ show $ Datadog.convertOpenTelemetryTraceIdToDatadogTraceId traceId
          )
  let
    attributes = Otel.getMaterializedResourcesAttributes $ Otel.getTracerProviderResources tracerProvider
    maybeEnv :: Maybe Text
    maybeEnv = Otel.lookupByKey Datadog.envKey $ Otel.getAttributes attributes
    maybeService =
      ( Otel.lookupByKey Datadog.serviceKey (Otel.getAttributes attributes)
          <|>
          -- "service.name" is the same key in the OpenTelemetry.Resource.Service module
          Otel.lookupByKey Otel.peer_service (Otel.getAttributes attributes)
      )
    maybeVersion = Otel.lookupByKey Datadog.versionKey (Otel.getAttributes attributes)
  pure $
    (\payloadObject -> mempty {Orig.payloadObject}) $
      Aeson.fromList $
        mconcat $
          maybeToList
            <$> [ ("span_id",) . Aeson.String <$> maybeSpanId
                , ("trace_id",) . Aeson.String <$> maybeTraceId
                , ("dd.env",) . Aeson.String <$> maybeEnv
                , ("dd.service",) . Aeson.String <$> maybeService
                , ("dd.version",) . Aeson.String <$> maybeVersion
                ]
