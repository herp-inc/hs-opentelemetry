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
  Orig.Logger (..),
  Orig.HasLogger (..),
  Orig.LogLevel (..),
  Orig.LoggerConfig (..),
  Orig.newLogger,
  Orig.withLogger,
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
  Orig.flush,

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
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Logger as ML
import Control.Monad.Reader.Class (MonadReader (ask))
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Herp.Logger ((.=))
import qualified Herp.Logger as Orig
import qualified Herp.Logger.LogLevel as Orig
import qualified Herp.Logger.Payload as Orig
import qualified OpenTelemetry.Attributes as Otel
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Resource as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.Vendor.Datadog as Datadog


logIO :: MonadIO m => Otel.Tracer -> Orig.Logger -> Orig.Payload -> m ()
logIO tracer logger payload = do
  context <- Otel.getContext
  payload' <- datadogPayload (Otel.getTracerTracerProvider tracer) $ Otel.lookupSpan context
  Orig.logIO logger (payload' <> payload)
{-# INLINE logIO #-}


logM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logM payload = do
  r <- ask
  void $ flip Otel.tracerL r $ \tracer -> do
    logIO tracer (Orig.toLogger r) payload
    pure tracer
{-# INLINE logM #-}


logOtherM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.LogLevel -> Orig.Payload -> m ()
logOtherM logLevel payload = logM $ Orig.level logLevel <> payload


logDebugM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logDebugM = logOtherM Orig.Debug


logInfoM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logInfoM = logOtherM Orig.Informational


logNoticeM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logNoticeM = logOtherM Orig.Notice


logWarnM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logWarnM = logOtherM Orig.Warning


logErrorM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logErrorM = logOtherM Orig.Error


logCritM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logCritM = logOtherM Orig.Critical


logAlertM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logAlertM = logOtherM Orig.Alert


logEmergM :: (MonadIO m, MonadReader r m, Orig.HasLogger r, Otel.HasTracer r) => Orig.Payload -> m ()
logEmergM = logOtherM Orig.Emergency


runLoggingT :: Otel.Tracer -> Orig.Logger -> ML.LoggingT m a -> m a
runLoggingT tracer logger (ML.LoggingT run) = run $ toLoggerIO tracer logger


toLoggerIO :: Otel.Tracer -> Orig.Logger -> ML.Loc -> ML.LogSource -> ML.LogLevel -> ML.LogStr -> IO ()
toLoggerIO tracer logger loc logSrc lv logStr = do
  let msg = Text.decodeUtf8 $ ML.fromLogStr $ ML.defaultLogStr loc logSrc lv logStr
  logIO
    tracer
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
    maybeEnv = attributeAsText =<< Otel.lookupAttribute attributes Datadog.envKey
    maybeService =
      attributeAsText
        =<< ( Otel.lookupAttribute attributes Datadog.serviceKey
                <|>
                -- "service.name" is the same key in the OpenTelemetry.Resource.Service module
                Otel.lookupAttribute attributes "service.name"
            )
    maybeVersion = attributeAsText =<< Otel.lookupAttribute attributes Datadog.versionKey
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


attributeAsText :: Otel.Attribute -> Maybe Text
attributeAsText (Otel.AttributeValue (Otel.TextAttribute a)) = Just a
attributeAsText _ = Nothing
