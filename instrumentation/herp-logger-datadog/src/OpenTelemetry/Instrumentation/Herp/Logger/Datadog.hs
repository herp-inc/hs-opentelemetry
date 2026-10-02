{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | This is herp-logger with connections of OpenTelemetry Traces and Logs.

Datadog functionality about connections of Traces and Logs is described in
<https://docs.datadoghq.com/tracing/other_telemetry/connect_logs_and_traces/opentelemetry/ Connect OpenTelemetry Traces and Logs>.

This logger requires 'Otel.Tracer' to retrieve OpenTelemetry context additionally.
-}
module OpenTelemetry.Instrumentation.Herp.Logger.Datadog (
  appendHooksToConfig,
  datadogHooks,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Herp.Logger as Orig
import qualified Herp.Logger.Payload as Orig
import qualified OpenTelemetry.Attributes as Otel
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.Context as Otel
import qualified OpenTelemetry.Context.ThreadLocal as Otel
import qualified OpenTelemetry.Resource as Otel
import qualified OpenTelemetry.SemanticConventions as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import qualified OpenTelemetry.Vendor.Datadog as Datadog
import Paths_hs_opentelemetry_instrumentation_herp_logger_datadog (version)


appendHooksToConfig :: Otel.TracerProvider -> Orig.LoggerConfig -> Orig.LoggerConfig
appendHooksToConfig provider config@Orig.LoggerConfig {Orig.hooks} = config {Orig.hooks = hooks {Orig.logHook = logHook provider . Orig.logHook hooks}}


datadogHooks :: Otel.TracerProvider -> Orig.Hooks
datadogHooks provider = Orig.Hooks $ logHook provider


logHook :: Otel.TracerProvider -> (Orig.Logger -> Orig.Payload -> IO ()) -> Orig.Logger -> Orig.Payload -> IO ()
logHook provider hook logger payload = do
  let
    tracer =
      Otel.makeTracer
        provider
        (Otel.InstrumentationLibrary "hs-opentelemetry-instrumentation-herp-logger-datadog" $ Text.pack $ showVersion version)
        Otel.tracerOptions
  context <- Otel.getContext
  payload' <- datadogPayload (Otel.getTracerTracerProvider tracer) $ Otel.lookupSpan context
  hook logger $ payload' <> payload


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
