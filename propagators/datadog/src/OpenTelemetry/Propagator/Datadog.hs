{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module OpenTelemetry.Propagator.Datadog
  ( datadogTraceContextPropagator
  , convertOpenTelemetrySpanIdToDatadogSpanId
  , convertOpenTelemetryTraceIdToDatadogTraceId
  ) where

import           OpenTelemetry.Common                      (TraceFlags (TraceFlags))
import           OpenTelemetry.Context                     (Context, insertSpan, lookupSpan)
import           OpenTelemetry.Propagator                  (Propagator (Propagator, extractor, injector, propagatorNames))
import           OpenTelemetry.Propagator.Datadog.Internal (newHeaderFromSpanId, newHeaderFromTraceId,
                                                            newSpanIdFromHeader, newTraceIdFromHeader)
import           OpenTelemetry.Trace                       (SpanContext (SpanContext, isRemote, spanId, traceFlags, traceId, traceState))
import           OpenTelemetry.Trace.Core                  (getSpanContext, wrapSpanContext)
import           OpenTelemetry.Trace.Id                    (SpanId (SpanId), TraceId (TraceId))
import           OpenTelemetry.Trace.TraceState            (TraceState (TraceState))
import qualified OpenTelemetry.Trace.TraceState            as TS

import qualified Data.ByteString.Char8 as BC
import           Data.String           (IsString)
import qualified Data.Text             as T
import           Network.HTTP.Types    (RequestHeaders, ResponseHeaders)

-- Reference: bi-directional conversion of IDs of Open Telemetry and ones of Datadog
-- - https://docs.datadoghq.com/ja/tracing/connect_logs_and_traces/opentelemetry/
datadogTraceContextPropagator :: Propagator Context RequestHeaders ResponseHeaders
datadogTraceContextPropagator =
  Propagator
    { propagatorNames = ["datadog trace context"]
    , extractor = \hs c -> do
        let
          spanContext' = do
            traceId <- TraceId . newTraceIdFromHeader <$> lookup traceIdKey hs
            parentId <- SpanId . newSpanIdFromHeader <$> lookup parentIdKey hs
            samplingPriority <- T.pack . BC.unpack <$> lookup samplingPriorityKey hs
            pure $
              SpanContext
                { traceId
                , spanId = parentId
                , isRemote = True
                , traceFlags = TraceFlags 1 -- when 0, not sampled
                                            -- refer: OpenTelemetry.Internal.Trace.Types.isSampled
                , traceState = TraceState [(TS.Key samplingPriorityKey, TS.Value samplingPriority)]
                }
        case spanContext' of
          Nothing          -> pure c
          Just spanContext -> pure $ insertSpan (wrapSpanContext spanContext) c
    , injector = \c hs ->
        case lookupSpan c of
          Nothing -> pure hs
          Just span' -> do
            SpanContext { traceId, spanId, traceState = TraceState traceState } <- getSpanContext span'
            let
              traceIdValue = (\(TraceId b) -> newHeaderFromTraceId b) traceId
              parentIdValue = (\(SpanId b) -> newHeaderFromSpanId b) spanId
            samplingPriority <-
              case lookup (TS.Key samplingPriorityKey) traceState of
                Nothing -> pure "1" -- when an origin of the trace
                Just (TS.Value p) -> pure $ BC.pack $ T.unpack p
            pure
              $ (traceIdKey, traceIdValue)
              : (parentIdKey, parentIdValue)
              : (samplingPriorityKey, samplingPriority)
              : hs
    }
  where
    traceIdKey, parentIdKey, samplingPriorityKey :: IsString s => s
    traceIdKey = "x-datadog-trace-id"
    parentIdKey = "x-datadog-parent-id"
    samplingPriorityKey = "x-datadog-sampling-priority"
