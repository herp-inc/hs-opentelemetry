{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OpenTelemetry.Vendor.Datadog (
  -- * Trace

  --

  -- | These functions are conversions of the hs-opentelemetry internal representation of the trace ID and
  -- the span ID and the Datadog header representation of them each other.
  --
  -- @
  -- +----------+-----------------+----------------+
  -- |          | Trace ID        | Span ID        |
  -- +----------+-----------------+----------------+
  -- | Internal | 128-bit integer | 64-bit integer |
  -- +----------+-----------------+----------------+
  -- | Datadog  | ASCII text of   | ASCII text of  |
  -- | Header   | 64-bit integer  | 64-bit integer |
  -- +----------+-----------------+----------------+
  -- @
  --
  -- Reference: bi-directional conversion of IDs of Open Telemetry and ones of Datadog
  --
  -- - English: https://docs.datadoghq.com/tracing/other_telemetry/connect_logs_and_traces/opentelemetry/
  -- - Japanese: https://docs.datadoghq.com/ja/tracing/connect_logs_and_traces/opentelemetry/
  convertOpenTelemetrySpanIdToDatadogSpanId,
  convertOpenTelemetryTraceIdToDatadogTraceId,

  -- * Attribute

  --

  -- | These are keys to lookup or insert 'OpenTelemetry.Attributes.Attribute's to 'OpenTelemetry.Attributes' with.
  envKey,
  serviceKey,
  versionKey,

  -- * Resource
  detectResource,
) where

import qualified Data.ByteString.Short.Internal as SBI
import Data.Primitive (ByteArray (ByteArray))
import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word64)
import qualified OpenTelemetry.Attributes as Attribute
import qualified OpenTelemetry.Internal.Trace.Id as Trace
import OpenTelemetry.Resource (Resource, mkResource)
import OpenTelemetry.Vendor.Datadog.Internal (indexByteArrayNbo)
import System.Environment (lookupEnv)


convertOpenTelemetrySpanIdToDatadogSpanId :: Trace.SpanId -> Word64
convertOpenTelemetrySpanIdToDatadogSpanId (Trace.SpanId (SBI.SBS a)) = indexByteArrayNbo (ByteArray a) 0


convertOpenTelemetryTraceIdToDatadogTraceId :: Trace.TraceId -> Word64
convertOpenTelemetryTraceIdToDatadogTraceId (Trace.TraceId (SBI.SBS a)) = indexByteArrayNbo (ByteArray a) 1


envKey :: Attribute.Key Text
envKey = "dd.env"


serviceKey :: Attribute.Key Text
serviceKey = "dd.service"


versionKey :: Attribute.Key Text
versionKey = "dd.version"


{- | Detect these environment variables and create a 'Resource' from them.

- DD_ENV
- DD_SERVICE
- DD_VERSION
-}
detectResource :: IO (Resource r)
detectResource = do
  env <- (envKey,) <$> lookupEnv "DD_ENV"
  service <- (serviceKey,) <$> lookupEnv "DD_SERVICE"
  version <- (versionKey,) <$> lookupEnv "DD_VERSION"
  pure $ mkResource $ (\(Attribute.Key k, mv) -> (k,) . fromString <$> mv) <$> [env, service, version]
