{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module OpenTelemetry.Propagator.Datadog
  ( datadogTraceContextPropagator
  ) where

import           OpenTelemetry.Common           (TraceFlags (TraceFlags))
import           OpenTelemetry.Context          (Context, insertSpan,
                                                 lookupSpan)
import           OpenTelemetry.Propagator       (Propagator (Propagator, extractor, injector, propagatorNames))
import           OpenTelemetry.Trace            (SpanContext (SpanContext, isRemote, spanId, traceFlags, traceId, traceState))
import           OpenTelemetry.Trace.Core       (getSpanContext,
                                                 wrapSpanContext)
import           OpenTelemetry.Trace.Id         (SpanId (SpanId),
                                                 TraceId (TraceId))
import           OpenTelemetry.Trace.TraceState (TraceState (TraceState))
import qualified OpenTelemetry.Trace.TraceState as TS

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Short          as SB
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Data.Word                      (Word64)
import           Network.HTTP.Types             (RequestHeaders,
                                                 ResponseHeaders)
import           Text.Read                      (readMaybe)

-- メモ：Open Telemetry の Trace の ID と Datadog の APM の ID の相互変換に関する情報
-- - https://docs.datadoghq.com/ja/tracing/connect_logs_and_traces/opentelemetry/
datadogTraceContextPropagator :: (String -> IO ()) -> Propagator Context RequestHeaders ResponseHeaders
datadogTraceContextPropagator logger =
  Propagator
    { propagatorNames = ["datadog trace context"]
    , extractor = \hs c -> do
        logger $ "hs-opentelemetry-propagator-datadog: extractor: headers: " ++ show hs
        let
          spanContext' = do
            traceId <- TraceId . SB.toShort . fillLeadingZeros 16 . convertWord64ToBinaryByteString <$> (readMaybe . BC.unpack =<< lookup traceIdKey hs)
            parentId <- SpanId . SB.toShort . fillLeadingZeros  8 . convertWord64ToBinaryByteString <$> (readMaybe . BC.unpack =<< lookup parentIdKey hs)
            samplingPriority <- T.pack . BC.unpack <$> lookup samplingPriorityKey hs
            pure $
              SpanContext
                { traceId
                , spanId = parentId
                , isRemote = True
                , traceFlags = TraceFlags 1 -- 0 だとサンプルされない
                                            -- 参照：OpenTelemetry.Internal.Trace.Types.isSampled
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
              traceIdValue = (\(TraceId b) -> BC.pack $ show $ convertBinaryByteStringToWord64 $ SB.fromShort b) traceId
              parentIdValue = (\(SpanId b) -> BC.pack $ show $ convertBinaryByteStringToWord64 $ SB.fromShort b) spanId
            samplingPriority <-
              case lookup (TS.Key samplingPriorityKey) traceState of
                Nothing -> do
                  logger $ "sampling-priority not found at trace state: trace ID: " ++ show traceId ++ ", span ID: " ++ show spanId ++ ", headers: " ++ show hs
                  pure "0"
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

convertBinaryByteStringToWord64 :: ByteString -> Word64
convertBinaryByteStringToWord64 = B.foldl (\acc b -> (2 ^ (8 :: Int)) * acc + fromIntegral b) 0 -- GHC.Prim.indexWord8ArrayAsWord64# とか駆使すると早くなりそう

convertWord64ToBinaryByteString :: Word64 -> ByteString
convertWord64ToBinaryByteString =
  B.pack . toWord8s []
  where
    toWord8s acc 0 = acc
    toWord8s acc n =
      let (p, q) = n `divMod` (2 ^ (8 :: Int))
      in toWord8s (fromIntegral q : acc) p

fillLeadingZeros :: Word -> ByteString -> ByteString
fillLeadingZeros len bs = B.replicate (fromIntegral len - B.length bs) 0 <> bs
