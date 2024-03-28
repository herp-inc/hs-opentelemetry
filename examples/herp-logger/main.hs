{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Herp.Logger
import OpenTelemetry.Instrumentation.Herp.Logger.Datadog
import OpenTelemetry.Resource
import OpenTelemetry.Trace
import OpenTelemetry.Vendor.Datadog


main :: IO ()
main = do
  let
    resource :: Resource 'Nothing
    resource =
      mkResource
        [ envKey .=$ "test"
        , serviceKey .=$ "hs-opentelemetry"
        , versionKey .=$ "0"
        ]
  bracket
    ( do
        (processors, options) <- getTracerProviderInitializationOptions' resource
        createTracerProvider processors options
    )
    shutdownTracerProvider
    $ \tracerProvider ->
      let loggerConfig = appendHooksToConfig tracerProvider defaultLoggerConfig
       in do
            withLogger loggerConfig $ \logger -> do
              let tracer = makeTracer tracerProvider "main" tracerOptions
              inSpan' tracer "main" defaultSpanArguments $ \span -> do
                addAttributeByKey span envKey "test"
                logIO logger "log"
