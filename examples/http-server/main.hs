{-# LANGUAGE OverloadedStrings #-}

import           Data.Text                                (Text)
import qualified Network.HTTP.Client                      as H
import qualified Network.HTTP.Types.Status                as H
import qualified Network.Wai                              as W
import qualified Network.Wai.Handler.Warp                 as W
import           OpenTelemetry.Instrumentation.HttpClient (httpClientInstrumentationConfig,
                                                           httpLbs)
import           OpenTelemetry.Instrumentation.Wai        (newOpenTelemetryWaiMiddleware)
import           OpenTelemetry.Logging.Core               (Log)
import           OpenTelemetry.Trace                      (TracerProviderOptions (tracerProviderOptionsLogger),
                                                           createTracerProvider,
                                                           getTracerProviderInitializationOptions,
                                                           setGlobalTracerProvider)

main :: IO ()
main = do
  httpClient <- H.newManager H.defaultManagerSettings
  do
    (processors, tracerProviderOptions) <- getTracerProviderInitializationOptions
    let
      tracerProviderOptions' = tracerProviderOptions { tracerProviderOptionsLogger = logger }
    tracerProvider <- createTracerProvider processors tracerProviderOptions'
    setGlobalTracerProvider tracerProvider
  tracerMiddleware <- newOpenTelemetryWaiMiddleware
  W.run 7777 $ tracerMiddleware $ app httpClient

app :: H.Manager -> W.Application
app httpManager req res =
  case W.pathInfo req of
    ["1"] -> do
      newReq <- H.parseRequest "http://localhost:7777/2"
      newRes <- httpLbs httpClientInstrumentationConfig newReq httpManager
      res $ W.responseLBS H.ok200 [] $ "1 (" <> H.responseBody newRes <> ")"
    ["2"] -> res $ W.responseLBS H.ok200 [] "2"
    _ -> res $ W.responseLBS H.ok200 [] "other"

logger :: Log Text -> IO ()
logger = putStrLn . ("log: " <>) . show
