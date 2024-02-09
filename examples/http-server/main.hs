{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types.Status as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import OpenTelemetry.Instrumentation.HttpClient (appendModifierToSettings)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Logging.Core (Log)
import OpenTelemetry.Propagator.Datadog (datadogTraceContextPropagator)
import OpenTelemetry.Trace (
  TracerProviderOptions (tracerProviderOptionsLogger, tracerProviderOptionsPropagators),
  createTracerProvider,
  getTracerProviderInitializationOptions,
  setGlobalTracerProvider,
 )
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  (processors, tracerProviderOptions) <- getTracerProviderInitializationOptions
  let
    tracerProviderOptions' = tracerProviderOptions {tracerProviderOptionsLogger = logger}
    tracerProviderOptions'' =
      case args of
        ["datadog"] -> tracerProviderOptions' {tracerProviderOptionsPropagators = datadogTraceContextPropagator}
        _ -> tracerProviderOptions'
  tracerProvider <- createTracerProvider processors tracerProviderOptions''
  setGlobalTracerProvider tracerProvider
  httpManagerSettings <- appendModifierToSettings tracerProvider H.defaultManagerSettings
  httpClient <- H.newManager httpManagerSettings
  let tracerMiddleware = newOpenTelemetryWaiMiddleware' tracerProvider
  W.run 7777 $ tracerMiddleware $ app httpClient


app :: H.Manager -> W.Application
app httpManager req res =
  case W.pathInfo req of
    ["1"] -> do
      newReq <- H.parseRequest "http://localhost:7777/2"
      newRes <- H.httpLbs newReq httpManager
      res $ W.responseLBS H.ok200 [] $ "1 (" <> H.responseBody newRes <> ")"
    ["2"] -> res $ W.responseLBS H.ok200 [] "2"
    _ -> res $ W.responseLBS H.ok200 [] "other"


logger :: Log Text -> IO ()
logger = putStrLn . ("log: " <>) . show
