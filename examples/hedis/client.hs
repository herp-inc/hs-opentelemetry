{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.Redis (connect, defaultConnectInfo, get, runRedis, set)
import OpenTelemetry.Instrumentation.Hedis (appendHooksToConnectionInfo)
import OpenTelemetry.Trace (defaultSpanArguments, inSpan, initializeTracerProvider, makeTracer, shutdownTracerProvider, tracerOptions)
import System.IO (hFlush, stdout)


main :: IO ()
main =
  bracket
    initializeTracerProvider
    shutdownTracerProvider
    $ \tracerProvider -> do
      let tracer = makeTracer tracerProvider "main" tracerOptions
      inSpan tracer "main" defaultSpanArguments $ do
        connInfo <- appendHooksToConnectionInfo tracerProvider defaultConnectInfo
        connection <- connect connInfo
        runRedis connection $ do
          void $ set "hello" "hello"
          void $ set "world" "world"
          hello <- get "hello"
          world <- get "world"
          liftIO $ print (hello, world)
      putStr "Press enter to exit after while..."
      hFlush stdout
      void $ getLine -- wait for transporting spans
