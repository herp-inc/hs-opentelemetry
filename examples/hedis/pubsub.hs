{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Database.Redis
import OpenTelemetry.Instrumentation.Hedis (appendHooksToConnectionInfo)
import qualified OpenTelemetry.Trace as Otel
import System.IO (hFlush, stdout)


main :: IO ()
main =
  bracket
    Otel.initializeTracerProvider
    Otel.shutdownTracerProvider
    $ \tracerProvider -> do
      let tracer = Otel.makeTracer tracerProvider "hedis-pubsub" Otel.tracerOptions
      connInfo <- appendHooksToConnectionInfo tracerProvider defaultConnectInfo
      connection <- connect connInfo
      void $ forkIO $ Otel.inSpan tracer "single-thread" Otel.defaultSpanArguments $ do
        runRedis connection $
          pubSub (subscribe ["hello"]) $ \message -> do
            Otel.inSpan tracer "single-thread callback" Otel.defaultSpanArguments $ do
              putStrLn $ "single-thread: received: " ++ show message
              pure mempty
      void $ forkIO $ Otel.inSpan tracer "multithread" Otel.defaultSpanArguments $ do
        controller <- newPubSubController [("hello", helloMessageCallback tracer)] []
        pubSubForever connection controller $ do
          putStrLn "subscribed acknowledged"
      liftIO $ do
        putStrLn "Press enter to publish a message..."
        hFlush stdout
        void $ getLine -- wait for subscribing
      Otel.inSpan tracer "publish" Otel.defaultSpanArguments $ do
        runRedis connection $ do
          void $ publish "hello" "world"
      putStrLn "Press enter to exit after while..."
      hFlush stdout
      void $ getLine -- wait for transporting spans


helloMessageCallback :: Otel.Tracer -> MessageCallback
helloMessageCallback tracer message = do
  Otel.inSpan tracer "multithread callback" Otel.defaultSpanArguments $ do
    putStrLn $ "multithread: receive: " ++ show message
