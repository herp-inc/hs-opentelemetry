{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import OpenTelemetry.Instrumentation.Hedis
import OpenTelemetry.Logging.Core (Log)
import OpenTelemetry.Trace (TracerProviderOptions (tracerProviderOptionsLogger), createTracerProvider, getTracerProviderInitializationOptions)
import System.IO (hFlush, stdout)


main :: IO ()
main = do
  tracerProvider <- do
    (processors, tracerProviderOptions) <- getTracerProviderInitializationOptions
    let
      tracerProviderOptions' = tracerProviderOptions {tracerProviderOptionsLogger = logger}
    createTracerProvider processors tracerProviderOptions'
  conn <- checkedConnect' tracerProvider defaultConnectInfo
  runRedis' tracerProvider conn $ do
    void $ set "hello" "hello"
    void $ set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello, world)
  disconnect' tracerProvider conn
  -- wait for uploading traces
  putStr "press enter key to return"
  hFlush stdout
  void getLine


logger :: Log Text -> IO ()
logger = putStrLn . ("log: " <>) . show
