{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (bracket)
import Control.Monad (void)
import Database.HDBC (commit, disconnect, run)
import OpenTelemetry.Instrumentation.HDBC.MySQL (connectMySQL, defaultMySQLConnectInfo)
import OpenTelemetry.Trace (
  defaultSpanArguments,
  inSpan,
  initializeTracerProvider,
  makeTracer,
  shutdownTracerProvider,
  tracerOptions,
 )
import System.IO (hFlush, stdout)


main :: IO ()
main = do
  bracket
    initializeTracerProvider
    shutdownTracerProvider
    $ \tracerProvider -> do
      bracket
        (connectMySQL tracerProvider mempty defaultMySQLConnectInfo)
        disconnect
        $ \connection -> do
          let tracer = makeTracer tracerProvider "hdbc-mysql-example" tracerOptions
          inSpan tracer "create" defaultSpanArguments $ do
            void $ run connection "CREATE TABLE test (id INTEGER PRIMARY KEY)" []
            commit connection
          inSpan tracer "drop" defaultSpanArguments $ do
            void $ run connection "DROP TABLE test" []
            commit connection
          putStr "Press enter to exit after while..."
          hFlush stdout
          void $ getLine -- wait for transporting spans
