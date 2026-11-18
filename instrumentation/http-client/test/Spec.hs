{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (replicateM_, unless, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as L
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats, getRTSStatsEnabled)
import qualified Network.HTTP.Client as HTTP
import Network.Socket (
  Family (AF_INET),
  SockAddr (SockAddrInet),
  Socket,
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  getSocketName,
  listen,
  setSocketOption,
  socket,
  tupleToHostAddress,
 )
import qualified Network.Socket.ByteString as SB
import OpenTelemetry.Instrumentation.HttpClient (appendTraceHeadersToSettings, withHttpSpan)
import OpenTelemetry.Processor (Processor (..), ShutdownResult (..))
import OpenTelemetry.Trace.Core (TracerProvider, createTracerProvider, emptyTracerProviderOptions)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)


main :: IO ()
main = do
  statsEnabled <- getRTSStatsEnabled
  unless statsEnabled $ die "RTS statistics are off. This test needs +RTS -T."
  endedRef <- newIORef (0 :: Int)
  port <- startServer
  closedPort <- unusedPort
  provider <- createTracerProvider [countingProcessor endedRef] emptyTracerProviderOptions
  settings <- appendTraceHeadersToSettings provider HTTP.defaultManagerSettings
  let
    settings' = settings {HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 2_000_000}
  manager <- HTTP.newManager settings'
  -- Keep the Manager alive until after the sample. Dropping it would also
  -- drop a map that still pointed at finished threads.
  managerRef <- newIORef manager
  let
    okRequest = request port
    failRequest = request closedPort
  replicateM_ 20 $ voidResult $ call provider okRequest manager
  endedBefore <- readIORef endedRef
  baseline <- liveBytes managerRef
  runCalls 2000 $ voidResult $ call provider okRequest manager
  runCalls 2000 $ do
    result <- try @SomeException $ call provider failRequest manager
    case result of
      Left _ -> pure ()
      Right _ -> die "expected the call against a closed port to throw"
  endedAfter <- readIORef endedRef
  after <- liveBytes managerRef
  let
    growth = after - baseline
    ended = endedAfter - endedBefore
  printf "live_bytes baseline=%d after=%d growth=%d ended_spans=%d\n" baseline after growth ended
  when (ended /= 4000) $
    die $
      "expected 4000 ended spans, got " <> show ended
  when (growth > maxGrowth) $
    die $
      "live heap grew by "
        <> show growth
        <> " bytes after 2000 calls on fresh threads (baseline "
        <> show baseline
        <> ", after "
        <> show after
        <> "). The instrumentation must not retain ThreadIds."
  _ <- readIORef managerRef
  pure ()


-- 2000 threads times the default 32KB stack chunk is about 64MB.
-- A few megabytes covers the connection pool and allocator noise.
maxGrowth :: Int
maxGrowth = 8 * 1024 * 1024


countingProcessor :: IORef Int -> Processor
countingProcessor endedRef =
  Processor
    { processorOnStart = \_ _ -> pure ()
    , processorOnEnd = \_ -> modifyIORef' endedRef (+ 1)
    , processorShutdown = async (pure ShutdownSuccess)
    , processorForceFlush = pure ()
    }


call :: TracerProvider -> HTTP.Request -> HTTP.Manager -> IO (HTTP.Response L.ByteString)
call provider req manager = withHttpSpan provider req $ HTTP.httpLbs req manager


request :: Int -> HTTP.Request
request port =
  HTTP.defaultRequest
    { HTTP.host = "127.0.0.1"
    , HTTP.method = "GET"
    , HTTP.path = "/"
    , HTTP.port = port
    }


runCalls :: Int -> IO () -> IO ()
runCalls n action = go 1
  where
    go i
      | i > n = pure ()
      | otherwise = do
          done <- newEmptyMVar
          _ <- forkFinally action (putMVar done)
          result <- takeMVar done
          case result of
            Left err -> throwIO err
            Right _ -> go (i + 1)


liveBytes :: IORef HTTP.Manager -> IO Int
liveBytes managerRef = do
  performGC
  _ <- readIORef managerRef
  fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats


voidResult :: IO a -> IO ()
voidResult action = do
  _ <- action
  pure ()


startServer :: IO Int
startServer = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 256
  SockAddrInet port _ <- getSocketName sock
  _ <- forkFinally (acceptLoop sock) (\_ -> close sock)
  pure (fromIntegral port)


unusedPort :: IO Int
unusedPort = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  SockAddrInet port _ <- getSocketName sock
  close sock
  pure (fromIntegral port)


acceptLoop :: Socket -> IO ()
acceptLoop sock = do
  (conn, _) <- accept sock
  _ <- forkFinally (talk conn BS.empty) (\_ -> close conn)
  acceptLoop sock


responseBytes :: BS.ByteString
responseBytes =
  "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: keep-alive\r\n\r\nok"


talk :: Socket -> BS.ByteString -> IO ()
talk conn buf = do
  filled <- fillUntil conn buf headerEnd
  case headerEnd filled of
    Nothing -> pure ()
    Just headerLen -> do
      SB.sendAll conn responseBytes
      talk conn (BS.drop headerLen filled)


fillUntil :: Socket -> BS.ByteString -> (BS.ByteString -> Maybe a) -> IO BS.ByteString
fillUntil sock buf ready = go buf
  where
    go acc
      | isJust (ready acc) = pure acc
      | otherwise = do
          chunk <- SB.recv sock 65536
          if BS.null chunk then pure acc else go (acc <> chunk)


headerEnd :: BS.ByteString -> Maybe Int
headerEnd bs = case BS.breakSubstring "\r\n\r\n" bs of
  (_, "") -> Nothing
  (headers, _) -> Just (BS.length headers + 4)
