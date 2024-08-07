{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Echo
import Network.GRPC.HighLevel.Generated (
  GRPCMethodType (..),
  Host (..),
  Port (..),
  ServerRequest (..),
  ServerResponse (..),
  StatusCode (..),
  defaultServiceOptions,
  serverHost,
  serverPort,
 )
import qualified OpenTelemetry.Instrumentation.GRPC as Otel
import qualified OpenTelemetry.Trace as Otel
import Options.Generic


data Args = Args
  { bind :: Maybe ByteString <?> "grpc endpoint hostname (default \"localhost\")"
  , port :: Maybe Int <?> "grpc endpoint port (default 50051)"
  }
  deriving (Generic, Show)


instance ParseRecord Args


doEcho
  :: ServerRequest 'Normal EchoRequest EchoResponse
  -> IO (ServerResponse 'Normal EchoResponse)
doEcho (ServerNormalRequest _meta (EchoRequest pay)) = do
  return (ServerNormalResponse (EchoResponse pay) mempty StatusOk "")


main :: IO ()
main = do
  Args {..} <- getRecord "Runs the echo service"
  let opts =
        defaultServiceOptions
          { serverHost = Host . fromMaybe "localhost" . unHelpful $ bind
          , serverPort = Port . fromMaybe 50051 . unHelpful $ port
          }
  tracerProvider <- createTracerProvider
  let service = Otel.propagatableTraceableServer tracerProvider Echo {echoDoEcho = doEcho}
  echoServer service opts


createTracerProvider :: IO Otel.TracerProvider
createTracerProvider = do
  (processors, tracerProviderOptions) <- Otel.getTracerProviderInitializationOptions
  Otel.createTracerProvider processors tracerProviderOptions


instance Otel.Traceable (Echo ServerRequest ServerResponse)


instance Otel.Propagatable (Echo ServerRequest ServerResponse)
