{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Echo
import Network.GRPC.HighLevel.Client
import Network.GRPC.LowLevel hiding (payload, port)
import Network.GRPC.LowLevel.Call hiding (payload)
import qualified OpenTelemetry.Instrumentation.GRPC as Otel
import qualified OpenTelemetry.Trace as Otel
import Options.Generic
import Prelude hiding (FilePath)


data Args = Args
  { bind :: Maybe ByteString <?> "grpc endpoint hostname (default \"localhost\")"
  , port :: Maybe Int <?> "grpc endpoint port (default 50051)"
  , payload :: Maybe TL.Text <?> "string to echo (default \"hullo!\")"
  }
  deriving (Generic, Show)


instance ParseRecord Args


main :: IO ()
main = do
  Args {bind, payload, port} <- getRecord "Runs the echo client"
  let
    pay = fromMaybe "hullo!" . unHelpful $ payload
    rqt = EchoRequest pay
    expected = EchoResponse pay
    cfg =
      ClientConfig
        (endpoint (Host . fromMaybe "localhost" . unHelpful $ bind) (Port . fromMaybe 50051 . unHelpful $ port))
        []
        Nothing
        Nothing
  tracerProvider <- createTracerProvider
  withGRPC $ \g -> withClient g cfg $ \c -> do
    Echo {echoDoEcho} <- Otel.propagatableTraceableClient tracerProvider <$> echoClient c
    echoDoEcho (ClientNormalRequest rqt 5 mempty) >>= \case
      ClientNormalResponse rsp _ _ StatusOk _
        | rsp == expected -> return ()
        | otherwise -> fail $ "Got unexpected response: '" ++ show rsp ++ "', expected: '" ++ show expected ++ "'"
      ClientNormalResponse _ _ _ st _ -> fail $ "Got unexpected status " ++ show st ++ " from call, expecting StatusOk"
      ClientErrorResponse e -> fail $ "Got client error: " ++ show e
  putStrLn $ "echo-client success: sent " ++ show pay ++ ", got " ++ show pay
  _ <- getLine -- wait for Otel send
  pure ()


createTracerProvider :: IO Otel.TracerProvider
createTracerProvider = do
  (processors, tracerProviderOptions) <- Otel.getTracerProviderInitializationOptions
  Otel.createTracerProvider processors tracerProviderOptions


instance Otel.Traceable (Echo ClientRequest ClientResult)


instance Otel.Propagatable (Echo ClientRequest ClientResult)
