{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Amazonka
import qualified Amazonka.Data.Path
import qualified Amazonka.Env
import qualified Amazonka.S3
import qualified Amazonka.S3.GetObject
import Conduit (
  runResourceT,
  sinkList,
 )
import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import GHC.Stack (HasCallStack)
import OpenTelemetry.Instrumentation.Amazonka
import OpenTelemetry.Trace (
  defaultSpanArguments,
  inSpan,
  initializeTracerProvider,
  makeTracer,
  shutdownTracerProvider,
  tracerOptions,
 )
import System.IO (hFlush, stdout)


main :: HasCallStack => IO ()
main =
  bracket
    initializeTracerProvider
    shutdownTracerProvider
    $ \tracerProvider -> runResourceT $ do
      let tracer = makeTracer tracerProvider "aws-s3-example" tracerOptions
      bytes <- inSpan tracer "main" defaultSpanArguments $ do
        awsEnv@Amazonka.Env {Amazonka.Env.overrides} <- Amazonka.newEnv Amazonka.discover
        awsEnv' <- appendHooksToEnv tracerProvider awsEnv
        let awsEnv'' = awsEnv' {Amazonka.Env.overrides = overridesServiceForLocalstack . overrides}
        Amazonka.S3.GetObject.GetObjectResponse' {Amazonka.S3.GetObject.body = responseBody} <-
          Amazonka.send
            awsEnv''
            $ Amazonka.S3.newGetObject (Amazonka.S3.BucketName "aws-s3-example")
            $ Amazonka.S3.ObjectKey "README.md"
        [bytes] <- responseBody `Amazonka.sinkBody` sinkList
        pure bytes
      liftIO $ do
        putStrLn "----"
        putStrLn $ BS.unpack bytes
        putStrLn "----"
        putStr "Press enter to exit after while..."
        hFlush stdout
        void $ getLine -- wait for transporting spans


overridesServiceForLocalstack :: Amazonka.Service -> Amazonka.Service
overridesServiceForLocalstack s =
  s
    { Amazonka.s3AddressingStyle = Amazonka.S3AddressingStylePath
    , Amazonka.endpoint =
        const $
          Amazonka.Endpoint
            { Amazonka.host = "localhost"
            , Amazonka.basePath = Amazonka.Data.Path.Raw []
            , Amazonka.secure = False
            , Amazonka.port = 4566
            , Amazonka.scope = ""
            }
    }
