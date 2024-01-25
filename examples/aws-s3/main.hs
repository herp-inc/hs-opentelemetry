{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Amazonka
import qualified Amazonka.Data.Path
import qualified Amazonka.Env
import qualified Amazonka.Env.Hooks
import qualified Amazonka.S3
import qualified Amazonka.S3.GetObject
import Data.Function ((&))
import Conduit (
  runResourceT,
  sinkList,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (Identity))
import OpenTelemetry.Instrumentation.Amazonka
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket)
import System.IO (hFlush, stdout)
import OpenTelemetry.Trace (
  initializeTracerProvider,
  shutdownTracerProvider,
 )
import Control.Monad (void)

main :: IO ()
main =
  bracket
    initializeTracerProvider
    shutdownTracerProvider
    $ \tracerProvider -> runResourceT $ do
        (ClientRequestHookUpdate clientRequestHook, ClientResponseHookUpdate clientResponseHook) <- liftIO $ createHooks tracerProvider
        awsEnv <-
          Amazonka.newEnv
            ( \e@Amazonka.Env {Amazonka.Env.hooks, Amazonka.Env.overrides} ->
                pure $
                  e
                    { Amazonka.Env.hooks =
                        hooks
                          & Amazonka.Env.Hooks.clientRequestHook clientRequestHook
                          & Amazonka.Env.Hooks.clientResponseHook clientResponseHook,
                      Amazonka.Env.overrides =
                        ( \s ->
                            s
                              { Amazonka.s3AddressingStyle = Amazonka.S3AddressingStylePath,
                                Amazonka.endpoint =
                                  const $
                                    Amazonka.Endpoint
                                      { Amazonka.host = "localhost"
                                      , Amazonka.basePath = Amazonka.Data.Path.Raw []
                                      , Amazonka.secure = False
                                      , Amazonka.port = 4566
                                      , Amazonka.scope = ""
                                      }
                              }
                        )
                          . overrides
                    , Amazonka.auth =
                        Identity $
                          Amazonka.Auth $
                            Amazonka.AuthEnv
                              { Amazonka.accessKeyId = "test"
                              , Amazonka.secretAccessKey = "test"
                              , Amazonka.sessionToken = Nothing
                              , Amazonka.expiration = Nothing
                              }
                    }
            )
        Amazonka.S3.GetObject.GetObjectResponse' {Amazonka.S3.GetObject.body = responseBody} <-
          Amazonka.send
            awsEnv
            $ Amazonka.S3.newGetObject (Amazonka.S3.BucketName "aws-s3-example")
            $ Amazonka.S3.ObjectKey "README.md"
        [bytes] <- responseBody `Amazonka.sinkBody` sinkList
        liftIO $ do
          putStrLn "----"
          putStrLn $ BS.unpack bytes
          putStrLn "----"
          putStr "Press enter to exit after while..."
          hFlush stdout
          void $ getLine -- wait for transporting spans
