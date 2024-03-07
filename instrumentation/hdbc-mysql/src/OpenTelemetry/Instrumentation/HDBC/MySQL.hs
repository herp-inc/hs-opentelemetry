{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.HDBC.MySQL (
  Orig.MySQLConnectInfo (..),
  Otel.Connection,
  connectMySQL,
  Orig.defaultMySQLConnectInfo,
  Orig.withRTSSignalsBlocked,
) where

import qualified Data.Text as Text
import qualified Database.HDBC.MySQL as Orig
import qualified OpenTelemetry.Attributes.Map as Attr
import qualified OpenTelemetry.Instrumentation.HDBC as Otel
import qualified OpenTelemetry.SemanticConventions as Attr
import qualified OpenTelemetry.Trace.Core as Otel


connectMySQL :: Otel.TracerProvider -> Attr.AttributeMap -> Orig.MySQLConnectInfo -> IO Otel.Connection
connectMySQL
  tracerProvider
  extraAttributes
  connectInfo@Orig.MySQLConnectInfo
    { Orig.mysqlHost
    , Orig.mysqlUser
    , Orig.mysqlDatabase
    , Orig.mysqlPort
    , Orig.mysqlUnixSocket
    } = do
    connection <- Orig.connectMySQL connectInfo
    let port :: Maybe Word
        transport :: Text.Text
        (port, transport) =
          if null mysqlUnixSocket
            then (Just $ fromIntegral mysqlPort, "tcp")
            else (Nothing, "unix")
        attributes =
          Otel.Attributes
            { Otel.db_connectionString = Nothing
            , Otel.db_system = "mysql"
            , Otel.db_user = Just $ Text.pack mysqlUser
            , Otel.network_peer_address = Just $ Text.pack mysqlHost
            , Otel.network_peer_port = port
            , Otel.network_transport = Just transport
            , Otel.network_type = Nothing
            , Otel.server_address = Just $ Text.pack mysqlHost
            , Otel.server_port = port
            }
        extraAttributes' = Attr.insertByKey Attr.db_name (Text.pack mysqlDatabase) extraAttributes
    pure $ Otel.makeConnection connection tracerProvider attributes extraAttributes'
