{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module OpenTelemetry.Instrumentation.HDBC (
  Connection (..),
  Attributes (..),
  makeConnection,
  module HDBC,
) where

import Data.Default.Class (def)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.HDBC (
  ConnWrapper (ConnWrapper),
  IConnection (
    clone,
    commit,
    dbServerVer,
    dbTransactionSupport,
    describeTable,
    disconnect,
    getTables,
    hdbcClientVer,
    hdbcDriverName,
    prepare,
    proxiedClientName,
    proxiedClientVer,
    rollback,
    run
  ),
 )
import qualified Database.HDBC as HDBC
import GHC.Generics (Generic)
import GHC.Stack (withFrozenCallStack)
import qualified OpenTelemetry.Attributes as Attr
import qualified OpenTelemetry.Attributes.Map as Attr
import qualified OpenTelemetry.SemanticConventions as Attr
import qualified OpenTelemetry.Trace.Core as Otel


type Connection :: Type
data Connection = Connection
  { original :: ConnWrapper
  , tracer :: Otel.Tracer
  , attributes :: Attr.AttributeMap
  }


-- | Specification: https://opentelemetry.io/docs/specs/semconv/database/database-spans/
type Attributes :: Type
data Attributes = Attributes
  { db_connectionString :: Maybe Text
  , db_system :: Text
  , db_user :: Maybe Text
  , network_peer_address :: Maybe Text
  , network_peer_port :: Maybe Word
  , network_transport :: Maybe Text
  , network_type :: Maybe Text
  , server_address :: Maybe Text
  , server_port :: Maybe Word
  }
  deriving stock (Show, Read, Eq, Ord, Generic)


makeConnection ::
  IConnection connection =>
  -- | Original connection
  connection ->
  Otel.TracerProvider ->
  Attributes ->
  -- | Extra attributes
  Attr.AttributeMap ->
  Connection
makeConnection original provider attributes extraAttributes =
  Connection
    { original = ConnWrapper original
    , tracer = Otel.makeTracer provider "hs-opentelemetry-instrumentation-hdbc" Otel.tracerOptions
    , attributes = Attr.union (convertAttributes attributes) extraAttributes
    }


convertAttributes :: Attributes -> Attr.AttributeMap
convertAttributes
  Attributes
    { db_connectionString
    , db_system
    , db_user
    , network_peer_address
    , network_peer_port
    , network_transport
    , network_type
    , server_address
    , server_port
    } =
    Attr.empty
      & insert Attr.db_connectionString db_connectionString
      & insert Attr.db_system (Just db_system)
      & insert Attr.db_user db_user
      & insert Attr.network_peer_address network_peer_address
      & insert Attr.network_peer_port (fromIntegral <$> network_peer_port)
      & insert Attr.network_transport network_transport
      & insert Attr.network_type network_type
      & insert Attr.server_address server_address
      & insert Attr.server_port (fromIntegral <$> server_port)
    where
      insert ::
        Attr.ToAttribute a =>
        Attr.Key a ->
        Maybe a ->
        Attr.AttributeMap ->
        Attr.AttributeMap
      insert _ Nothing = id
      insert key (Just value) = Attr.insertByKey key value


instance IConnection Connection where
  disconnect Connection {original, tracer, attributes} =
    withFrozenCallStack $
      Otel.inSpan tracer "disconnect" (defaultSpanArguments attributes) $
        disconnect original


  commit Connection {original, tracer, attributes} =
    withFrozenCallStack $
      Otel.inSpan tracer "commit" (defaultSpanArguments attributes) $
        commit original


  rollback Connection {original, tracer, attributes} =
    withFrozenCallStack $
      Otel.inSpan tracer "rollback" (defaultSpanArguments attributes) $
        rollback original


  run Connection {original, tracer, attributes} query params =
    withFrozenCallStack $ do
      let attributes' = Attr.insertByKey Attr.db_statement (Text.pack query) attributes
      Otel.inSpan tracer "run" (defaultSpanArguments attributes') $
        run original query params


  prepare Connection {original, tracer, attributes} query =
    withFrozenCallStack $ do
      let attributes' = Attr.insertByKey Attr.db_statement (Text.pack query) attributes
      Otel.inSpan tracer "prepare" (defaultSpanArguments attributes') $
        prepare original query


  clone Connection {original, tracer, attributes} =
    withFrozenCallStack $
      Otel.inSpan tracer "clone" (defaultSpanArguments attributes) $ do
        conn <- clone original
        pure $ Connection {original = conn, tracer, attributes}


  hdbcDriverName Connection {original} = withFrozenCallStack $ hdbcDriverName original


  hdbcClientVer Connection {original} = withFrozenCallStack $ hdbcClientVer original


  proxiedClientName Connection {original} = withFrozenCallStack $ proxiedClientName original


  proxiedClientVer Connection {original} = withFrozenCallStack $ proxiedClientVer original


  dbServerVer Connection {original} = withFrozenCallStack $ dbServerVer original


  dbTransactionSupport Connection {original} = withFrozenCallStack $ dbTransactionSupport original


  getTables Connection {original, tracer, attributes} =
    withFrozenCallStack $ do
      Otel.inSpan tracer "getTables" (defaultSpanArguments attributes) $
        getTables original


  describeTable Connection {original, tracer, attributes} tableName =
    withFrozenCallStack $ do
      let attributes' = Attr.insertByKey Attr.db_sql_table (Text.pack tableName) attributes
      Otel.inSpan tracer "describeTable" (defaultSpanArguments attributes') $
        describeTable original tableName


defaultSpanArguments :: Attr.AttributeMap -> Otel.SpanArguments
defaultSpanArguments attributes =
  def {Otel.kind = Otel.Client, Otel.attributes = attributes}
