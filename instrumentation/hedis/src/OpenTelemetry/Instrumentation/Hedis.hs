{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.Hedis (
  appendHooksToConnectionInfo,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import qualified Database.Redis as Orig
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified OpenTelemetry.Attributes as Otel (emptyAttributes)
import qualified OpenTelemetry.Attributes.Map as Otel
import qualified OpenTelemetry.SemanticConventions as Otel
import qualified OpenTelemetry.Trace.Core as Otel
import Paths_hs_opentelemetry_instrumentation_hedis (version)


appendHooksToConnectionInfo :: (MonadIO m, HasCallStack) => Otel.TracerProvider -> Orig.ConnectInfo -> m Orig.ConnectInfo
appendHooksToConnectionInfo tracerProvider connectInfo@Orig.ConnInfo {Orig.connectHooks} = withFrozenCallStack $ liftIO $ do
  let
    tracer =
      Otel.makeTracer
        tracerProvider
        (Otel.InstrumentationLibrary "hs-opentelemetry-instrumentation-hedis" (Text.pack $ showVersion version) "" Otel.emptyAttributes)
        Otel.tracerOptions
  pure
    connectInfo
      { Orig.connectHooks =
          connectHooks
            { Orig.sendRequestHook = sendDatabaseOrPubSubHook tracer connectInfo . Orig.sendRequestHook connectHooks
            , Orig.sendPubSubHook = sendDatabaseOrPubSubHook tracer connectInfo . Orig.sendPubSubHook connectHooks
            , Orig.callbackHook = callbackHook tracer connectInfo . Orig.callbackHook connectHooks
            }
      }


sendDatabaseOrPubSubHook :: HasCallStack => Otel.Tracer -> Orig.ConnectInfo -> ([ByteString] -> IO a) -> [ByteString] -> IO a
sendDatabaseOrPubSubHook tracer connectInfo send message@[command, channel, _] | elem command ["PUBLISH", "SPUBLISH"] = do
  let spanName = decodeBs channel <> " publish"
      kind = Otel.Producer
      attributes = makePubSubAttributes connectInfo $ Right message
  Otel.inSpan tracer spanName Otel.defaultSpanArguments {Otel.kind, Otel.attributes} $ send message
sendDatabaseOrPubSubHook _ _ send message@(command : _) | elem command pubSubCommands = send message
sendDatabaseOrPubSubHook tracer connectInfo send message = do
  let spanName = makeDatabaseSpanName message
      attributes = makeDatabaseAttributes connectInfo message
      kind = Otel.Client
  Otel.inSpan tracer spanName Otel.defaultSpanArguments {Otel.kind, Otel.attributes} $ send message


callbackHook :: HasCallStack => Otel.Tracer -> Orig.ConnectInfo -> (Orig.Message -> IO Orig.PubSub) -> Orig.Message -> IO Orig.PubSub
callbackHook tracer connectInfo callback message = do
  let spanName = decodeBs (Orig.msgChannel message) <> " deliver"
      kind = Otel.Consumer
      attributes = makePubSubAttributes connectInfo $ Left message
  Otel.inSpan tracer spanName Otel.defaultSpanArguments {Otel.kind, Otel.attributes} $ callback message


makeDatabaseSpanName :: HasCallStack => [ByteString] -> Text
makeDatabaseSpanName (command : _) = decodeBs command
makeDatabaseSpanName _ = error "unexpected"


makeDatabaseAttributes :: Orig.ConnectInfo -> [ByteString] -> Otel.AttributeMap
makeDatabaseAttributes Orig.ConnInfo {Orig.connectHost, Orig.connectPort, Orig.connectUsername, Orig.connectDatabase} request =
  let
    host :: Text
    maybePort :: Maybe Int64
    transport :: Text
    (host, maybePort, transport) =
      case connectPort of
        Orig.PortNumber n -> (Text.pack connectHost, Just $ fromInteger $ toInteger n, "tcp")
        Orig.UnixSocket s -> (Text.pack s, Nothing, "unix")
    statement = Text.unwords $ (\s -> "'" <> s <> "'") . decodeBs <$> request
   in
    mempty
      -- Database Client attributes
      -- attributes to dissmiss: db.connection_string, network.type
      & Otel.insertByKey Otel.db_instance_id host
      & Otel.insertByKey Otel.db_system "redis"
      & maybe id (Otel.insertByKey Otel.db_user . decodeBs) connectUsername
      & Otel.insertByKey Otel.network_peer_address host
      & maybe id (Otel.insertByKey Otel.network_peer_port) maybePort
      & Otel.insertByKey Otel.network_transport transport
      & Otel.insertByKey Otel.server_address host
      & maybe id (Otel.insertByKey Otel.server_port) maybePort
      -- Database Client Call-level attributes
      -- attributes to dissmiss: db.name, db.operation
      & Otel.insertByKey Otel.db_statement statement
      -- Redis Call-level attributes
      & Otel.insertByKey Otel.db_redis_databaseIndex (fromInteger connectDatabase)


makePubSubAttributes :: Orig.ConnectInfo -> Either Orig.Message [ByteString] -> Otel.AttributeMap
makePubSubAttributes Orig.ConnInfo {Orig.connectHost, Orig.connectPort} message =
  let
    destination :: Text
    operation :: Text
    message' :: Text
    maybePattern :: Maybe Text
    (destination, operation, message', maybePattern) =
      case message of
        Right ["PUBLISH", channel, message''] -> (decodeBs channel, "publish", "'" <> decodeBs message'' <> "'", Nothing)
        Right (command : _) -> error $ "unexpected command: " <> show command
        Right [] -> error "unexpected"
        Left Orig.Message {Orig.msgChannel, Orig.msgMessage} -> (decodeBs msgChannel, "deliver", "'" <> decodeBs msgMessage <> "'", Nothing)
        Left Orig.PMessage {Orig.msgPattern, Orig.msgChannel, Orig.msgMessage} -> (decodeBs msgChannel, "deliver", "'" <> decodeBs msgMessage <> "'", Just $ decodeBs msgPattern)
    host :: Text
    maybePort :: Maybe Int64
    transport :: Text
    (host, maybePort, transport) =
      case connectPort of
        Orig.PortNumber n -> (Text.pack connectHost, Just $ fromInteger $ toInteger n, "tcp")
        Orig.UnixSocket s -> (Text.pack s, Nothing, "unix")
   in
    mempty
      -- Messaging attributes
      -- attributes to dissmiss:
      --   - messaging.batch.message_count
      --   - messaging.client_id
      --   - messaging.destination.template
      --   - messaging.message.body.size
      --   - messaging.message.conversation_id
      --   - messaging.message.envelope.size
      --   - messaging.message.id
      --   - network.protocol.version
      --   - network.type
      & Otel.insertByKey Otel.messaging_destination_anonymous False
      & Otel.insertByKey Otel.messaging_destination_name destination
      & Otel.insertByKey Otel.messaging_destination_temporary False
      & Otel.insertByKey Otel.messaging_operation operation
      & Otel.insertByKey Otel.messaging_system "redis"
      & Otel.insertByKey Otel.network_peer_address host
      & maybe id (Otel.insertByKey Otel.network_peer_port) maybePort
      & Otel.insertByKey Otel.network_protocol_name "redis"
      & Otel.insertByKey Otel.network_transport transport
      & Otel.insertByKey Otel.server_address host
      & maybe id (Otel.insertByKey Otel.server_port) maybePort
      -- Per-message attributes
      & Otel.insert "messaging.message" (Otel.AttributeValue $ Otel.TextAttribute message')
      & maybe id (Otel.insertByKey "messaging.redis.pattern" . Otel.AttributeValue . Otel.TextAttribute) maybePattern


pubSubCommands :: [ByteString]
pubSubCommands =
  [ "SUBSCRIBE"
  , "SSUBSCRIBE"
  , "PSUBSCRIBE"
  , "UNSUBSCRIBE"
  , "SUNSUBSCRIBE"
  , "PUNSUBSCRIBE"
  ]


decodeBs :: ByteString -> Text
#if MIN_VERSION_text(2,0,0)
decodeBs = Text.decodeUtf8Lenient
#else
decodeBs = Text.decodeLatin1
#endif
