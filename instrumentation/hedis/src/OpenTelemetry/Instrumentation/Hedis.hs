{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module OpenTelemetry.Instrumentation.Hedis (
  -- * The Redis Monad
  Redis (Redis),
  runRedis,
  runRedis',

  -- * Connection
  Connection (..),
  Orig.ConnectError (..),
  connect,
  connect',
  checkedConnect,
  checkedConnect',
  disconnect,
  disconnect',
  withConnect,
  withConnect',
  withCheckedConnect,
  withCheckedConnect',
  Orig.ConnectInfo (..),
  Orig.defaultConnectInfo,
  Orig.parseConnectInfo,
  Orig.connectCluster,
  Orig.PortID (..),

  -- * Commands

  -- ** Connection
  auth,
  echo,
  ping,
  quit,
  select,

  -- ** Keys
  del,
  dump,
  exists,
  expire,
  expireat,
  keys,
  Orig.MigrateOpts (..),
  Orig.defaultMigrateOpts,
  migrate,
  migrateMultiple,
  move,
  objectRefcount,
  objectEncoding,
  objectIdletime,
  persist,
  pexpire,
  pexpireat,
  pttl,
  randomkey,
  rename,
  renamenx,
  restore,
  restoreReplace,
  Orig.Cursor,
  Orig.cursor0,
  Orig.ScanOpts (..),
  Orig.defaultScanOpts,
  scan,
  scanOpts,
  Orig.SortOpts (..),
  Orig.defaultSortOpts,
  Orig.SortOrder (..),
  sort,
  sortStore,
  ttl,
  Orig.RedisType (..),
  getType,
  wait,

  -- ** Hashes
  hdel,
  hexists,
  hget,
  hgetall,
  hincrby,
  hincrbyfloat,
  hkeys,
  hlen,
  hmget,
  hmset,
  hscan,
  hscanOpts,
  hset,
  hsetnx,
  hstrlen,
  hvals,

  -- ** HyperLogLogs
  pfadd,
  pfcount,
  pfmerge,

  -- ** Lists
  blpop,
  brpop,
  brpoplpush,
  lindex,
  linsertBefore,
  linsertAfter,
  llen,
  lpop,
  lpush,
  lpushx,
  lrange,
  lrem,
  lset,
  ltrim,
  rpop,
  rpoplpush,
  rpush,
  rpushx,

  -- ** Scripting
  eval,
  evalsha,
  Orig.DebugMode,
  scriptDebug,
  scriptExists,
  scriptFlush,
  scriptKill,
  scriptLoad,

  -- ** Server
  bgrewriteaof,
  bgsave,
  clientGetname,
  clientList,
  clientPause,
  Orig.ReplyMode,
  clientReply,
  clientSetname,
  commandCount,
  commandInfo,
  configGet,
  configResetstat,
  configRewrite,
  configSet,
  dbsize,
  debugObject,
  flushall,
  flushdb,
  info,
  infoSection,
  lastsave,
  save,
  slaveof,
  Orig.Slowlog (..),
  slowlogGet,
  slowlogLen,
  slowlogReset,
  time,

  -- ** Sets
  sadd,
  scard,
  sdiff,
  sdiffstore,
  sinter,
  sinterstore,
  sismember,
  smembers,
  smove,
  spop,
  spopN,
  srandmember,
  srandmemberN,
  srem,
  sscan,
  sscanOpts,
  sunion,
  sunionstore,

  -- ** Sorted Sets
  Orig.ZaddOpts (..),
  Orig.defaultZaddOpts,
  zadd,
  zaddOpts,
  zcard,
  zcount,
  zincrby,
  Orig.Aggregate (..),
  zinterstore,
  zinterstoreWeights,
  zlexcount,
  zrange,
  zrangeWithscores,
  Orig.RangeLex (..),
  zrangebylex,
  zrangebylexLimit,
  zrangebyscore,
  zrangebyscoreWithscores,
  zrangebyscoreLimit,
  zrangebyscoreWithscoresLimit,
  zrank,
  zrem,
  zremrangebylex,
  zremrangebyrank,
  zremrangebyscore,
  zrevrange,
  zrevrangeWithscores,
  zrevrangebyscore,
  zrevrangebyscoreWithscores,
  zrevrangebyscoreLimit,
  zrevrangebyscoreWithscoresLimit,
  zrevrank,
  zscan,
  zscanOpts,
  zscore,
  zunionstore,
  zunionstoreWeights,

  -- ** Strings
  append,
  bitcount,
  bitcountRange,
  bitopAnd,
  bitopOr,
  bitopXor,
  bitopNot,
  bitpos,
  decr,
  decrby,
  get,
  getbit,
  getrange,
  getset,
  incr,
  incrby,
  incrbyfloat,
  mget,
  mset,
  msetnx,
  psetex,
  Orig.Condition (..),
  Orig.SetOpts (..),
  set,
  setOpts,
  setbit,
  setex,
  setnx,
  setrange,
  strlen,

  -- ** Streams
  Orig.XReadOpts (..),
  Orig.defaultXreadOpts,
  Orig.XReadResponse (..),
  Orig.StreamsRecord (..),
  Orig.TrimOpts (..),
  xadd,
  xaddOpts,
  xread,
  xreadOpts,
  xreadGroup,
  xreadGroupOpts,
  xack,
  xgroupCreate,
  xgroupSetId,
  xgroupDestroy,
  xgroupDelConsumer,
  xrange,
  xrevRange,
  xlen,
  Orig.XPendingSummaryResponse (..),
  xpendingSummary,
  Orig.XPendingDetailRecord (..),
  xpendingDetail,
  Orig.XClaimOpts (..),
  Orig.defaultXClaimOpts,
  xclaim,
  xclaimJustIds,
  Orig.XInfoConsumersResponse (..),
  xinfoConsumers,
  Orig.XInfoGroupsResponse (..),
  xinfoGroups,
  Orig.XInfoStreamResponse (..),
  xinfoStream,
  xdel,
  xtrim,
  Orig.inf,
  Orig.ClusterNodesResponse (..),
  Orig.ClusterNodesResponseEntry (..),
  Orig.ClusterNodesResponseSlotSpec (..),
  clusterNodes,
  Orig.ClusterSlotsResponse (..),
  Orig.ClusterSlotsResponseEntry (..),
  Orig.ClusterSlotsNode (..),
  clusterSlots,
  clusterSetSlotNode,
  clusterSetSlotStable,
  clusterSetSlotImporting,
  clusterSetSlotMigrating,
  clusterGetKeysInSlot,

  -- * Transactions
  watch,
  unwatch,
  multiExec,
  Orig.Queued (),
  Orig.TxResult (..),
  Orig.RedisTx (),

  -- * Pub\/Sub
  publish,

  -- ** Subscribing to channels
  -- $pubsubexpl

  -- *** Single-thread Pub/Sub
  pubSub,
  Orig.Message (..),
  Orig.PubSub (),
  Orig.subscribe,
  Orig.unsubscribe,
  Orig.psubscribe,
  Orig.punsubscribe,

  -- *** Continuous Pub/Sub message controller
  pubSubForever,
  pubSubForever',
  Orig.RedisChannel,
  Orig.RedisPChannel,
  Orig.MessageCallback,
  Orig.PMessageCallback,
  Orig.PubSubController,
  Orig.newPubSubController,
  Orig.currentChannels,
  Orig.currentPChannels,
  Orig.addChannels,
  Orig.addChannelsAndWait,
  Orig.removeChannels,
  Orig.removeChannelsAndWait,
  Orig.UnregisterCallbacksAction,

  -- * Low-Level Command API
  sendRequest,
  Orig.Reply (..),
  Orig.Status (..),
  Orig.RedisResult (..),
  Orig.ConnectionLostException (..),
  Orig.ConnectTimeout (..),
  Orig.HashSlot,
  Orig.keyToSlot,
) where

import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (), ReaderT (ReaderT, runReaderT))
import Data.ByteString (ByteString)
import Data.IP (IP)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Database.Redis as Orig
import GHC.Stack (HasCallStack)
import OpenTelemetry.Instrumentation.Hedis.Internal.Action
import OpenTelemetry.Instrumentation.Hedis.Internal.Wrapper (wrap0, wrap1, wrap2)
import qualified OpenTelemetry.Trace.Core as Otel (Attribute, SpanArguments (attributes, kind), SpanKind (Client), Tracer, TracerProvider, defaultSpanArguments, getGlobalTracerProvider, inSpan, makeTracer, tracerOptions)
import qualified OpenTelemetry.Trace.Monad as Otel (MonadTracer, TracerT (TracerT))
import Text.Read (readMaybe)


#if MIN_VERSION_hedis(0, 15, 1)
import Control.Monad.IO.Unlift (MonadUnliftIO)
#else
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Database.Redis.Core.Internal as Orig
#endif


data Connection = Connection {connectInfo :: Orig.ConnectInfo, originalConnection :: Orig.Connection}


{- | A wrapper data type with 'Otel.Tracer'.
@m@ is expected to be 'Orig.Redis' or 'Orig.RedisTx'.
-}
newtype Redis m a
  = Redis (ReaderT Otel.Tracer m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadReader Otel.Tracer)
  deriving (Otel.MonadTracer) via (Otel.TracerT m)

#if !MIN_VERSION_hedis(0, 15, 1)
instance {-# OVERLAPPING #-} MonadUnliftIO (Redis Orig.Redis) where
  withRunInIO inner =
    Redis $ ReaderT $ \tracer -> Orig.Redis $ ReaderT $ \env ->
      inner $ flip runReaderT env . (\(Orig.Redis m) -> m) . flip runReaderT tracer . (\(Redis m) -> m)
#endif


{- | Note: @'Redis' 'Orig.RedisTx'@ cannot be an instance of 'Orig.MonadRedis'
 because 'Orig.RedisTx' is not an instance of 'MonadUnliftIO'.
-}
instance Orig.MonadRedis (Redis Orig.Redis) where
  liftRedis = lift


instance Orig.RedisCtx (Redis Orig.Redis) (Either Orig.Reply) where
  returnDecode = lift . Orig.returnDecode


lift :: m a -> Redis m a
lift r = Redis $ ReaderT $ const r


-- | A function wrapping 'Orig.runRedis' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
runRedis :: (MonadUnliftIO m, HasCallStack) => Connection -> Redis Orig.Redis a -> m a
runRedis conn m = do
  tp <- Otel.getGlobalTracerProvider
  runRedis' tp conn m


-- | A version of an explicit parameter for 'runRedis'.
runRedis' :: (MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Connection -> Redis Orig.Redis a -> m a
runRedis' tp Connection {originalConnection, connectInfo} (Redis m) = do
  let tracer = makeTracer tp
  inSpan tracer "runRedis'" connectInfo $ liftIO $ Orig.runRedis originalConnection $ runReaderT m tracer


-- | A function wrapping 'Orig.connect' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
connect :: (MonadUnliftIO m, HasCallStack) => Orig.ConnectInfo -> m Connection
connect info = do
  tp <- Otel.getGlobalTracerProvider
  connect' tp info


-- | A version of an explicit parameter for 'connect'.
connect' :: (MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Orig.ConnectInfo -> m Connection
connect' tp info = do
  let tracer = makeTracer tp
  doConnect tracer info


doConnect :: (MonadUnliftIO m, HasCallStack) => Otel.Tracer -> Orig.ConnectInfo -> m Connection
doConnect tracer info = inSpan tracer "doConnect" info $ liftIO $ Connection info <$> Orig.connect info


-- | A function wrapping 'Orig.checkedConnect' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
checkedConnect :: (MonadUnliftIO m, HasCallStack) => Orig.ConnectInfo -> m Connection
checkedConnect info = do
  tp <- Otel.getGlobalTracerProvider
  checkedConnect' tp info


-- | A version of an explicit parameter for 'checkedConnect'.
checkedConnect' :: (MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Orig.ConnectInfo -> m Connection
checkedConnect' tp info = do
  let tracer = makeTracer tp
  doCheckedConnect tracer info


doCheckedConnect :: (MonadUnliftIO m, HasCallStack) => Otel.Tracer -> Orig.ConnectInfo -> m Connection
doCheckedConnect tracer info = inSpan tracer "doCheckedConnect" info $ liftIO $ Connection info <$> Orig.checkedConnect info


-- | A function wrapping 'Orig.disconnect' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
disconnect :: (MonadUnliftIO m, HasCallStack) => Connection -> m ()
disconnect conn = do
  tp <- Otel.getGlobalTracerProvider
  disconnect' tp conn


-- | A version of an explicit parameter for 'disconnect'.
disconnect' :: (MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Connection -> m ()
disconnect' tp conn = do
  let tracer = makeTracer tp
  doDisconnect tracer conn


doDisconnect :: (MonadUnliftIO m, HasCallStack) => Otel.Tracer -> Connection -> m ()
doDisconnect tracer Connection {originalConnection, connectInfo} = inSpan tracer "doDisconnect" connectInfo $ liftIO $ Orig.disconnect originalConnection


-- | A function wrapping 'Orig.withConnect' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
withConnect :: (E.MonadMask m, MonadUnliftIO m, HasCallStack) => Orig.ConnectInfo -> (Orig.Connection -> m c) -> m c
withConnect info action = do
  tp <- Otel.getGlobalTracerProvider
  withConnect' tp info action


-- | A version of an explicit parameter for 'withConnect'.
withConnect' :: (E.MonadMask m, MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Orig.ConnectInfo -> (Orig.Connection -> m c) -> m c
withConnect' tp info action = do
  let tracer = makeTracer tp
  inSpan tracer "withConnect'" info $ E.bracket (doConnect tracer info) (doDisconnect tracer) $ \(Connection {originalConnection}) -> action originalConnection


-- | A function wrapping 'Orig.withCheckedConnect' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
withCheckedConnect :: (E.MonadMask m, MonadUnliftIO m, HasCallStack) => Orig.ConnectInfo -> (Orig.Connection -> m c) -> m c
withCheckedConnect info action = do
  tp <- Otel.getGlobalTracerProvider
  withCheckedConnect' tp info action


-- | A version of an explicit parameter for 'withCheckedConnect'.
withCheckedConnect' :: (E.MonadMask m, MonadUnliftIO m, HasCallStack) => Otel.TracerProvider -> Orig.ConnectInfo -> (Orig.Connection -> m c) -> m c
withCheckedConnect' tp info action = do
  let tracer = makeTracer tp
  inSpan tracer "withConnect'" info $ E.bracket (doCheckedConnect tracer info) (doDisconnect tracer) $ \(Connection {originalConnection}) -> action originalConnection


watch :: [ByteString] -> Redis Orig.Redis (Either Orig.Reply Orig.Status)
watch = wrap1 "watch" $ lift . Orig.watch


unwatch :: Redis Orig.Redis (Either Orig.Reply Orig.Status)
unwatch = wrap0 "unwatch" $ lift Orig.unwatch


multiExec :: Redis Orig.RedisTx (Orig.Queued a) -> Redis Orig.Redis (Orig.TxResult a)
multiExec (Redis m) = wrap0 "multiExec" $ Redis $ ReaderT $ Orig.multiExec . runReaderT m


pubSub :: Orig.PubSub -> (Orig.Message -> IO Orig.PubSub) -> Redis Orig.Redis ()
pubSub = wrap2 "pubSub" $ \sub f -> lift $ Orig.pubSub sub f


-- | A function wrapping 'Orig.pubSubForever' with 'Otel.Tracer' using 'Otel.getGlobalTracerProvider'.
pubSubForever :: Connection -> Orig.PubSubController -> IO () -> IO ()
pubSubForever connection controller action = do
  tp <- Otel.getGlobalTracerProvider
  pubSubForever' tp connection controller action


-- | A version of an explicit parameter for 'pubSubForever'.
pubSubForever' :: Otel.TracerProvider -> Connection -> Orig.PubSubController -> IO () -> IO ()
pubSubForever' tp Connection {originalConnection, connectInfo} controller action = do
  let tracer = makeTracer tp
  inSpan tracer "pubSubForever'" connectInfo $ Orig.pubSubForever originalConnection controller action


makeTracer :: Otel.TracerProvider -> Otel.Tracer
makeTracer tp = Otel.makeTracer tp "hs-opentelemetry-instrumentation-hedis" Otel.tracerOptions


inSpan :: (MonadUnliftIO m, HasCallStack) => Otel.Tracer -> Text -> Orig.ConnectInfo -> m a -> m a
inSpan tracer name info f = do
  let args = Otel.defaultSpanArguments {Otel.kind = Otel.Client, Otel.attributes = attachAttributes info []}
  Otel.inSpan tracer name args f


attachAttributes :: Orig.ConnectInfo -> [(Text, Otel.Attribute)] -> [(Text, Otel.Attribute)]
attachAttributes info@Orig.ConnInfo {Orig.connectHost, Orig.connectPort} =
  let
    transportAttr :: Otel.Attribute
    portAttr :: (Text, Otel.Attribute)
    (transportAttr, portAttr) =
      case connectPort of
        Orig.PortNumber n -> ("ip_tcp", ("net.peer.port", fromString $ show n))
        Orig.UnixSocket p -> ("other", ("net.sock.peer.port", fromString p))
   in
    (("db.connection_string", fromString $ showsPrecConnectInfoMasked 0 info "") :)
      . (portAttr :)
      . (("net.transport", transportAttr) :)
      . ((maybe "net.peer.name" (const "net.sock.peer.addr") (readMaybe connectHost :: Maybe IP), fromString connectHost) :)


showsPrecConnectInfoMasked :: Int -> Orig.ConnectInfo -> ShowS
showsPrecConnectInfoMasked d Orig.ConnInfo {Orig.connectHost, Orig.connectPort, Orig.connectAuth, Orig.connectDatabase, Orig.connectMaxConnections, Orig.connectMaxIdleTime, Orig.connectTimeout, Orig.connectTLSParams} =
  showParen (d > 10) $
    showString "ConnInfo {"
      . (showString "connectHost = " . shows connectHost . showString ", ")
      . (showString "connectPort = " . shows connectPort . showString ", ")
      . (showString "connectAuth = " . maybe (shows (Nothing :: Maybe ())) (const $ showString "Just \"****\"") connectAuth . showString ", ")
      . (showString "connectDatabase = " . shows connectDatabase . showString ", ")
      . (showString "connectMaxConnections = " . shows connectMaxConnections . showString ", ")
      . (showString "connectMaxIdleTime = " . shows connectMaxIdleTime . showString ", ")
      . (showString "connectTimeout = " . shows connectTimeout . showString ", ")
      . (showString "connectTLSParams = " . shows connectTLSParams)
      . showString "}"
