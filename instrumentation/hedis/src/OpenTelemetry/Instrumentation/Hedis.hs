{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.Hedis (
  Redis (Redis),
  runRedis,
  runRedis',
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT, runReaderT))
import qualified Database.Redis as Redis
import qualified OpenTelemetry.Trace.Core as Otel (Tracer, defaultSpanArguments, inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel (MonadTracer, getTracer)


newtype Redis a
  = Redis (ReaderT Otel.Tracer Redis.Redis a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadReader Otel.Tracer)


instance Redis.MonadRedis Redis where
  liftRedis r = do
    tracer <- ask
    Otel.inSpan tracer "liftRedis" Otel.defaultSpanArguments $ lift r


instance Redis.RedisCtx Redis (Either Redis.Reply) where
  returnDecode = lift . Redis.returnDecode


lift :: Redis.Redis a -> Redis a
lift r = Redis $ ReaderT $ const r


runRedis :: MonadIO m => Redis.Connection -> Otel.Tracer -> Redis a -> m a
runRedis conn tracer (Redis m) = liftIO $ Redis.runRedis conn $ runReaderT m tracer


runRedis' :: (Otel.MonadTracer m, MonadIO m) => Redis.Connection -> Redis a -> m a
runRedis' conn redis = do
  tracer <- Otel.getTracer
  runRedis conn tracer redis
