{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
 Module      :  OpenTelemetry.Trace.Monad
 Copyright   :  (c) Ian Duncan, 2021
 License     :  BSD-3
 Description :  Higher-level tracing API
 Maintainer  :  Ian Duncan
 Stability   :  experimental
 Portability :  non-portable (GHC extensions)

 The recommended tracing interface for application developers

 See OpenTelemetry.Trace for an interface that's
 more lower-level, but more flexible.
-}
module OpenTelemetry.Trace.Monad (
  inSpan,
  inSpan',
  OpenTelemetry.Trace.Monad.inSpan'',
  -- Interacting with the span in the current context
  -- , getSpan
  -- , updateName
  -- , addAttribute
  -- , addAttributes
  -- , getAttributes
  -- , addEvent
  -- , NewEvent (..)
  -- Fundamental monad instances
  MonadTracer (..),
  TracerT (..),
  runTracerT,
) where

import Control.Applicative (Const (Const, getConst))
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Text (Text)
import GHC.Stack (CallStack, HasCallStack, callStack)
import OpenTelemetry.Trace.Core (
  HasTracer (tracerL),
  Span,
  SpanArguments (..),
  Tracer,
  addAttributesToSpanArguments,
  callerAttributes,
  inSpan'',
 )


-- | This is generally scoped by Monad stack to do different things
class (Monad m) => MonadTracer m where
  getTracer :: m Tracer


inSpan
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => Text
  -> SpanArguments
  -> m a
  -> m a
inSpan n args m = OpenTelemetry.Trace.Monad.inSpan'' n (addAttributesToSpanArguments callerAttributes args) (const m)


inSpan'
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => Text
  -> SpanArguments
  -> (Span -> m a)
  -> m a
inSpan' n args f = OpenTelemetry.Trace.Monad.inSpan'' n (addAttributesToSpanArguments callerAttributes args) f


inSpan''
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => Text
  -> SpanArguments
  -> (Span -> m a)
  -> m a
inSpan'' n args f = do
  t <- getTracer
  OpenTelemetry.Trace.Core.inSpan'' t n args f


instance (MonadTracer m) => MonadTracer (IdentityT m) where
  getTracer = lift getTracer


instance {-# OVERLAPPABLE #-} (MonadTracer m) => MonadTracer (ReaderT r m) where
  getTracer = lift getTracer


{- | Another 'MonadTracer' instance for 'ReaderT'.
This @newtype@ data type is intended to be used with @DerivingVia@ language extension.
-}
newtype TracerT s m a = TracerT (ReaderT s m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadFail, MonadReader s)


runTracerT :: s -> TracerT s m a -> m a
runTracerT s (TracerT m) = runReaderT m s


instance (Monad m, HasTracer s) => MonadTracer (TracerT s m) where
  getTracer = ask >>= pure . getConst . tracerL Const
