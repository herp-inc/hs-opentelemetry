{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  OpenTelemetry.Trace.Monad
-- Copyright   :  (c) Ian Duncan, 2021
-- License     :  BSD-3
-- Description :  Higher-level tracing API
-- Maintainer  :  Ian Duncan
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions)
--
-- The recommended tracing interface for application developers
--
-- See OpenTelemetry.Trace for an interface that's
-- more lower-level, but more flexible.
--
-----------------------------------------------------------------------------
module OpenTelemetry.Trace.Monad
  ( inSpan
  , inSpan'
  , OpenTelemetry.Trace.Monad.inSpan''
  -- Interacting with the span in the current context
  -- , getSpan
  -- , updateName
  -- , addAttribute
  -- , addAttributes
  -- , getAttributes
  -- , addEvent
  -- , NewEvent (..)
  -- Fundamental monad instances
  , MonadTracer(..)
  ) where

import Data.Text (Text)
import OpenTelemetry.Trace.Core
  ( Tracer
  , Span
  , SpanArguments(..)
  , inSpan''
  )
import Control.Monad.IO.Unlift
import GHC.Stack
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Accum (AccumT)
import Control.Monad.Cont (ContT)
import Control.Monad.Identity (IdentityT)
import qualified Control.Monad.Trans.RWS.CPS as C
import qualified Control.Monad.RWS.Lazy as L
import qualified Control.Monad.RWS.Strict as S
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Select (SelectT)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S

-- | This is generally scoped by Monad stack to do different things
class Monad m => MonadTracer m where
  getTracer :: m Tracer

inSpan
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => Text
  -> SpanArguments
  -> m a
  -> m a
inSpan n args m = OpenTelemetry.Trace.Monad.inSpan'' callStack n args (const m)

inSpan'
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => Text
  -> SpanArguments
  -> (Span -> m a)
  -> m a
inSpan' = OpenTelemetry.Trace.Monad.inSpan'' callStack

inSpan''
  :: (MonadUnliftIO m, MonadTracer m, HasCallStack)
  => CallStack
  -> Text
  -> SpanArguments
  -> (Span -> m a)
  -> m a
inSpan'' cs n args f = do
  t <- getTracer
  OpenTelemetry.Trace.Core.inSpan'' t cs n args f

instance (MonadTracer m, Monoid w) => MonadTracer (AccumT w m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (ContT r m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (IdentityT m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (C.RWST r w s m) where
  getTracer = lift getTracer

instance (MonadTracer m, Monoid w) => MonadTracer (L.RWST r w s m) where
  getTracer = lift getTracer

instance (MonadTracer m, Monoid w) => MonadTracer (S.RWST r w s m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (ReaderT r m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (SelectT r m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (L.StateT s m) where
  getTracer = lift getTracer

instance MonadTracer m => MonadTracer (S.StateT s m) where
  getTracer = lift getTracer

instance (MonadTracer m, Monoid w) => MonadTracer (L.WriterT w m) where
  getTracer = lift getTracer

instance (MonadTracer m, Monoid w) => MonadTracer (S.WriterT w m) where
  getTracer = lift getTracer
