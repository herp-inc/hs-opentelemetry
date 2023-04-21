module OpenTelemetry.Instrumentation.Hedis.Internal.Wrapper (
  wrap0,
  wrap1,
  wrap2,
  wrap3,
  wrap4,
  wrap5,
  wrap6,
) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import qualified OpenTelemetry.Trace.Core as Otel (defaultSpanArguments, inSpan)
import qualified OpenTelemetry.Trace.Monad as Otel (MonadTracer, getTracer)


wrap0 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> m z -> m z
wrap0 n m = do
  tracer <- Otel.getTracer
  Otel.inSpan tracer n Otel.defaultSpanArguments m


wrap1 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> m z) -> a -> m z
wrap1 n z a = wrap0 n $ z a


wrap2 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> b -> m z) -> a -> b -> m z
wrap2 n z a b = wrap0 n $ z a b


wrap3 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> b -> c -> m z) -> a -> b -> c -> m z
wrap3 n z a b c = wrap0 n $ z a b c


wrap4 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> b -> c -> d -> m z) -> a -> b -> c -> d -> m z
wrap4 n z a b c d = wrap0 n $ z a b c d


wrap5 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> b -> c -> d -> e -> m z) -> a -> b -> c -> d -> e -> m z
wrap5 n z a b c d e = wrap0 n $ z a b c d e


wrap6 :: (Otel.MonadTracer m, MonadUnliftIO m, HasCallStack) => Text -> (a -> b -> c -> d -> e -> f -> m z) -> a -> b -> c -> d -> e -> f -> m z
wrap6 n z a b c d e f = wrap0 n $ z a b c d e f
