module OpenTelemetry.Common where

import Data.Default.Class (Default (def))
import Data.Word (Word8)
import System.Clock (TimeSpec)


newtype Timestamp = Timestamp TimeSpec
  deriving (Read, Show, Eq, Ord)


-- | Contain details about the trace. Unlike TraceState values, TraceFlags are present in all traces. The current version of the specification only supports a single flag called sampled.
newtype TraceFlags = TraceFlags Word8
  deriving (Show, Eq, Ord)


instance Default TraceFlags where
  def = TraceFlags 0
