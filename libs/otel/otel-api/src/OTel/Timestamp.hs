-- | Wall-clock timestamps and durations in nanosecond resolution.
module OTel.Timestamp
  ( -- * Timestamp
    Timestamp (..)
  , now
  , fromNanos
  , toNanos

    -- * Duration
  , Duration (..)
  , milliseconds
  , seconds
  ) where

import Data.Word (Word64)
import System.Clock qualified as Clock


-- | Nanoseconds since Unix epoch.
newtype Timestamp = Timestamp {unTimestamp :: Word64}
  deriving stock (Eq, Ord, Show)


-- | Duration in nanoseconds.
newtype Duration = Duration {unDuration :: Word64}
  deriving stock (Eq, Ord, Show)


-- | Capture the current wall-clock time as a 'Timestamp'.
now :: IO Timestamp
now = do
  t <- Clock.getTime Clock.Realtime
  let nanos = fromIntegral @Integer @Word64 (Clock.toNanoSecs t)
  pure (Timestamp nanos)


-- | Construct a 'Timestamp' from nanoseconds since Unix epoch.
fromNanos :: Word64 -> Timestamp
fromNanos = Timestamp


-- | Extract the nanoseconds value from a 'Timestamp'.
toNanos :: Timestamp -> Word64
toNanos = unTimestamp


-- | Construct a 'Duration' from milliseconds.
milliseconds :: Word64 -> Duration
milliseconds ms = Duration (ms * 1_000_000)


-- | Construct a 'Duration' from seconds.
seconds :: Word64 -> Duration
seconds s = Duration (s * 1_000_000_000)
