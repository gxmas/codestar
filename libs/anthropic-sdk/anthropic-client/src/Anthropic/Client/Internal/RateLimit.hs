-- | Internal: Rate limit header parsing and advisory tracking.
--
-- __This module is not part of the public API.__
module Anthropic.Client.Internal.RateLimit
  ( -- * Rate Limit Tracking
    parseRateLimitHeaders

    -- * TVar State
  , newRateLimitState
  , updateRateLimits
  , readRateLimits
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Network.HTTP.Types (ResponseHeaders)

import Anthropic.Types (RateLimitInfo(..))

-- | Parse rate limit information from HTTP response headers.
--
-- Returns 'Nothing' if any required header is missing or unparseable.
-- Required headers: requests (limit, remaining, reset) and
-- input-tokens (limit, remaining, reset).
parseRateLimitHeaders :: ResponseHeaders -> Maybe RateLimitInfo
parseRateLimitHeaders hdrs = do
  -- Required fields
  reqLimit      <- lookupInt "anthropic-ratelimit-requests-limit"
  reqRemaining  <- lookupInt "anthropic-ratelimit-requests-remaining"
  reqReset      <- lookupTime "anthropic-ratelimit-requests-reset"
  itLimit       <- lookupInt "anthropic-ratelimit-input-tokens-limit"
  itRemaining   <- lookupInt "anthropic-ratelimit-input-tokens-remaining"
  itReset       <- lookupTime "anthropic-ratelimit-input-tokens-reset"
  -- Optional fields
  let otLimit     = lookupInt  "anthropic-ratelimit-output-tokens-limit"
      otRemaining = lookupInt  "anthropic-ratelimit-output-tokens-remaining"
      otReset     = lookupTime "anthropic-ratelimit-output-tokens-reset"
      tLimit      = lookupInt  "anthropic-ratelimit-tokens-limit"
      tRemaining  = lookupInt  "anthropic-ratelimit-tokens-remaining"
      tReset      = lookupTime "anthropic-ratelimit-tokens-reset"
      ra          = lookupInt  "retry-after"
  pure RateLimitInfo
    { requestsLimit         = reqLimit
    , requestsRemaining     = reqRemaining
    , requestsReset         = reqReset
    , inputTokensLimit      = itLimit
    , inputTokensRemaining  = itRemaining
    , inputTokensReset      = itReset
    , outputTokensLimit     = otLimit
    , outputTokensRemaining = otRemaining
    , outputTokensReset     = otReset
    , tokensLimit           = tLimit
    , tokensRemaining       = tRemaining
    , tokensReset           = tReset
    , retryAfter            = ra
    }
  where
    lookupInt :: BS8.ByteString -> Maybe Int
    lookupInt name = do
      val <- lookup (CI.mk name) hdrs
      case BS8.readInt val of
        Just (n, rest) | BS8.null rest -> Just n
        _                              -> Nothing

    lookupTime :: BS8.ByteString -> Maybe UTCTime
    lookupTime name = do
      val <- lookup (CI.mk name) hdrs
      let s = BS8.unpack val
      tryParse "%Y-%m-%dT%H:%M:%S%Z" s
        `orElse` tryParse "%Y-%m-%dT%H:%M:%SZ" s
        `orElse` tryParse "%Y-%m-%dT%H:%M:%S" s

    tryParse :: String -> String -> Maybe UTCTime
    tryParse = parseTimeM True defaultTimeLocale

    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse Nothing y = y
    orElse x       _ = x

-- | Create a new rate limit state (initially empty).
newRateLimitState :: IO (TVar (Maybe RateLimitInfo))
newRateLimitState = newTVarIO Nothing

-- | Update the rate limit state with new information.
updateRateLimits :: TVar (Maybe RateLimitInfo) -> RateLimitInfo -> IO ()
updateRateLimits var info = atomically (writeTVar var (Just info))

-- | Read the current rate limit state.
readRateLimits :: TVar (Maybe RateLimitInfo) -> IO (Maybe RateLimitInfo)
readRateLimits = readTVarIO
