-- | Rate limiting for Anthropic API-conforming servers.
--
-- Token bucket implementation with per-dimension tracking (RPM/ITPM/OTPM).
-- Framework-agnostic: provides composable building blocks.
module Anthropic.Server.RateLimit
  ( -- * Rate Limiter
    RateLimiter
  , RateLimitConfig (..)
  , RateLimitDimension (..)
  , RateLimitResult (..)

    -- * Operations
  , newRateLimiter
  , checkRateLimit
  , consumeTokens
  , getRateLimitHeaders
  ) where

import Control.Concurrent.STM

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)


import Data.Text (Text)
import Anthropic.Types (ApiKey (..), ModelId (..), Usage (..))

-- | Rate limit dimension.
data RateLimitDimension
  = RequestsPerMinute
  | InputTokensPerMinute
  | OutputTokensPerMinute
  deriving stock (Eq, Ord, Show)

-- | Configuration for rate limiting.
data RateLimitConfig = RateLimitConfig
  { requestsLimit    :: !Int
  , inputTokensLimit :: !Int
  , outputTokensLimit :: !Int
  }
  deriving stock (Eq, Show)

-- | Result of a rate limit check.
data RateLimitResult
  = Allowed
  | RateLimited !RateLimitDimension !Int
    -- ^ Dimension exceeded and seconds to wait.
  deriving stock (Eq, Show)

-- | Token bucket state for a single dimension.
data TokenBucket = TokenBucket
  { bucketTokens    :: !Double
    -- ^ Current tokens available (fractional for smooth refill).
  , bucketCapacity  :: !Int
    -- ^ Maximum tokens.
  , bucketRefillRate :: !Double
    -- ^ Tokens added per second.
  , bucketLastRefill :: !UTCTime
    -- ^ Last refill timestamp.
  }

-- | Per-key rate limit state.
-- Using Text keys instead of ApiKey/ModelId to avoid Ord requirement
type RateLimitState = Map (Text, Text) (Map RateLimitDimension TokenBucket)

-- | Opaque rate limiter handle.
data RateLimiter = RateLimiter
  { limiterConfig :: !RateLimitConfig
  , limiterState  :: !(TVar RateLimitState)
  }

-- | Create a new rate limiter.
newRateLimiter :: RateLimitConfig -> IO RateLimiter
newRateLimiter config = do
  stateVar <- newTVarIO Map.empty
  pure RateLimiter
    { limiterConfig = config
    , limiterState = stateVar
    }

-- | Check whether a request should be allowed.
checkRateLimit
  :: RateLimiter
  -> ApiKey
  -> ModelId
  -> Int          -- ^ Estimated input tokens
  -> IO RateLimitResult
checkRateLimit limiter (ApiKey key) (ModelId model) estimatedTokens = do
  now <- getCurrentTime
  atomically $ do
    state <- readTVar limiter.limiterState
    let keyState = Map.findWithDefault Map.empty (key, model) state
        config = limiter.limiterConfig

        -- Refill buckets based on time elapsed
        refillBucket bucket = do
          let elapsed = realToFrac $ diffUTCTime now bucket.bucketLastRefill
              tokensToAdd = bucket.bucketRefillRate * elapsed
              newTokens = min (fromIntegral bucket.bucketCapacity)
                              (bucket.bucketTokens + tokensToAdd)
          bucket
            { bucketTokens = newTokens
            , bucketLastRefill = now
            }

        -- Initialize or refresh buckets
        reqBucket = refillBucket $ Map.findWithDefault
          (initBucket config.requestsLimit (fromIntegral config.requestsLimit / 60) now)
          RequestsPerMinute
          keyState
        inBucket = refillBucket $ Map.findWithDefault
          (initBucket config.inputTokensLimit (fromIntegral config.inputTokensLimit / 60) now)
          InputTokensPerMinute
          keyState
        outBucket = refillBucket $ Map.findWithDefault
          (initBucket config.outputTokensLimit (fromIntegral config.outputTokensLimit / 60) now)
          OutputTokensPerMinute
          keyState

        -- Check if we have enough tokens
        hasRequests = reqBucket.bucketTokens >= 1
        hasInputTokens = inBucket.bucketTokens >= fromIntegral estimatedTokens

    if not hasRequests
      then do
        -- Not enough request tokens
        let waitTime = ceiling (60 / reqBucket.bucketRefillRate :: Double)
        pure $ RateLimited RequestsPerMinute waitTime
      else if not hasInputTokens
      then do
        -- Not enough input token quota
        let waitTime = ceiling ((fromIntegral estimatedTokens - inBucket.bucketTokens) / inBucket.bucketRefillRate)
        pure $ RateLimited InputTokensPerMinute waitTime
      else do
        -- Consume the request token (actual token consumption happens in consumeTokens)
        let newReqBucket = reqBucket { bucketTokens = reqBucket.bucketTokens - 1 }
            newKeyState = Map.insert RequestsPerMinute newReqBucket $
                          Map.insert InputTokensPerMinute inBucket $
                          Map.insert OutputTokensPerMinute outBucket keyState
            newState = Map.insert (key, model) newKeyState state
        writeTVar limiter.limiterState newState
        pure Allowed

-- | Record token consumption after a request completes.
consumeTokens
  :: RateLimiter
  -> ApiKey
  -> ModelId
  -> Usage
  -> IO ()
consumeTokens limiter (ApiKey key) (ModelId model) usage = do
  now <- getCurrentTime
  atomically $ do
    state <- readTVar limiter.limiterState
    let keyState = Map.findWithDefault Map.empty (key, model) state
        config = limiter.limiterConfig

        -- Consume input and output tokens
        consumeFromBucket bucket tokens =
          bucket { bucketTokens = max 0 (bucket.bucketTokens - fromIntegral tokens) }

        inBucket = Map.findWithDefault
          (initBucket config.inputTokensLimit (fromIntegral config.inputTokensLimit / 60) now)
          InputTokensPerMinute
          keyState
        outBucket = Map.findWithDefault
          (initBucket config.outputTokensLimit (fromIntegral config.outputTokensLimit / 60) now)
          OutputTokensPerMinute
          keyState

        newInBucket = consumeFromBucket inBucket usage.inputTokens
        newOutBucket = consumeFromBucket outBucket usage.outputTokens

        newKeyState = Map.insert InputTokensPerMinute newInBucket $
                      Map.insert OutputTokensPerMinute newOutBucket keyState
        newState = Map.insert (key, model) newKeyState state

    writeTVar limiter.limiterState newState

-- | Get rate limit response headers for inclusion in HTTP responses.
getRateLimitHeaders
  :: RateLimiter
  -> ApiKey
  -> ModelId
  -> IO [(ByteString, ByteString)]
getRateLimitHeaders limiter (ApiKey key) (ModelId model) = do
  now <- getCurrentTime
  state <- readTVarIO limiter.limiterState
  let keyState = Map.findWithDefault Map.empty (key, model) state
      config = limiter.limiterConfig

      reqBucket = Map.findWithDefault
        (initBucket config.requestsLimit (fromIntegral config.requestsLimit / 60) now)
        RequestsPerMinute
        keyState
      inBucket = Map.findWithDefault
        (initBucket config.inputTokensLimit (fromIntegral config.inputTokensLimit / 60) now)
        InputTokensPerMinute
        keyState
      outBucket = Map.findWithDefault
        (initBucket config.outputTokensLimit (fromIntegral config.outputTokensLimit / 60) now)
        OutputTokensPerMinute
        keyState

      resetTime = addUTCTime 60 now
      resetTimestamp = floor (utcTimeToPOSIXSeconds resetTime) :: Int

      headers =
        [ ("anthropic-ratelimit-requests-limit", BS8.pack $ show config.requestsLimit)
        , ("anthropic-ratelimit-requests-remaining", BS8.pack $ show (floor reqBucket.bucketTokens :: Int))
        , ("anthropic-ratelimit-requests-reset", BS8.pack $ show resetTimestamp)
        , ("anthropic-ratelimit-input-tokens-limit", BS8.pack $ show config.inputTokensLimit)
        , ("anthropic-ratelimit-input-tokens-remaining", BS8.pack $ show (floor inBucket.bucketTokens :: Int))
        , ("anthropic-ratelimit-input-tokens-reset", BS8.pack $ show resetTimestamp)
        , ("anthropic-ratelimit-output-tokens-limit", BS8.pack $ show config.outputTokensLimit)
        , ("anthropic-ratelimit-output-tokens-remaining", BS8.pack $ show (floor outBucket.bucketTokens :: Int))
        , ("anthropic-ratelimit-output-tokens-reset", BS8.pack $ show resetTimestamp)
        ]

  pure headers

-- | Initialize a token bucket.
initBucket :: Int -> Double -> UTCTime -> TokenBucket
initBucket capacity refillRate now = TokenBucket
  { bucketTokens = fromIntegral capacity
  , bucketCapacity = capacity
  , bucketRefillRate = refillRate
  , bucketLastRefill = now
  }
