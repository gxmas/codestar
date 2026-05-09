{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal: Retry logic with exponential backoff and jitter.
--
-- __This module is not part of the public API.__
module Anthropic.Client.Internal.Retry
  ( -- * Retry
    withRetry
  , isRetryableStatus
  ) where

import Control.Monad.Catch (Handler(..))
import Control.Retry
  ( RetryPolicyM, RetryStatus(..), exponentialBackoff
  , limitRetries, capDelay, recovering
  )
import Network.HTTP.Client
  ( HttpException(..), HttpExceptionContent(..)
  , responseStatus
  )
import Network.HTTP.Types (statusCode)

import Anthropic.Client.Config (RetryPolicy(..))

-- | Execute an IO action with retry logic.
--
-- Retries on HTTP status codes 429, 500, and 529 with exponential
-- backoff and jitter per the configured 'RetryPolicy'.
--
-- The callback is fired before each retry (not on the initial attempt)
-- with the attempt number (1-based) and computed delay in microseconds.
withRetry :: RetryPolicy -> (Int -> Int -> IO ()) -> IO a -> IO a
withRetry policy onRetryCallback action =
  recovering (toRetryPolicy policy) [httpHandler] $ \rs -> do
    -- Fire callback before retry attempts (rsIterNumber is 0-based)
    let attemptNum = rs.rsIterNumber
    if attemptNum > 0
      then do
        let delay = computeDelay policy attemptNum
        onRetryCallback attemptNum delay
      else pure ()
    action

-- | Whether an HTTP status code is retryable.
isRetryableStatus :: Int -> Bool
isRetryableStatus code = code == 429 || code == 500 || code == 529

-- | Convert our RetryPolicy to the retry library's policy.
toRetryPolicy :: RetryPolicy -> RetryPolicyM IO
toRetryPolicy rp =
  limitRetries rp.maxRetries
    <> capDelay rp.maxDelay (exponentialBackoff rp.initialDelay)

-- | Handler that checks for retryable HTTP exceptions.
httpHandler :: RetryStatus -> Handler IO Bool
httpHandler _ = Handler $ \(e :: HttpException) ->
  pure $ case e of
    HttpExceptionRequest _ (StatusCodeException resp _) ->
      isRetryableStatus (statusCode (responseStatus resp))
    _ -> False

-- | Compute the delay for a given retry attempt (0-based).
computeDelay :: RetryPolicy -> Int -> Int
computeDelay policy attemptNum =
  min (policy.maxDelay) (policy.initialDelay * (2 ^ attemptNum))
