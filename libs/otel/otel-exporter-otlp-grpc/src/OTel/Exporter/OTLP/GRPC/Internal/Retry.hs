module OTel.Exporter.OTLP.GRPC.Internal.Retry
  ( RetryConfig (..)
  , GrpcError (..)
  , defaultRetryConfig
  , withRetry
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.Text (Text)
import System.Random (randomRIO)
import OTel.Exporter.OTLP.GRPC.Internal.Status


-- | Configuration for exponential backoff retry on transient gRPC errors.
data RetryConfig = RetryConfig
  { retryMaxAttempts :: Int
  , retryInitialDelay :: Int
  , retryMaxDelay :: Int
  } deriving stock (Show)


-- | Errors from gRPC unary RPC calls.
data GrpcError
  = GrpcStatusError GrpcStatus (Maybe Int)
  | GrpcProtocolError Text
  | GrpcNetworkError SomeException
  deriving stock (Show)


-- | Default retry config: 5 retries, 200ms initial backoff, 10s max.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts = 5
  , retryInitialDelay = 1_000_000
  , retryMaxDelay = 30_000_000
  }


-- | Execute an action with exponential backoff retry on transient errors.
withRetry
  :: RetryConfig
  -> (Int -> IO (Either GrpcError a))
  -> IO (Either GrpcError a)
withRetry cfg action = go 0
  where
    go attempt = do
      result <- action attempt
      case result of
        Right _ -> pure result
        Left err
          | attempt + 1 >= cfg.retryMaxAttempts -> pure result
          | shouldRetry err -> do
              let delay = computeDelay cfg attempt err
              actualDelay <- addJitter delay
              threadDelay actualDelay
              go (attempt + 1)
          | otherwise -> pure result


shouldRetry :: GrpcError -> Bool
shouldRetry (GrpcStatusError status _) = isRetryable status.statusCode
shouldRetry (GrpcProtocolError _) = False
shouldRetry (GrpcNetworkError _) = True


computeDelay :: RetryConfig -> Int -> GrpcError -> Int
computeDelay cfg attempt err =
  case err of
    GrpcStatusError _ (Just retryAfterMs) -> retryAfterMs * 1000
    _ ->
      let base = cfg.retryInitialDelay * (2 ^ attempt)
       in min base cfg.retryMaxDelay


addJitter :: Int -> IO Int
addJitter delay = do
  jitter <- randomRIO (0, delay)
  pure (delay + jitter)
