-- | Client configuration, lifecycle management, and error types.
module Anthropic.Client.Config
  ( -- * Client (opaque)
    AnthropicClient (..)

    -- * Configuration
  , ClientConfig (..)
  , defaultConfig
  , RetryPolicy (..)
  , defaultRetryPolicy

    -- ** ClientConfig Setters
  , withBaseUrl
  , withDefaultHeaders
  , withRetryPolicy
  , withTimeout
  , withBetaFeatures
  , withOnResponseMeta
  , withOnRetry

    -- * Client Lifecycle
  , newClient
  , closeClient
  , withClient

    -- * Error Type
  , ClientError (..)

    -- * Tracer
  , getClientTracer

    -- * Rate Limit Observability
  , getRateLimits
  ) where

import Control.Concurrent.STM (TVar)
import Control.Exception (bracket)
import Data.Text (Text)
import Network.HTTP.Client (HttpException, Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Data.ByteString (ByteString)

import Anthropic.Types

import Anthropic.Client.Internal.RateLimit
  ( newRateLimitState, readRateLimits )

import OTel.Trace (SomeTracer, getGlobalTracerProvider, getTracer)
import OTel.Attribute (InstrumentationScope(..))

-- | Retry policy for failed requests.
data RetryPolicy = RetryPolicy
  { maxRetries   :: !Int
  , initialDelay :: !Int
    -- ^ Initial delay in microseconds.
  , maxDelay     :: !Int
    -- ^ Maximum delay in microseconds.
  , jitter       :: !Bool
    -- ^ Whether to add random jitter to delays.
  }
  deriving stock (Eq, Show)

-- | Default retry policy: 3 retries, 500ms initial delay, 30s max, with jitter.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { maxRetries   = 3
  , initialDelay = 500000
  , maxDelay     = 30000000
  , jitter       = True
  }

-- | Client configuration.
--
-- Use 'defaultConfig' and override fields as needed:
--
-- @
-- let config = (defaultConfig \"sk-my-key\")
--       { baseUrl = Just \"https://my-proxy.example.com\"
--       , onResponseMeta = \\meta -> pushToPrometheus meta.rateLimits
--       }
-- @
data ClientConfig = ClientConfig
  { apiKey          :: !ApiKey
  , baseUrl         :: !(Maybe Text)
    -- ^ Override the default API base URL.
  , defaultHeaders  :: !(Maybe [(ByteString, ByteString)])
    -- ^ Additional headers to include on every request.
  , retryPolicy     :: !RetryPolicy
  , timeout         :: !(Maybe Int)
    -- ^ Request timeout in seconds.
  , betaFeatures    :: !(Maybe [Text])
    -- ^ Beta feature strings for the @anthropic-beta@ header.
  , onResponseMeta  :: ResponseMeta -> IO ()
    -- ^ Callback fired on every response with rate limit info and request ID.
    -- Default: @const (pure ())@. See ADR-003.
  , onRetry         :: Int -> Int -> IO ()
    -- ^ Callback fired before each retry attempt.
    -- Arguments: attempt number (1-based), delay in microseconds.
    -- Default: @\\_ _ -> pure ()@. Useful for retry logging and debugging.
  }

-- | Default configuration with only the API key.
--
-- All optional fields use sensible defaults. Configure with @with*@ setters:
--
-- @
-- let config = defaultConfig "sk-my-key"
--            & withBaseUrl "https://my-proxy.example.com"
--            & withTimeout 30
--            & withOnResponseMeta (\\meta -> pushToPrometheus meta.rateLimits)
-- @
defaultConfig :: ApiKey -> ClientConfig
defaultConfig key = ClientConfig
  { apiKey          = key
  , baseUrl         = Nothing
  , defaultHeaders  = Nothing
  , retryPolicy     = defaultRetryPolicy
  , timeout         = Nothing
  , betaFeatures    = Nothing
  , onResponseMeta  = \_ -> pure ()
  , onRetry         = \_ _ -> pure ()
  }

-- | Set the API base URL.
withBaseUrl :: Text -> ClientConfig -> ClientConfig
withBaseUrl x cfg = cfg { baseUrl = Just x }

-- | Set additional default headers for every request.
withDefaultHeaders :: [(ByteString, ByteString)] -> ClientConfig -> ClientConfig
withDefaultHeaders x cfg = cfg { defaultHeaders = Just x }

-- | Set a custom retry policy (replaces the default).
withRetryPolicy :: RetryPolicy -> ClientConfig -> ClientConfig
withRetryPolicy x cfg = cfg { retryPolicy = x }

-- | Set the request timeout in seconds.
withTimeout :: Int -> ClientConfig -> ClientConfig
withTimeout x cfg = cfg { timeout = Just x }

-- | Set beta feature strings for the @anthropic-beta@ header.
withBetaFeatures :: [Text] -> ClientConfig -> ClientConfig
withBetaFeatures x cfg = cfg { betaFeatures = Just x }

-- | Set the response metadata callback (rate limits, request ID).
--
-- See ADR-003.
withOnResponseMeta :: (ResponseMeta -> IO ()) -> ClientConfig -> ClientConfig
withOnResponseMeta f cfg = cfg { onResponseMeta = f }

-- | Set the retry callback.
--
-- Called before each retry attempt with the attempt number (1-based)
-- and delay in microseconds.
--
-- @
-- let config = defaultConfig "sk-my-key"
--            & withOnRetry (\\attempt delay ->
--                putStrLn $ "Retry " ++ show attempt ++ ", waiting " ++ show (delay `div` 1000) ++ "ms"
--              )
-- @
withOnRetry :: (Int -> Int -> IO ()) -> ClientConfig -> ClientConfig
withOnRetry f cfg = cfg { onRetry = f }

-- | Opaque client handle.
--
-- Holds the HTTP connection pool, configuration, and advisory rate limit state.
-- Constructors are not exported from @Anthropic.Client@. Use 'newClient',
-- 'closeClient', or 'withClient' for lifecycle management.
data AnthropicClient = AnthropicClient
  { clientConfig      :: !ClientConfig
  , clientManager     :: !Manager
  , clientRateLimits  :: !(TVar (Maybe RateLimitInfo))
  , clientTracer      :: !SomeTracer
  }

-- | Create a new client. The client maintains an HTTP connection pool
-- and rate limit state internally.
--
-- Must be closed with 'closeClient' when no longer needed, or use
-- 'withClient' for bracket-style lifecycle.
newClient :: ClientConfig -> IO AnthropicClient
newClient cfg = do
  mgr <- newTlsManager
  rlState <- newRateLimitState
  provider <- getGlobalTracerProvider
  tracer <- getTracer provider InstrumentationScope
    { scopeName       = "anthropic-client"
    , scopeVersion    = Nothing
    , scopeSchemaUrl  = Nothing
    , scopeAttributes = Nothing
    }
  pure AnthropicClient
    { clientConfig     = cfg
    , clientManager    = mgr
    , clientRateLimits = rlState
    , clientTracer     = tracer
    }

-- | Close a client, releasing its connection pool.
--
-- The @http-client@ 'Manager' is GC'd automatically, but calling this
-- signals intent and allows the connection pool to be released early.
closeClient :: AnthropicClient -> IO ()
closeClient _ = pure ()

-- | Bracket-style client lifecycle.
--
-- @
-- withClient (defaultConfig \"sk-my-key\") $ \\client -> do
--   result <- createMessage client req
--   ...
-- @
withClient :: ClientConfig -> (AnthropicClient -> IO a) -> IO a
withClient cfg = bracket (newClient cfg) closeClient

-- | What the client returns when an API call fails.
data ClientError
  = ApiErrorResponse !ApiError !(Maybe RateLimitInfo)
    -- ^ API returned a structured error. Rate limit info may be available.
  | NetworkError !HttpException
    -- ^ Connection failed, DNS resolution failed, TLS handshake failed.
  | TimeoutError
    -- ^ Request exceeded configured timeout.
  | DeserializationError !Text !ByteString
    -- ^ Response body could not be parsed. Includes the error message
    -- and the raw bytes for debugging.
  deriving stock (Show)

-- | Get the tracer for instrumentation.
getClientTracer :: AnthropicClient -> SomeTracer
getClientTracer (AnthropicClient { clientTracer = t }) = t

-- | Get the last-seen rate limit information.
--
-- Returns 'Nothing' before the first API call. Non-blocking (reads a TVar).
-- The values are advisory and inherently stale.
--
-- See ADR-003 for the observability design.
getRateLimits :: AnthropicClient -> IO (Maybe RateLimitInfo)
getRateLimits client = readRateLimits client.clientRateLimits
