{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal: Shared request execution logic.
--
-- __This module is not part of the public API.__
module Anthropic.Client.Internal.Execute
  ( -- * Request Execution
    executeJsonPost
  , executeJsonGet
  , executeDelete

    -- * Error Parsing (internal)
  , ErrorEnvelope (..)
  ) where

import Control.Exception (catch)
import Data.Aeson (FromJSON(..), ToJSON, (.:), eitherDecode, withObject)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( HttpException(..)
  , Response
  , httpLbs, responseBody, responseHeaders, responseStatus
  )
import Network.HTTP.Types (statusCode, statusIsSuccessful)

import Anthropic.Types

import Anthropic.Client.Config
import Anthropic.Client.Internal.Http
import Anthropic.Client.Internal.RateLimit
import Anthropic.Client.Internal.Retry

-- | Anthropic API error envelope: @{"type": "error", "error": {...}}@
newtype ErrorEnvelope = ErrorEnvelope ApiError

instance FromJSON ErrorEnvelope where
  parseJSON = withObject "ErrorEnvelope" $ \o ->
    ErrorEnvelope <$> o .: "error"

-- | Execute a JSON POST request and decode the response.
executeJsonPost
  :: (ToJSON req, FromJSON resp)
  => AnthropicClient
  -> BS8.ByteString       -- ^ Path (e.g., "/v1/messages")
  -> req                  -- ^ Request body
  -> IO (Either ClientError resp)
executeJsonPost client path body = executeRequest client $ do
  let cfg = client.clientConfig
      base = fromMaybe baseUrl (cfg.baseUrl)
      req = buildRequest
              (cfg.apiKey) base path
              (cfg.defaultHeaders) (cfg.betaFeatures) (cfg.timeout)
              body
  resp <- httpLbs req (client.clientManager)
  processJsonResponse client resp

-- | Execute a GET request and decode the response.
executeJsonGet
  :: FromJSON resp
  => AnthropicClient
  -> BS8.ByteString       -- ^ Path with query string
  -> IO (Either ClientError resp)
executeJsonGet client path = executeRequest client $ do
  let cfg = client.clientConfig
      base = fromMaybe baseUrl (cfg.baseUrl)
      req = buildGetRequest
              (cfg.apiKey) base path
              (cfg.defaultHeaders) (cfg.betaFeatures) (cfg.timeout)
  resp <- httpLbs req (client.clientManager)
  processJsonResponse client resp

-- | Execute a DELETE request.
executeDelete
  :: FromJSON resp
  => AnthropicClient
  -> BS8.ByteString
  -> IO (Either ClientError resp)
executeDelete client path = executeRequest client $ do
  let cfg = client.clientConfig
      base = fromMaybe baseUrl (cfg.baseUrl)
      req = buildDeleteRequest
              (cfg.apiKey) base path
              (cfg.defaultHeaders) (cfg.betaFeatures) (cfg.timeout)
  resp <- httpLbs req (client.clientManager)
  processJsonResponse client resp

-- | Wrap execution with retry logic and HttpException handling.
executeRequest :: AnthropicClient -> IO (Either ClientError a) -> IO (Either ClientError a)
executeRequest client action =
  withRetry (client.clientConfig).retryPolicy (client.clientConfig).onRetry action
    `catch` \(e :: HttpException) ->
      pure (Left (NetworkError e))

-- | Process a JSON response: extract rate limits, fire callback,
-- decode body or build error.
processJsonResponse
  :: FromJSON a
  => AnthropicClient
  -> Response LBS.ByteString
  -> IO (Either ClientError a)
processJsonResponse client resp = do
  let hdrs = responseHeaders resp
      status = responseStatus resp
      rawBody = responseBody resp
      mRateLimits = parseRateLimitHeaders hdrs
      mRequestId  = fmap (RequestId . TE.decodeUtf8) (lookup (CI.mk "request-id") hdrs)

  -- Update rate limit state and fire callback
  case (mRateLimits, mRequestId) of
    (Just rl, Just rid) -> do
      updateRateLimits (client.clientRateLimits) rl
      (client.clientConfig).onResponseMeta ResponseMeta
        { rateLimits = rl
        , requestId  = rid
        }
    (Just rl, Nothing) ->
      updateRateLimits (client.clientRateLimits) rl
    _ -> pure ()

  if statusIsSuccessful status
    then case eitherDecode rawBody of
      Right val -> pure (Right val)
      Left err  -> pure (Left (DeserializationError (T.pack err) (LBS.toStrict rawBody)))
    else
      -- Try to parse API error envelope: {"type":"error","error":{...}}
      case eitherDecode rawBody of
        Right (ErrorEnvelope apiErr) ->
          pure (Left (ApiErrorResponse apiErr mRateLimits))
        Left _ ->
          -- Non-JSON error body
          pure (Left (DeserializationError
            (T.pack $ "HTTP " <> show (statusCode status))
            (LBS.toStrict rawBody)))
