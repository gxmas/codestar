-- | Re-export module for the Anthropic client SDK.
--
-- A single @import Anthropic.Client@ gives consumers everything
-- needed to interact with the Anthropic Messages API.
--
-- @
-- import Anthropic.Client
--
-- main :: IO ()
-- main = withClient (defaultConfig \"sk-my-key\") $ \\client -> do
--   result <- createMessage client $
--     messageRequest \"claude-sonnet-4-20250514\" [userMessage \"Hello!\"] 1024
--   case result of
--     Right msg -> mapM_ print msg.content
--     Left err  -> print err
-- @
module Anthropic.Client
  ( -- * Client Setup
    AnthropicClient
  , ClientConfig (..)
  , defaultConfig
  , RetryPolicy (..)
  , defaultRetryPolicy
  , newClient
  , closeClient
  , withClient

    -- ** ClientConfig Setters
  , withBaseUrl
  , withDefaultHeaders
  , withRetryPolicy
  , withTimeout
  , withBetaFeatures
  , withOnResponseMeta
  , withOnRetry

    -- * Messages
  , createMessage
  , countTokens

    -- * Streaming
  , streamMessages
  , streamMessagesWith
  , EventHandler (..)
  , defaultEventHandler

    -- * Models
  , listModels
  , getModel

    -- * Batches
  , createBatch
  , getBatch
  , listBatches
  , cancelBatch
  , deleteBatch
  , getBatchResults

    -- * Rate Limit Observability
  , getRateLimits

    -- * Errors
  , ClientError (..)

    -- * Re-exports from anthropic-types
  , module Anthropic.Types

    -- * Re-exports from anthropic-protocol
  , module Anthropic.Protocol
  ) where

import Anthropic.Types
import Anthropic.Protocol

import Anthropic.Client.Config
import Anthropic.Client.Messages
import Anthropic.Client.Streaming
import Anthropic.Client.Models
import Anthropic.Client.Batches
