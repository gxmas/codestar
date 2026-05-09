-- | Message creation and token counting operations.
module Anthropic.Client.Messages
  ( -- * Messages
    createMessage

    -- * Token Counting
  , countTokens
  ) where

import Prelude hiding (log)

import Anthropic.Client.Config (AnthropicClient, ClientError)
import Anthropic.Client.Internal.Execute (executeJsonPost)
import Anthropic.Protocol.Message (MessageRequest, MessageResponse)
import Anthropic.Protocol.TokenCount (TokenCountRequest, TokenCountResponse)
import Telemetry.Core (withSpan, AttributeValue(..))

-- | Create a message (non-streaming).
--
-- @
-- result <- createMessage client $
--   messageRequest \"claude-sonnet-4-20250514\" [userMessage \"Hello!\"] 1024
-- case result of
--   Right msg -> mapM_ print msg.content
--   Left err  -> print err
-- @
createMessage
  :: AnthropicClient
  -> MessageRequest
  -> IO (Either ClientError MessageResponse)
createMessage client req =
  withSpan "gen_ai.chat" [("gen_ai.system", TextValue "anthropic")] $
    executeJsonPost client "/v1/messages" req

-- | Count input tokens for a request without creating a message.
countTokens
  :: AnthropicClient
  -> TokenCountRequest
  -> IO (Either ClientError TokenCountResponse)
countTokens client req =
  withSpan "gen_ai.token_count" [("gen_ai.system", TextValue "anthropic")] $
    executeJsonPost client "/v1/messages/count_tokens" req
