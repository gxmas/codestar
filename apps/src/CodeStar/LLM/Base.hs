{- |
= CodeStar.LLM.Base — provider-agnostic LLM interface

This module defines the __common vocabulary__ shared by every LLM provider
adapter (@Anthropic@, @OpenAI@, @Ollama@).  No provider-specific types
appear here; callers work exclusively against 'LlmClientDict'.

== Design: record-of-functions

'LlmClientDict' is a __dictionary-passing__ pattern (sometimes called
"record of closures" or "poor-man's type class").  Instead of a type
class with a provider type parameter, we pass a concrete record whose
fields are the operations.  The advantages:

  * Adapters can wrap each other at runtime without changing types
    (see 'withRetry', 'withFallback', 'withDefaults').
  * Testing is trivial — build a mock record with pure stubs.
  * Configuration-driven provider selection: the same @AgentEnv@ field
    works for any provider without generics or existentials.

== Streaming model

'stream' delivers 'CompletionEvent' values to a callback as they arrive,
then returns the full 'CompletionResponse' when the stream ends.  This
lets the REPL print tokens as they stream while the agent loop waits for
the complete response to process tool calls.

== Wrapping pipeline

@
  buildClientFromEntry
       │  newAnthropicClient / newOpenAIClient
       ▼
  raw LlmClientDict
       │  withDefaults (temperature, topP, maxTokens from config)
       ▼
  LlmClientDict
       │  withRetry (RecoveryEngine, onRetry telemetry hook)
       ▼
  final LlmClientDict stored in AgentEnv
@
-}
module CodeStar.LLM.Base
  ( -- * Messages
    Role (..)
  , Message (..)
  , Content (..)

    -- * Tool Identifiers
  , ToolName (..)
  , ToolCallId (..)

    -- * Tool Calls
  , ToolCall (..)
  , ToolResult (..)

    -- * Completion
  , CompletionRequest (..)
  , CompletionResponse (..)
  , StopReason (..)
  , TokenCount (..)

    -- * Streaming
  , CompletionEvent (..)

    -- * Errors
  , LlmError (..)

    -- * Client Interface
  , ClientInfo (..)
  , LlmClientDict (..)

    -- * Model Resolution
  , withFallback
  , withDefaults
  , withRetry
  , buildClientFromEntry
  ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Resilience.Core (RecoveryEngine, withRecoveryEither)

import CodeStar.Config (ModelEntry (..), ApiKey (..))

-- --------------------------------------------------------------------
-- Messages
-- --------------------------------------------------------------------

-- | Conversation participant role.  'System' messages carry the system
-- prompt; most providers translate them into a top-level @system@ field
-- rather than a conversation turn.
data Role = User | Assistant | System
  deriving stock (Eq, Show, Generic)

-- | A single block of message content.  A message may contain multiple
-- blocks (e.g. text followed by a tool call).
data Content
  = TextContent Text             -- ^ A text token or full text block.
  | ToolUseContent ToolCall      -- ^ The model wants to call a tool.
  | ToolResultContent ToolResult -- ^ The result of a previous tool call.
  deriving stock (Eq, Show, Generic)

data Message = Message
  { role :: Role
  , content :: [Content]
  }
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Tool Identifiers
-- --------------------------------------------------------------------

newtype ToolName = ToolName {unToolName :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

newtype ToolCallId = ToolCallId {unToolCallId :: Text}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- --------------------------------------------------------------------
-- Tool Calls
-- --------------------------------------------------------------------

data ToolCall = ToolCall
  { toolCallId :: ToolCallId
  , toolName :: ToolName
  , arguments :: Value
  }
  deriving stock (Eq, Show, Generic)

data ToolResult = ToolResult
  { toolResultId :: ToolCallId
  , resultBody :: Text
  , isError :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Completion
-- --------------------------------------------------------------------

data CompletionRequest = CompletionRequest
  { messages :: [Message]
  , systemPrompt :: Maybe Text
  , tools :: [Value]
  , maxTokens :: Int
  , temperature :: Maybe Double
  , topP :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)

-- | Why the model stopped generating.
-- The agent loop branches on this: 'ToolUse' means process tool calls
-- and continue; 'EndTurn' means the model is done.
data StopReason = EndTurn | ToolUse | MaxTokens | StopSequence
  deriving stock (Eq, Show, Generic)

-- | Token usage for a single API call.  Cache fields are Anthropic-specific
-- (prompt caching); they are zero for providers that do not support it.
data TokenCount = TokenCount
  { inputTokens         :: Word64 -- ^ Tokens charged as fresh input.
  , outputTokens        :: Word64 -- ^ Tokens generated by the model.
  , cacheCreationTokens :: Word64 -- ^ Tokens written to the prompt cache.
  , cacheReadTokens     :: Word64 -- ^ Tokens served from the prompt cache (cheaper).
  }
  deriving stock (Eq, Show, Generic)

data CompletionResponse = CompletionResponse
  { responseContent :: [Content]
  , stopReason :: StopReason
  , usage :: TokenCount
  }
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Streaming
-- --------------------------------------------------------------------

-- | An event emitted during a streaming completion.
-- The agent loop receives these via the callback passed to 'stream';
-- 'EventToken' values are forwarded to the UI immediately.
data CompletionEvent
  = EventToken Text                    -- ^ A streamed text token.
  | EventToolCallStart ToolCall        -- ^ A tool call is starting (partial).
  | EventComplete CompletionResponse   -- ^ Stream finished; full response available.
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Errors
-- --------------------------------------------------------------------

-- | Errors returned by the LLM client.  'RateLimited' and 'NetworkError'
-- are considered transient and are retried by 'withRetry'; all others are
-- fatal for the current turn.
data LlmError
  = RateLimited Double         -- ^ Rate limited; value is retry-after in seconds.
  | AuthenticationFailed Text  -- ^ Invalid API key or token.
  | ContextTooLong Int Int     -- ^ Request exceeds the model's context window.
  | ContentFiltered Text       -- ^ Response blocked by provider content policy.
  | InvalidRequest Text        -- ^ Malformed request (bug in the adapter layer).
  | ProviderError Text         -- ^ Generic provider-side error.
  | NetworkError Text          -- ^ HTTP or connection failure.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- --------------------------------------------------------------------
-- Client Interface (record-of-functions / dictionary passing)
-- --------------------------------------------------------------------

data ClientInfo = ClientInfo
  { providerName :: Text
  , modelId :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | The LLM client interface as a record of functions.
-- Every provider adapter produces one of these; wrappers like 'withRetry'
-- and 'withDefaults' return a modified copy.
data LlmClientDict = LlmClientDict
  { clientInfo  :: ClientInfo
  -- ^ Identifies the provider and model for logging and telemetry.
  , complete    :: CompletionRequest -> IO (Either LlmError CompletionResponse)
  -- ^ Send a request and wait for the full response (no streaming).
  , stream      :: CompletionRequest -> (CompletionEvent -> IO ()) -> IO (Either LlmError CompletionResponse)
  -- ^ Send a request and deliver tokens to the callback as they arrive,
  --   then return the complete response.
  , countTokens :: [Message] -> IO (Either LlmError TokenCount)
  -- ^ Estimate the token count for a message list without generating output.
  --   Used for compaction decisions.
  }

{- | Wrap a client to inject default parameters into every CompletionRequest.
Spec values take precedence over whatever the caller sets in the request.
-}
withDefaults :: Maybe Double -> Maybe Double -> Maybe Int -> LlmClientDict -> LlmClientDict
withDefaults temp topp maxtok client =
  client
    { complete = client.complete . applyDefaults
    , stream = \req cb -> client.stream (applyDefaults req) cb
    , countTokens = client.countTokens
    }
 where
  applyDefaults :: CompletionRequest -> CompletionRequest
  applyDefaults req =
    CompletionRequest
      { messages = req.messages
      , systemPrompt = req.systemPrompt
      , tools = req.tools
      , maxTokens = fromMaybe req.maxTokens maxtok
      , temperature = temp <|> req.temperature
      , topP = topp <|> req.topP
      }

-- | Build a client from a config entry, applying 'withDefaults' for any
-- per-model overrides (temperature, topP, maxTokens) that were set in the
-- TOML config.
buildClientFromEntry :: ModelEntry -> (Text -> Text -> IO LlmClientDict) -> IO LlmClientDict
buildClientFromEntry entry factory = do
  let ApiKey key = entry.meApiKey
  client <- factory key entry.meModel
  pure (withDefaults entry.meTemperature entry.meTopP entry.meMaxTokens client)

{- | Wrap a client with a fallback: on Transient or NetworkError,
retry the same request once with the fallback client.
-}
withFallback :: LlmClientDict -> LlmClientDict -> LlmClientDict
withFallback primary fallback =
  LlmClientDict
    { clientInfo = primary.clientInfo
    , complete = \req -> primary.complete req >>= tryFallback (fallback.complete req)
    , stream = \req cb -> primary.stream req cb >>= tryFallback (fallback.stream req cb)
    , countTokens = \msgs -> primary.countTokens msgs >>= tryFallback (fallback.countTokens msgs)
    }
 where
  tryFallback retry (Left (RateLimited _)) = retry
  tryFallback retry (Left (NetworkError _)) = retry
  tryFallback _ result = pure result

{- | Wrap a client with retry and exponential backoff via the resilience engine.
Transient errors (RateLimited, NetworkError) are retried according to the
engine's policy. RateLimited honours the server-supplied retry-after delay.

The @onRetry@ callback is invoked on each retry attempt with the error that
triggered it and the 0-indexed attempt number.
-}
withRetry :: RecoveryEngine -> (LlmError -> Int -> IO ()) -> LlmClientDict -> LlmClientDict
withRetry engine onRetry client =
  client
    { complete = \req -> recover "llm.complete" (client.complete req)
    , stream = \req cb -> recover "llm.stream" (client.stream req cb)
    , countTokens = \msgs -> recover "llm.countTokens" (client.countTokens msgs)
    }
 where
  recover :: Text -> IO (Either LlmError a) -> IO (Either LlmError a)
  recover opName action = do
    attemptRef <- newIORef (0 :: Int)
    let instrumented = do
          attempt <- readIORef attemptRef
          result <- action
          case result of
            Left err | isTransient err -> do
              onRetry err attempt
              writeIORef attemptRef (attempt + 1)
              pure (Left err)
            _ -> pure result
    withRecoveryEither engine opName isTransient retryDelay instrumented

  isTransient (RateLimited _) = True
  isTransient (NetworkError _) = True
  isTransient _ = False

  retryDelay :: LlmError -> Maybe NominalDiffTime
  retryDelay (RateLimited secs) = Just (realToFrac secs)
  retryDelay _ = Nothing
