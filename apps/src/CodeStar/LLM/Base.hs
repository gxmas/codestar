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
  , ModelResolver
  , trivialResolver
  , withFallback
  , withDefaults
  , withRetry
  , buildResolver
  , buildClientFromEntry
  ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad (forM)
import Data.Aeson (Value)
import Data.Hashable (Hashable)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Resilience.Core (RecoveryEngine, withRecoveryEither)

import CodeStar.Config (ModelSpec (..), ModelEntry (..), ApiKey (..))
import CodeStar.Types (ModelRole)

-- --------------------------------------------------------------------
-- Messages
-- --------------------------------------------------------------------

data Role = User | Assistant | System
  deriving stock (Eq, Show, Generic)

data Content
  = TextContent Text
  | ToolUseContent ToolCall
  | ToolResultContent ToolResult
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

data StopReason = EndTurn | ToolUse | MaxTokens | StopSequence
  deriving stock (Eq, Show, Generic)

data TokenCount = TokenCount
  { inputTokens         :: Word64
  , outputTokens        :: Word64
  , cacheCreationTokens :: Word64
  , cacheReadTokens     :: Word64
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

data CompletionEvent
  = EventToken Text
  | EventToolCallStart ToolCall
  | EventComplete CompletionResponse
  deriving stock (Eq, Show, Generic)

-- --------------------------------------------------------------------
-- Errors
-- --------------------------------------------------------------------

data LlmError
  = RateLimited Double
  | AuthenticationFailed Text
  | ContextTooLong Int Int
  | ContentFiltered Text
  | InvalidRequest Text
  | ProviderError Text
  | NetworkError Text
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

data LlmClientDict = LlmClientDict
  { clientInfo :: ClientInfo
  , complete :: CompletionRequest -> IO (Either LlmError CompletionResponse)
  , stream :: CompletionRequest -> (CompletionEvent -> IO ()) -> IO (Either LlmError CompletionResponse)
  , countTokens :: [Message] -> IO (Either LlmError TokenCount)
  }

-- --------------------------------------------------------------------
-- Model Resolution
-- --------------------------------------------------------------------

type ModelResolver = ModelRole -> LlmClientDict

trivialResolver :: LlmClientDict -> ModelResolver
trivialResolver client = const client

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

{- | Build a ModelResolver from a role→spec map and a client factory.
Creates one base client per unique modelId to reuse connections, then
wraps each with the role's parameter defaults baked in.
-}
buildResolver ::
  Map ModelRole ModelSpec ->
  (Text -> IO LlmClientDict) ->
  IO ModelResolver
buildResolver roleMap factory = do
  let pairs = Map.toList roleMap
      modelNames = nub (map ((.modelName) . snd) pairs)
  baseClients <- fmap Map.fromList $
    forM modelNames $
      \m -> (m,) <$> factory m
  let fallback = snd (Map.findMin baseClients)
      clientMap =
        Map.fromList
          [ ( role
            , withDefaults
                spec.temperature
                spec.topP
                spec.maxTokens
                (baseClients Map.! spec.modelName)
            )
          | (role, spec) <- pairs
          ]
  pure $ \role -> Map.findWithDefault fallback role clientMap

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
