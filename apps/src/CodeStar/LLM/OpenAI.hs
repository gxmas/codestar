module CodeStar.LLM.OpenAI
  ( newOpenAIClient
  , newCompatibleClient

    -- * Internals exposed for testing
  , toOAIMessage
  , cacheMarkerText
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import OpenAI.Client qualified as OAI
import OpenAI.Types qualified as OT

import CodeStar.LLM.Base

newOpenAIClient :: Text -> Text -> IO LlmClientDict
newOpenAIClient apiKey modelId =
  newCompatibleClient apiKey modelId "https://api.openai.com"

newCompatibleClient :: Text -> Text -> Text -> IO LlmClientDict
newCompatibleClient apiKey modelId baseUrl = do
  client <-
    OAI.newClient
      OT.ClientConfig
        { OT.apiKey = apiKey
        , OT.baseUrl = baseUrl
        , OT.timeoutMs = 120000
        }
  pure
    LlmClientDict
      { clientInfo = ClientInfo "openai" modelId
      , complete = doComplete client modelId
      , stream = doStream client modelId
      , countTokens = \_ -> pure (Right (TokenCount 0 0))
      }

doComplete :: OAI.OpenAIClient -> Text -> CompletionRequest -> IO (Either LlmError CompletionResponse)
doComplete client modelId req = do
  result <- OAI.createChatCompletion client (toOAIRequest modelId req)
  pure $ case result of
    Left err -> Left (fromClientError err)
    Right res -> Right (fromOAIResponse res)

doStream ::
  OAI.OpenAIClient ->
  Text ->
  CompletionRequest ->
  (CompletionEvent -> IO ()) ->
  IO (Either LlmError CompletionResponse)
doStream client modelId req onEvent = do
  result <- OAI.streamChatCompletion client (toOAIRequest modelId req) (handleChunk onEvent)
  pure $ case result of
    Left err -> Left (fromClientError err)
    Right () ->
      Right
        CompletionResponse
          { responseContent = []
          , stopReason = EndTurn
          , usage = TokenCount 0 0
          }

handleChunk :: (CompletionEvent -> IO ()) -> OT.StreamChunk -> IO ()
handleChunk onEvent chunk =
  mapM_ (handleChoice onEvent) chunk.choices

handleChoice :: (CompletionEvent -> IO ()) -> OT.StreamChoice -> IO ()
handleChoice onEvent choice = case choice.delta of
  OT.DeltaText t -> onEvent (EventToken t)
  OT.DeltaToolCall _ -> pure ()
  OT.DeltaEmpty -> pure ()

-- --------------------------------------------------------------------
-- Request translation
-- --------------------------------------------------------------------

toOAIRequest :: Text -> CompletionRequest -> OT.ChatRequest
toOAIRequest modelId req =
  (OT.chatRequest modelId (map toOAIMessage req.messages))
    { OT.tools = case req.tools of [] -> Nothing; ts -> Just (map toOAITool ts)
    , OT.maxTokens = Just req.maxTokens
    , OT.temperature = req.temperature
    , OT.topP = req.topP
    }

toOAIMessage :: Message -> OT.Message
toOAIMessage msg = case msg.role of
  System -> OT.systemMessage (textOf msg.content)
  User -> OT.userMessage (textOf msg.content)
  Assistant -> OT.assistantMessage (textOf msg.content)
 where
  -- Skip the Anthropic cache_control sentinel; OpenAI has no equivalent
  -- block-level cache directive, so the marker is dropped silently.
  textOf blocks = mconcat [t | TextContent t <- blocks, t /= cacheMarkerText]

cacheMarkerText :: Text
cacheMarkerText = "\x0000cache_control"

toOAITool :: Value -> OT.Tool
toOAITool schema =
  OT.Tool
    { OT.function =
        OT.FunctionDef
          { OT.name = ""
          , OT.description = Nothing
          , OT.parameters = schema
          }
    }

-- --------------------------------------------------------------------
-- Response translation
-- --------------------------------------------------------------------

fromOAIResponse :: OT.ChatResponse -> CompletionResponse
fromOAIResponse res =
  CompletionResponse
    { responseContent = concatMap fromChoice res.choices
    , stopReason = maybe EndTurn fromFinish (head' res.choices >>= (.finishReason))
    , usage = maybe (TokenCount 0 0) fromUsage res.usage
    }

fromChoice :: OT.Choice -> [Content]
fromChoice choice = case choice.message.content of
  OT.TextContent t -> [TextContent t]
  _ -> []

fromFinish :: OT.FinishReason -> StopReason
fromFinish = \case
  OT.FinishStop -> EndTurn
  OT.FinishLength -> MaxTokens
  OT.FinishToolCalls -> ToolUse
  OT.FinishOther _ -> EndTurn

fromUsage :: OT.Usage -> TokenCount
fromUsage u =
  TokenCount
    { inputTokens = fromIntegral u.promptTokens
    , outputTokens = fromIntegral u.completionTokens
    }

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

fromClientError :: OT.ClientError -> LlmError
fromClientError = \case
  OT.ApiErrorResponse status detail ->
    if status == 429
      then RateLimited 60.0
      else
        if status == 401
          then AuthenticationFailed detail.message
          else ProviderError detail.message
  OT.NetworkError msg -> NetworkError msg
  OT.TimeoutError -> NetworkError "Request timed out"
  OT.DeserializationError msg -> ProviderError ("Deserialization: " <> msg)
