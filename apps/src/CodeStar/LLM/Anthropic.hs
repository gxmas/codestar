module CodeStar.LLM.Anthropic
  ( newAnthropicClient

    -- * Internals exposed for testing
  , toAnthropicMessage
  , toAnthropicContent
  , cacheMarkerText
  ) where

import Data.Text (Text)
import Data.Text qualified as Text

import Data.Aeson (Result (..), Value, fromJSON)

import Anthropic.Client qualified as A
import Anthropic.Client.Config qualified as AC
import Anthropic.Protocol.Message qualified as AP
import Anthropic.Protocol.TokenCount qualified as ATC
import Anthropic.Protocol.Tool qualified as APT
import Anthropic.Types qualified as AT

import CodeStar.LLM.Base

newAnthropicClient :: Text -> Text -> IO LlmClientDict
newAnthropicClient apiKey modelId = do
  client <- A.newClient (AC.defaultConfig (AT.ApiKey apiKey))
  let model = AT.ModelId modelId
  pure
    LlmClientDict
      { clientInfo = ClientInfo "anthropic" modelId
      , complete = doComplete client model
      , stream = doStream client model
      , countTokens = doCountTokens client model
      }

-- --------------------------------------------------------------------
-- Complete
-- --------------------------------------------------------------------

doComplete :: A.AnthropicClient -> AT.ModelId -> CompletionRequest -> IO (Either LlmError CompletionResponse)
doComplete client model req = do
  result <- A.createMessage client (toAnthropicRequest model req)
  pure $ case result of
    Left err -> Left (fromClientError err)
    Right res -> Right (fromAnthropicResponse res)

-- --------------------------------------------------------------------
-- Stream
-- --------------------------------------------------------------------

doStream ::
  A.AnthropicClient ->
  AT.ModelId ->
  CompletionRequest ->
  (CompletionEvent -> IO ()) ->
  IO (Either LlmError CompletionResponse)
doStream client model req onEvent = do
  let handler =
        A.defaultEventHandler
          { A.onContentBlockDelta = \_ delta -> case delta of
              A.TextDelta t -> onEvent (EventToken t)
              _ -> pure ()
          }
  result <- A.streamMessagesWith client (toAnthropicRequest model req) handler
  pure $ case result of
    Left err -> Left (fromClientError err)
    Right res -> Right (fromAnthropicResponse res)

-- --------------------------------------------------------------------
-- Count Tokens
-- --------------------------------------------------------------------

doCountTokens :: A.AnthropicClient -> AT.ModelId -> [Message] -> IO (Either LlmError TokenCount)
doCountTokens client model msgs = do
  let nonSystemMsgs = filter (\m -> m.role /= System) msgs
      req = ATC.tokenCountRequest model (map toAnthropicMessage nonSystemMsgs)
  result <- A.countTokens client req
  pure $ case result of
    Left err -> Left (fromClientError err)
    Right res ->
      Right
        TokenCount
          { inputTokens         = fromIntegral res.inputTokens
          , outputTokens        = 0
          , cacheCreationTokens = 0
          , cacheReadTokens     = 0
          }

-- --------------------------------------------------------------------
-- Request translation
-- --------------------------------------------------------------------

toAnthropicRequest :: AT.ModelId -> CompletionRequest -> AP.MessageRequest
toAnthropicRequest model req =
  let systemText = mergeSystemPrompt req
      nonSystemMsgs = filter (\m -> m.role /= System) req.messages
      base = AP.messageRequest model (map toAnthropicMessage nonSystemMsgs) req.maxTokens
   in base
        { AP.system = AT.SimpleSystem <$> systemText
        , AP.temperature = req.temperature
        , AP.topP = req.topP
        , AP.tools = parseToolDefs req.tools
        }

parseToolDefs :: [Value] -> Maybe [APT.ToolDefinition]
parseToolDefs [] = Nothing
parseToolDefs vs = Just (map decodeOne vs)
 where
  decodeOne v = case fromJSON v of
    Success td -> td
    Error msg -> error ("CodeStar.LLM.Anthropic: invalid tool schema: " <> msg)

mergeSystemPrompt :: CompletionRequest -> Maybe Text
mergeSystemPrompt req =
  let fromMessages = [t | Message System cs <- req.messages, TextContent t <- cs]
      parts = maybe [] (: []) req.systemPrompt <> fromMessages
   in case parts of
        [] -> Nothing
        _ -> Just (Text.intercalate "\n\n" parts)

toAnthropicMessage :: Message -> AP.Message
toAnthropicMessage msg = case msg.role of
  User -> AP.userMessage (toAnthropicContent msg.content)
  Assistant -> AP.assistantMessage (toAnthropicContent msg.content)
  System -> error "System messages should be filtered before toAnthropicMessage"

toAnthropicContent :: [Content] -> AT.MessageContent
toAnthropicContent blocks =
  case translateBlocks blocks of
    [AT.TextContent tb]
      | tb.cacheControl == Nothing && tb.citations == Nothing ->
          AT.TextMessage tb.text
    bs -> AT.BlockMessage bs

{- | Translate codestar Content blocks to Anthropic ContentBlocks, consuming
cache_control sentinels: a sentinel attaches cache_control to the next
emitted block instead of becoming its own text block. Trailing markers
with no following block are dropped.
-}
translateBlocks :: [Content] -> [AT.ContentBlock]
translateBlocks = go False
 where
  go _ [] = []
  go _ (c : rest) | isCacheMarker c = go True rest
  go pending (c : rest) =
    let bs = toAnthropicBlock c
     in case bs of
          [] -> go pending rest
          (b : tl) ->
            let b' = if pending then setCacheControl b else b
             in b' : tl ++ go False rest

isCacheMarker :: Content -> Bool
isCacheMarker (TextContent t) = t == cacheMarkerText
isCacheMarker _ = False

cacheMarkerText :: Text
cacheMarkerText = "\x0000cache_control"

setCacheControl :: AT.ContentBlock -> AT.ContentBlock
setCacheControl (AT.TextContent (AT.TextBlock t cs _)) =
  AT.TextContent (AT.TextBlock t cs (Just ephemeral))
setCacheControl (AT.ToolUseContent (AT.ToolUseBlock i n a _)) =
  AT.ToolUseContent (AT.ToolUseBlock i n a (Just ephemeral))
setCacheControl (AT.ToolResultContent (AT.ToolResultBlock i c e _)) =
  AT.ToolResultContent (AT.ToolResultBlock i c e (Just ephemeral))
setCacheControl other = other

ephemeral :: AT.CacheControl
ephemeral = AT.CacheControl{AT.ttl = Nothing}

toAnthropicBlock :: Content -> [AT.ContentBlock]
toAnthropicBlock (TextContent t) =
  [AT.TextContent (AT.TextBlock t Nothing Nothing)]
toAnthropicBlock (ToolUseContent tc) =
  [AT.ToolUseContent (AT.ToolUseBlock (unToolCallId tc.toolCallId) (unToolName tc.toolName) tc.arguments Nothing)]
toAnthropicBlock (ToolResultContent tr) =
  [AT.ToolResultContent (AT.ToolResultBlock (unToolCallId tr.toolResultId) resultContent errFlag Nothing)]
 where
  -- Anthropic rejects empty tool_result content with a misleading
  -- "tool_use without tool_result" error. Substitute a placeholder.
  body = if Text.null tr.resultBody then "(empty)" else tr.resultBody
  resultContent = Just (AT.ToolResultText body)
  errFlag = if tr.isError then Just True else Nothing

-- --------------------------------------------------------------------
-- Response translation
-- --------------------------------------------------------------------

fromAnthropicResponse :: AP.MessageResponse -> CompletionResponse
fromAnthropicResponse res =
  CompletionResponse
    { responseContent = concatMap fromAnthropicBlock res.content
    , stopReason = fromAnthropicStopReason res.stopReason
    , usage =
        TokenCount
          { inputTokens         = fromIntegral res.usage.inputTokens
          , outputTokens        = fromIntegral res.usage.outputTokens
          , cacheCreationTokens = maybe 0 fromIntegral res.usage.cacheCreationInputTokens
          , cacheReadTokens     = maybe 0 fromIntegral res.usage.cacheReadInputTokens
          }
    }

fromAnthropicBlock :: AT.ContentBlock -> [Content]
fromAnthropicBlock (AT.TextContent tb) = [TextContent tb.text]
fromAnthropicBlock (AT.ToolUseContent tu) =
  [ ToolUseContent
      ToolCall
        { toolCallId = ToolCallId tu.id
        , toolName = ToolName tu.name
        , arguments = tu.input
        }
  ]
fromAnthropicBlock _ = []

fromAnthropicStopReason :: Maybe AT.StopReason -> StopReason
fromAnthropicStopReason Nothing = EndTurn
fromAnthropicStopReason (Just r) = case r of
  AT.EndTurn -> EndTurn
  AT.ToolUse -> ToolUse
  AT.MaxTokens -> MaxTokens
  AT.StopSequence -> StopSequence
  _ -> EndTurn

-- --------------------------------------------------------------------
-- Error translation
-- --------------------------------------------------------------------

fromClientError :: AC.ClientError -> LlmError
fromClientError (AC.ApiErrorResponse apiErr _) =
  case apiErr.errorType of
    AT.RateLimitError -> RateLimited 60.0
    AT.AuthenticationError -> AuthenticationFailed apiErr.errorMessage
    AT.RequestTooLargeError -> ContextTooLong 0 0
    AT.InvalidRequestError -> InvalidRequest apiErr.errorMessage
    AT.OverloadedError -> RateLimited 30.0
    _ -> ProviderError apiErr.errorMessage
fromClientError (AC.NetworkError _) = NetworkError "Network error"
fromClientError AC.TimeoutError = NetworkError "Request timed out"
fromClientError (AC.DeserializationError msg _)
  | "401" `Text.isInfixOf` msg || "403" `Text.isInfixOf` msg =
      AuthenticationFailed ("HTTP auth error: " <> msg)
  | "429" `Text.isInfixOf` msg =
      RateLimited 30.0
  | otherwise =
      ProviderError ("Deserialization: " <> msg)
