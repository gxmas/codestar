module OpenAI.Types
  ( -- * Messages
    Role (..)
  , MessageContent (..)
  , TextPart (..)
  , ToolCallPart (..)
  , ToolResultPart (..)
  , Message (..)
  , userMessage
  , assistantMessage
  , systemMessage
  , toolResultMessage

    -- * Tools
  , FunctionDef (..)
  , Tool (..)
  , ToolChoice (..)
  , ToolCallChunk (..)
  , ToolCall (..)

    -- * Request / Response
  , ChatRequest (..)
  , chatRequest
  , Choice (..)
  , Usage (..)
  , ChatResponse (..)
  , FinishReason (..)

    -- * Streaming
  , DeltaContent (..)
  , StreamChoice (..)
  , StreamChunk (..)

    -- * Errors
  , ApiErrorDetail (..)
  , ApiError (..)
  , ClientError (..)

    -- * Config
  , ClientConfig (..)
  , defaultOpenAIConfig
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)


-- --------------------------------------------------------------------
-- Messages
-- --------------------------------------------------------------------

data Role = User | Assistant | System | ToolRole
  deriving stock (Eq, Show, Generic)

instance ToJSON Role where
  toJSON = \case
    User      -> String "user"
    Assistant -> String "assistant"
    System    -> String "system"
    ToolRole  -> String "tool"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user"      -> pure User
    "assistant" -> pure Assistant
    "system"    -> pure System
    "tool"      -> pure ToolRole
    other       -> fail $ "Unknown role: " <> show other

data TextPart = TextPart
  { text :: Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON TextPart where
  toJSON p = object ["type" .= ("text" :: Text), "text" .= p.text]

instance FromJSON TextPart where
  parseJSON = withObject "TextPart" $ \o -> TextPart <$> o .: "text"

data ToolCallPart = ToolCallPart
  { id       :: Text
  , name     :: Text
  , arguments :: Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON ToolCallPart where
  toJSON p = object
    [ "type" .= ("tool_use" :: Text)
    , "id"   .= p.id
    , "name" .= p.name
    , "input" .= p.arguments
    ]

instance FromJSON ToolCallPart where
  parseJSON = withObject "ToolCallPart" $ \o -> ToolCallPart
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "input"

data ToolResultPart = ToolResultPart
  { toolCallId :: Text
  , content    :: Text
  , isError    :: Bool
  } deriving stock (Eq, Show, Generic)

instance ToJSON ToolResultPart where
  toJSON p = object
    [ "type"         .= ("tool_result" :: Text)
    , "tool_call_id" .= p.toolCallId
    , "content"      .= p.content
    ]

instance FromJSON ToolResultPart where
  parseJSON = withObject "ToolResultPart" $ \o -> ToolResultPart
    <$> o .: "tool_call_id"
    <*> o .: "content"
    <*> pure False

data MessageContent
  = TextContent !Text
  | PartsContent ![Value]
  deriving stock (Eq, Show, Generic)

instance ToJSON MessageContent where
  toJSON (TextContent t)  = toJSON t
  toJSON (PartsContent ps) = toJSON ps

instance FromJSON MessageContent where
  parseJSON (String t) = pure (TextContent t)
  parseJSON (Array _)  = pure (PartsContent [])
  parseJSON _          = fail "MessageContent: expected string or array"

data Message = Message
  { role       :: Role
  , content    :: MessageContent
  , toolCallId :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance ToJSON Message where
  toJSON msg = object $
    [ "role"    .= msg.role
    , "content" .= msg.content
    ] <>
    [ "tool_call_id" .= tcid | Just tcid <- [msg.toolCallId] ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> Message
    <$> o .:  "role"
    <*> o .:  "content"
    <*> o .:? "tool_call_id"

userMessage :: Text -> Message
userMessage t = Message User (TextContent t) Nothing

assistantMessage :: Text -> Message
assistantMessage t = Message Assistant (TextContent t) Nothing

systemMessage :: Text -> Message
systemMessage t = Message System (TextContent t) Nothing

toolResultMessage :: Text -> Text -> Message
toolResultMessage tcid result =
  Message ToolRole (TextContent result) (Just tcid)


-- --------------------------------------------------------------------
-- Tools
-- --------------------------------------------------------------------

data FunctionDef = FunctionDef
  { name        :: Text
  , description :: Maybe Text
  , parameters  :: Value
  } deriving stock (Eq, Show, Generic)

instance ToJSON FunctionDef where
  toJSON fd = object $
    [ "name"       .= fd.name
    , "parameters" .= fd.parameters
    ] <> [ "description" .= d | Just d <- [fd.description] ]

instance FromJSON FunctionDef where
  parseJSON = withObject "FunctionDef" $ \o -> FunctionDef
    <$> o .:  "name"
    <*> o .:? "description"
    <*> o .:  "parameters"

data Tool = Tool
  { function :: FunctionDef
  } deriving stock (Eq, Show, Generic)

instance ToJSON Tool where
  toJSON t = object ["type" .= ("function" :: Text), "function" .= t.function]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> Tool <$> o .: "function"

data ToolChoice = ToolChoiceAuto | ToolChoiceNone | ToolChoiceRequired
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolChoice where
  toJSON = \case
    ToolChoiceAuto     -> String "auto"
    ToolChoiceNone     -> String "none"
    ToolChoiceRequired -> String "required"

data ToolCallChunk = ToolCallChunk
  { index    :: Int
  , id       :: Maybe Text
  , name     :: Maybe Text
  , argsDelta :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON ToolCallChunk where
  parseJSON = withObject "ToolCallChunk" $ \o -> do
    idx  <- o .:  "index"
    mid  <- o .:? "id"
    fn   <- o .:? "function"
    mname <- maybe (pure Nothing) (.:? "name") fn
    margs <- maybe (pure Nothing) (.:? "arguments") fn
    pure $ ToolCallChunk idx mid mname margs

data ToolCall = ToolCall
  { id        :: Text
  , name      :: Text
  , arguments :: Text
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- --------------------------------------------------------------------
-- Request / Response
-- --------------------------------------------------------------------

data ChatRequest = ChatRequest
  { model       :: Text
  , messages    :: [Message]
  , tools       :: Maybe [Tool]
  , toolChoice  :: Maybe ToolChoice
  , maxTokens   :: Maybe Int
  , temperature :: Maybe Double
  , topP        :: Maybe Double
  , stream      :: Bool
  } deriving stock (Eq, Show, Generic)

instance ToJSON ChatRequest where
  toJSON req = object $
    [ "model"    .= req.model
    , "messages" .= req.messages
    , "stream"   .= req.stream
    ] <>
    [ "tools"       .= ts  | Just ts <- [req.tools]       ] <>
    [ "tool_choice" .= tc  | Just tc <- [req.toolChoice]  ] <>
    [ "max_tokens"  .= n   | Just n  <- [req.maxTokens]   ] <>
    [ "temperature" .= t   | Just t  <- [req.temperature] ] <>
    [ "top_p"       .= p   | Just p  <- [req.topP]        ]

chatRequest :: Text -> [Message] -> ChatRequest
chatRequest model msgs = ChatRequest
  { model       = model
  , messages    = msgs
  , tools       = Nothing
  , toolChoice  = Nothing
  , maxTokens   = Nothing
  , temperature = Nothing
  , topP        = Nothing
  , stream      = False
  }

data FinishReason = FinishStop | FinishLength | FinishToolCalls | FinishOther Text
  deriving stock (Eq, Show, Generic)

instance FromJSON FinishReason where
  parseJSON = withText "FinishReason" $ \case
    "stop"         -> pure FinishStop
    "length"       -> pure FinishLength
    "tool_calls"   -> pure FinishToolCalls
    other          -> pure (FinishOther other)

data Usage = Usage
  { promptTokens     :: Int
  , completionTokens :: Int
  } deriving stock (Eq, Show, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o -> Usage
    <$> o .: "prompt_tokens"
    <*> o .: "completion_tokens"

data Choice = Choice
  { message      :: Message
  , finishReason :: Maybe FinishReason
  } deriving stock (Eq, Show, Generic)

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o -> Choice
    <$> o .:  "message"
    <*> o .:? "finish_reason"

data ChatResponse = ChatResponse
  { id      :: Text
  , choices :: [Choice]
  , usage   :: Maybe Usage
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)


-- --------------------------------------------------------------------
-- Streaming
-- --------------------------------------------------------------------

data DeltaContent
  = DeltaText !Text
  | DeltaToolCall !ToolCallChunk
  | DeltaEmpty
  deriving stock (Eq, Show, Generic)

data StreamChoice = StreamChoice
  { delta        :: DeltaContent
  , finishReason :: Maybe FinishReason
  } deriving stock (Eq, Show, Generic)

instance FromJSON StreamChoice where
  parseJSON = withObject "StreamChoice" $ \o -> do
    d  <- o .:  "delta"
    fr <- o .:? "finish_reason"
    delta <- parseDelta d
    pure (StreamChoice delta fr)
    where
      parseDelta = withObject "Delta" $ \d -> do
        mtext  <- d .:? "content"
        mtcs   <- d .:? "tool_calls"
        pure $ case (mtext, mtcs) of
          (Just t, _)        -> DeltaText t
          (_, Just (tc:_))   -> DeltaToolCall tc
          _                  -> DeltaEmpty

data StreamChunk = StreamChunk
  { id      :: Text
  , choices :: [StreamChoice]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)


-- --------------------------------------------------------------------
-- Errors
-- --------------------------------------------------------------------

data ApiErrorDetail = ApiErrorDetail
  { message :: Text
  , type_   :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON ApiErrorDetail where
  parseJSON = withObject "ApiErrorDetail" $ \o -> ApiErrorDetail
    <$> o .:  "message"
    <*> o .:? "type"

newtype ApiError = ApiError { error :: ApiErrorDetail }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data ClientError
  = ApiErrorResponse !Int !ApiErrorDetail
  | NetworkError !Text
  | TimeoutError
  | DeserializationError !Text
  deriving stock (Eq, Show, Generic)


-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data ClientConfig = ClientConfig
  { apiKey    :: Text
  , baseUrl   :: Text
  , timeoutMs :: Int
  } deriving stock (Eq, Show, Generic)

defaultOpenAIConfig :: Text -> ClientConfig
defaultOpenAIConfig key = ClientConfig
  { apiKey    = key
  , baseUrl   = "https://api.openai.com"
  , timeoutMs = 120000
  }
