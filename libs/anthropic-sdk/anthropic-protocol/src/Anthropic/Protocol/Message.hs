{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
-- The with* setters use record update syntax on fields shared across types
-- (cacheControl, container, serviceTier, thinking). The type signature on
-- each setter makes the target type unambiguous, but GHC 9.6+ warns about
-- TDNR deprecation. This pragma suppresses that warning in the defining
-- module only -- consumers use plain functions and never encounter it.

-- | Message request, response, and convenience constructors.
module Anthropic.Protocol.Message
  ( -- * Request
    MessageRequest (..)
  , messageRequest

    -- ** Builder Setters
    -- | Use these with 'Data.Function.&' to configure optional fields
    -- without @Just@ wrapping or record update syntax. See ADR-010.
    --
    -- @
    -- let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
    --         & withTemperature 0.7
    --         & withSystem "You are helpful."
    -- @
  , withSystem
  , withStopSequences
  , withTemperature
  , withTopP
  , withTopK
  , withTools
  , withToolChoice
  , withThinking
  , withStream
  , withMetadata
  , withServiceTier
  , withContainer
  , withCacheControl

    -- * Request Metadata
  , RequestMetadata (..)

    -- * Message
  , Message (..)
  , userMessage
  , assistantMessage

    -- * Response
  , MessageResponse (..)
  , Container (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject
  , defaultOptions, genericToJSON, genericToEncoding, genericParseJSON
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Options(..), Parser, camelTo2)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types
import Anthropic.Protocol.Tool (ToolDefinition, ToolChoice)
import Anthropic.Protocol.Thinking (ThinkingConfig)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

-- | Request metadata.
data RequestMetadata = RequestMetadata
  { userId :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RequestMetadata where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON RequestMetadata where
  parseJSON = genericParseJSON customOptions

-- | A message in a conversation.
data Message = Message
  { role    :: !Role
  , content :: !MessageContent
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Message where
  toJSON m = object
    [ "role"    .= m.role
    , "content" .= m.content
    ]
  toEncoding m = E.pairs $
       "role"    .= m.role
    <> "content" .= m.content

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message
      <$> o .: "role"
      <*> o .: "content"

-- | Convenience constructor for user messages.
--
-- @userMessage \"Hello\"@ is equivalent to @Message User \"Hello\"@.
userMessage :: MessageContent -> Message
userMessage = Message User

-- | Convenience constructor for assistant messages.
assistantMessage :: MessageContent -> Message
assistantMessage = Message Assistant

-- | Container info for code execution tool sessions.
data Container = Container
  { containerId :: !Text
  , expiresAt   :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Container where
  toJSON c = object
    [ "id"         .= c.containerId
    , "expires_at" .= c.expiresAt
    ]
  toEncoding c = E.pairs $
       "id"         .= c.containerId
    <> "expires_at" .= c.expiresAt

instance FromJSON Container where
  parseJSON = withObject "Container" $ \o ->
    Container
      <$> o .: "id"
      <*> o .: "expires_at"

-- | A message creation request.
--
-- Use the 'messageRequest' smart constructor for required fields,
-- then configure optional fields with @with*@ setters and
-- 'Data.Function.&':
--
-- @
-- let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
--         & withTemperature 0.7
--         & withSystem "You are helpful."
-- @
--
-- Record constructors are exported for pattern matching and test
-- construction, but @with*@ setters are the recommended mutation API.
-- See ADR-010.
data MessageRequest = MessageRequest
  { model         :: !ModelId
  , messages      :: ![Message]
  , maxTokens     :: !Int
  , system        :: !(Maybe SystemPrompt)
  , stopSequences :: !(Maybe [Text])
  , temperature   :: !(Maybe Double)
  , topP          :: !(Maybe Double)
  , topK          :: !(Maybe Int)
  , tools         :: !(Maybe [ToolDefinition])
  , toolChoice    :: !(Maybe ToolChoice)
  , thinking      :: !(Maybe ThinkingConfig)
  , stream        :: !(Maybe Bool)
  , metadata      :: !(Maybe RequestMetadata)
  , serviceTier   :: !(Maybe ServiceTierPreference)
  , container     :: !(Maybe Text)
    -- ^ Container ID for code execution reuse.
  , cacheControl  :: !(Maybe CacheControl)
    -- ^ Top-level cache breakpoint.
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON MessageRequest where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON MessageRequest where
  parseJSON = genericParseJSON customOptions

-- | Smart constructor for 'MessageRequest' with only required fields.
--
-- All optional fields default to 'Nothing'. Configure them with
-- @with*@ setters:
--
-- @
-- let req = messageRequest "claude-sonnet-4-20250514" [userMessage "Hello"] 1024
--         & withTemperature 0.7
--         & withSystem "You are helpful."
--         & withTools [weatherTool]
-- @
messageRequest :: ModelId -> [Message] -> Int -> MessageRequest
messageRequest model msgs maxTok = MessageRequest
  { model         = model
  , messages      = msgs
  , maxTokens     = maxTok
  , system        = Nothing
  , stopSequences = Nothing
  , temperature   = Nothing
  , topP          = Nothing
  , topK          = Nothing
  , tools         = Nothing
  , toolChoice    = Nothing
  , thinking      = Nothing
  , stream        = Nothing
  , metadata      = Nothing
  , serviceTier   = Nothing
  , container     = Nothing
  , cacheControl  = Nothing
  }

-- | Set the system prompt.
withSystem :: SystemPrompt -> MessageRequest -> MessageRequest
withSystem x req = req { system = Just x }

-- | Set stop sequences.
withStopSequences :: [Text] -> MessageRequest -> MessageRequest
withStopSequences x req = req { stopSequences = Just x }

-- | Set the sampling temperature.
withTemperature :: Double -> MessageRequest -> MessageRequest
withTemperature x req = req { temperature = Just x }

-- | Set the nucleus sampling probability.
withTopP :: Double -> MessageRequest -> MessageRequest
withTopP x req = req { topP = Just x }

-- | Set the top-k sampling parameter.
withTopK :: Int -> MessageRequest -> MessageRequest
withTopK x req = req { topK = Just x }

-- | Set the tool definitions.
withTools :: [ToolDefinition] -> MessageRequest -> MessageRequest
withTools x req = req { tools = Just x }

-- | Set the tool choice strategy.
withToolChoice :: ToolChoice -> MessageRequest -> MessageRequest
withToolChoice x req = req { toolChoice = Just x }

-- | Set the thinking configuration.
withThinking :: ThinkingConfig -> MessageRequest -> MessageRequest
withThinking x req = req { thinking = Just x }

-- | Set the stream flag.
withStream :: Bool -> MessageRequest -> MessageRequest
withStream x req = req { stream = Just x }

-- | Set request metadata.
withMetadata :: RequestMetadata -> MessageRequest -> MessageRequest
withMetadata x req = req { metadata = Just x }

-- | Set the service tier preference.
withServiceTier :: ServiceTierPreference -> MessageRequest -> MessageRequest
withServiceTier x req = req { serviceTier = Just x }

-- | Set the container ID for code execution reuse.
withContainer :: Text -> MessageRequest -> MessageRequest
withContainer x req = req { container = Just x }

-- | Set the top-level cache breakpoint.
withCacheControl :: CacheControl -> MessageRequest -> MessageRequest
withCacheControl x req = req { cacheControl = Just x }

-- | Response from the Messages API.
--
-- This is a plain record with exported constructors for easy test construction.
-- The @type@ and @role@ fields from the wire format are handled by serialization
-- only (validated in 'FromJSON', injected in 'ToJSON').
--
-- See ADR-004 for the design rationale.
data MessageResponse = MessageResponse
  { id           :: !MessageId
  , model        :: !ModelId
  , content      :: ![ContentBlock]
  , stopReason   :: !(Maybe StopReason)
    -- ^ 'Nothing' in streaming @message_start@; always 'Just' in final response.
  , stopSequence :: !(Maybe Text)
  , usage        :: !Usage
  , container    :: !(Maybe Container)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON MessageResponse where
  toJSON mr = object $
    [ "type"          .= ("message" :: Text)
    , "role"          .= ("assistant" :: Text)
    , "id"            .= mr.id
    , "model"         .= mr.model
    , "content"       .= mr.content
    , "usage"         .= mr.usage
    ]
    ++ maybe [] (\r -> ["stop_reason"   .= r]) mr.stopReason
    ++ maybe [] (\s -> ["stop_sequence" .= s]) mr.stopSequence
    ++ maybe [] (\c -> ["container"     .= c]) mr.container

  toEncoding mr = E.pairs $
       "type"          .= ("message" :: Text)
    <> "role"          .= ("assistant" :: Text)
    <> "id"            .= mr.id
    <> "model"         .= mr.model
    <> "content"       .= mr.content
    <> "usage"         .= mr.usage
    <> foldMap ("stop_reason"   .=) mr.stopReason
    <> foldMap ("stop_sequence" .=) mr.stopSequence
    <> foldMap ("container"     .=) mr.container

instance FromJSON MessageResponse where
  parseJSON = withObject "MessageResponse" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "message" -> pure ()
      _         -> fail $ "Expected type 'message', got: " ++ T.unpack typ
    role' <- o .: "role" :: Parser Text
    case role' of
      "assistant" -> pure ()
      _           -> fail $ "Expected role 'assistant', got: " ++ T.unpack role'
    MessageResponse
      <$> o .:  "id"
      <*> o .:  "model"
      <*> o .:  "content"
      <*> o .:? "stop_reason"
      <*> o .:? "stop_sequence"
      <*> o .:  "usage"
      <*> o .:? "container"
