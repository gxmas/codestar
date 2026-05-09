-- | Token counting types.
--
-- The @with*@ setters for 'TokenCountRequest' are not re-exported from
-- @Anthropic.Protocol@ or @Anthropic.Client@ to avoid name collisions
-- with 'Anthropic.Protocol.Message.MessageRequest' setters. Import this
-- module qualified if you need them:
--
-- @
-- import qualified Anthropic.Protocol.TokenCount as TC
--
-- let req = TC.tokenCountRequest "claude-sonnet-4-20250514" msgs
--         & TC.withSystem "You are helpful."
-- @
module Anthropic.Protocol.TokenCount
  ( -- * Token Count
    TokenCountRequest (..)
  , TokenCountResponse (..)
  , tokenCountRequest

    -- ** Builder Setters
  , withSystem
  , withTools
  , withToolChoice
  , withThinking
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:)
  , object, withObject
  , defaultOptions, genericToJSON, genericToEncoding, genericParseJSON
  )
import Data.Aeson.Types (Options(..), camelTo2)
import qualified Data.Aeson.Encoding as E
import GHC.Generics (Generic)

import Anthropic.Types (ModelId, SystemPrompt)
import Anthropic.Protocol.Message (Message)
import Anthropic.Protocol.Tool (ToolDefinition, ToolChoice)
import Anthropic.Protocol.Thinking (ThinkingConfig)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

-- | Token counting request.
--
-- Uses the same typed fields as 'MessageRequest' for tools and thinking,
-- ensuring the consumer cannot construct an invalid token count request.
data TokenCountRequest = TokenCountRequest
  { model      :: !ModelId
  , messages   :: ![Message]
  , system     :: !(Maybe SystemPrompt)
  , tools      :: !(Maybe [ToolDefinition])
  , toolChoice :: !(Maybe ToolChoice)
  , thinking   :: !(Maybe ThinkingConfig)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TokenCountRequest where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON TokenCountRequest where
  parseJSON = genericParseJSON customOptions

-- | Smart constructor for 'TokenCountRequest' with only required fields.
--
-- Configure optional fields with @with*@ setters:
--
-- @
-- import qualified Anthropic.Protocol.TokenCount as TC
--
-- let req = TC.tokenCountRequest "claude-sonnet-4-20250514" msgs
--         & TC.withSystem "You are helpful."
--         & TC.withTools [myTool]
-- @
tokenCountRequest :: ModelId -> [Message] -> TokenCountRequest
tokenCountRequest model msgs = TokenCountRequest
  { model      = model
  , messages   = msgs
  , system     = Nothing
  , tools      = Nothing
  , toolChoice = Nothing
  , thinking   = Nothing
  }

-- | Set the system prompt.
withSystem :: SystemPrompt -> TokenCountRequest -> TokenCountRequest
withSystem x req = req { system = Just x }

-- | Set the tool definitions.
withTools :: [ToolDefinition] -> TokenCountRequest -> TokenCountRequest
withTools x req = req { tools = Just x }

-- | Set the tool choice strategy.
withToolChoice :: ToolChoice -> TokenCountRequest -> TokenCountRequest
withToolChoice x req = req { toolChoice = Just x }

-- | Set the thinking configuration.
withThinking :: ThinkingConfig -> TokenCountRequest -> TokenCountRequest
withThinking x req = req { thinking = Just x }

-- | Token counting response.
data TokenCountResponse = TokenCountResponse
  { inputTokens :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TokenCountResponse where
  toJSON r = object [ "input_tokens" .= r.inputTokens ]
  toEncoding r = E.pairs $ "input_tokens" .= r.inputTokens

instance FromJSON TokenCountResponse where
  parseJSON = withObject "TokenCountResponse" $ \o ->
    TokenCountResponse <$> o .: "input_tokens"
