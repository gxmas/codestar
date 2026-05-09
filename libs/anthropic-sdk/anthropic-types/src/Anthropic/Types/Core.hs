-- | Core type definitions: newtypes for identifiers, enums for shared vocabulary.
--
-- These types form the foundation of the Anthropic API type system.
-- Every other module in the library depends on these definitions.
module Anthropic.Types.Core
  ( -- * API Key
    ApiKey (..)

    -- * Identifiers
  , ModelId (..)
  , MessageId (..)
  , ContentBlockIndex (..)
  , BatchId (..)
  , RequestId (..)

    -- * Enumerations
  , Role (..)
  , StopReason (..)
  , ServiceTierPreference (..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import qualified Data.Aeson.Encoding as E
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | API key for authenticating with the Anthropic API.
--
-- Intentionally has no 'Show' instance to prevent accidental logging.
newtype ApiKey = ApiKey { unApiKey :: Text }
  deriving newtype (Eq, IsString)

-- | Model identifier (e.g., @"claude-sonnet-4-20250514"@).
newtype ModelId = ModelId { unModelId :: Text }
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | Unique identifier for a message response.
newtype MessageId = MessageId { unMessageId :: Text }
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | Zero-based index of a content block within a message.
newtype ContentBlockIndex = ContentBlockIndex { unContentBlockIndex :: Int }
  deriving newtype (Eq, Ord, Show, Num, FromJSON, ToJSON)

-- | Unique identifier for a message batch.
newtype BatchId = BatchId { unBatchId :: Text }
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | Server-assigned request identifier for debugging and support.
newtype RequestId = RequestId { unRequestId :: Text }
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | Message role: user or assistant.
data Role
  = User
  | Assistant
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON Role where
  toJSON User      = "user"
  toJSON Assistant = "assistant"
  toEncoding User      = E.text "user"
  toEncoding Assistant = E.text "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user"      -> pure User
    "assistant" -> pure Assistant
    other       -> fail $ "Unknown Role: " ++ T.unpack other

-- | Reason why generation stopped.
data StopReason
  = EndTurn
  | MaxTokens
  | StopSequence
  | ToolUse
  | PauseTurn
  | Refusal
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON StopReason where
  toJSON EndTurn      = "end_turn"
  toJSON MaxTokens    = "max_tokens"
  toJSON StopSequence = "stop_sequence"
  toJSON ToolUse      = "tool_use"
  toJSON PauseTurn    = "pause_turn"
  toJSON Refusal      = "refusal"
  toEncoding EndTurn      = E.text "end_turn"
  toEncoding MaxTokens    = E.text "max_tokens"
  toEncoding StopSequence = E.text "stop_sequence"
  toEncoding ToolUse      = E.text "tool_use"
  toEncoding PauseTurn    = E.text "pause_turn"
  toEncoding Refusal      = E.text "refusal"

instance FromJSON StopReason where
  parseJSON = withText "StopReason" $ \case
    "end_turn"      -> pure EndTurn
    "max_tokens"    -> pure MaxTokens
    "stop_sequence" -> pure StopSequence
    "tool_use"      -> pure ToolUse
    "pause_turn"    -> pure PauseTurn
    "refusal"       -> pure Refusal
    other           -> fail $ "Unknown StopReason: " ++ T.unpack other

-- | Preferred service tier for request processing.
data ServiceTierPreference
  = ServiceTierAuto
  | StandardOnly
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON ServiceTierPreference where
  toJSON ServiceTierAuto = "auto"
  toJSON StandardOnly    = "standard"
  toEncoding ServiceTierAuto = E.text "auto"
  toEncoding StandardOnly    = E.text "standard"

instance FromJSON ServiceTierPreference where
  parseJSON = withText "ServiceTierPreference" $ \case
    "auto"     -> pure ServiceTierAuto
    "standard" -> pure StandardOnly
    other      -> fail $ "Unknown ServiceTierPreference: " ++ T.unpack other
