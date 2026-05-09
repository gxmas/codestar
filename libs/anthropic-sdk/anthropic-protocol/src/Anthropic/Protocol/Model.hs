-- | Models API types.
module Anthropic.Protocol.Model
  ( -- * Model Info
    ModelInfo (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , Value, object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types (ModelId)

-- | Information about a model.
--
-- The @capabilities@ field is kept as raw 'Value' because its structure
-- varies across models and evolves over time. Consumers inspect it via
-- aeson combinators.
--
-- The @type@ field (@\"model\"@) is a wire format artifact handled in
-- serialization only (same pattern as 'MessageResponse', see ADR-004).
data ModelInfo = ModelInfo
  { modelId        :: !ModelId
  , displayName    :: !Text
  , createdAt      :: !Text
  , maxInputTokens :: !(Maybe Int)
    -- ^ Maximum input tokens the model accepts.
  , maxTokens      :: !(Maybe Int)
    -- ^ Maximum output tokens the model can generate.
  , capabilities   :: !(Maybe Value)
    -- ^ Model capabilities (batch, citations, thinking, etc.)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ModelInfo where
  toJSON mi = object $
    [ "type"             .= ("model" :: Text)
    , "id"               .= mi.modelId
    , "display_name"     .= mi.displayName
    , "created_at"       .= mi.createdAt
    ]
    ++ maybe [] (\v -> ["max_input_tokens" .= v]) mi.maxInputTokens
    ++ maybe [] (\v -> ["max_tokens"       .= v]) mi.maxTokens
    ++ maybe [] (\v -> ["capabilities"     .= v]) mi.capabilities

  toEncoding mi = E.pairs $
       "type"             .= ("model" :: Text)
    <> "id"               .= mi.modelId
    <> "display_name"     .= mi.displayName
    <> "created_at"       .= mi.createdAt
    <> foldMap ("max_input_tokens" .=) mi.maxInputTokens
    <> foldMap ("max_tokens"       .=) mi.maxTokens
    <> foldMap ("capabilities"     .=) mi.capabilities

instance FromJSON ModelInfo where
  parseJSON = withObject "ModelInfo" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "model" -> pure ()
      _       -> fail $ "Expected type 'model', got: " ++ T.unpack typ
    ModelInfo
      <$> o .:  "id"
      <*> o .:  "display_name"
      <*> o .:  "created_at"
      <*> o .:? "max_input_tokens"
      <*> o .:? "max_tokens"
      <*> o .:? "capabilities"
