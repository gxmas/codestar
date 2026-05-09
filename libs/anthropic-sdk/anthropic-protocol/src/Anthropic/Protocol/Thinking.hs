-- | Extended thinking configuration.
module Anthropic.Protocol.Thinking
  ( -- * Thinking Config
    ThinkingConfig (..)
  , ThinkingDisplay (..)
  , enableThinking
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject, withText
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Display mode for thinking blocks.
data ThinkingDisplay
  = DisplaySummarized
    -- ^ Full thinking returned (default).
  | DisplayOmitted
    -- ^ Thinking redacted; signature returned for multi-turn continuity.
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON ThinkingDisplay where
  toJSON DisplaySummarized = "summarized"
  toJSON DisplayOmitted    = "omitted"
  toEncoding DisplaySummarized = E.text "summarized"
  toEncoding DisplayOmitted    = E.text "omitted"

instance FromJSON ThinkingDisplay where
  parseJSON = withText "ThinkingDisplay" $ \case
    "summarized" -> pure DisplaySummarized
    "omitted"    -> pure DisplayOmitted
    other        -> fail $ "Unknown ThinkingDisplay: " ++ T.unpack other

-- | Configuration for extended thinking.
--
-- Use 'enableThinking' smart constructor to validate the budget constraint.
data ThinkingConfig
  = ThinkingEnabled !Int !(Maybe ThinkingDisplay)
    -- ^ Enabled with budget_tokens (must be >= 1024) and optional display mode.
  | ThinkingDisabled
  | ThinkingAdaptive !(Maybe ThinkingDisplay)
    -- ^ Adaptive thinking with optional display mode.
  deriving stock (Eq, Show, Generic)

instance ToJSON ThinkingConfig where
  toJSON (ThinkingEnabled budget display) = object $
    [ "type"         .= ("enabled" :: Text)
    , "budget_tokens" .= budget
    ]
    ++ maybe [] (\d -> ["display" .= d]) display
  toJSON ThinkingDisabled = object
    [ "type" .= ("disabled" :: Text)
    ]
  toJSON (ThinkingAdaptive display) = object $
    [ "type" .= ("adaptive" :: Text)
    ]
    ++ maybe [] (\d -> ["display" .= d]) display

  toEncoding (ThinkingEnabled budget display) = E.pairs $
       "type"          .= ("enabled" :: Text)
    <> "budget_tokens" .= budget
    <> foldMap ("display" .=) display
  toEncoding ThinkingDisabled = E.pairs $
    "type" .= ("disabled" :: Text)
  toEncoding (ThinkingAdaptive display) = E.pairs $
       "type" .= ("adaptive" :: Text)
    <> foldMap ("display" .=) display

instance FromJSON ThinkingConfig where
  parseJSON = withObject "ThinkingConfig" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "enabled" ->
        ThinkingEnabled
          <$> o .:  "budget_tokens"
          <*> o .:? "display"
      "disabled" -> pure ThinkingDisabled
      "adaptive" ->
        ThinkingAdaptive
          <$> o .:? "display"
      _ -> fail $ "Unknown ThinkingConfig type: " ++ T.unpack typ

-- | Smart constructor for 'ThinkingEnabled' that validates the budget constraint.
--
-- Returns 'Left' with an error message if @budget_tokens < 1024@.
enableThinking :: Int -> Either Text ThinkingConfig
enableThinking budget
  | budget < 1024 = Left "budget_tokens must be >= 1024"
  | otherwise     = Right (ThinkingEnabled budget Nothing)
