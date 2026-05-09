-- | Thinking and redacted thinking content block types.
module Anthropic.Types.Content.Thinking
  ( -- * Thinking Block
    ThinkingBlock (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:)
  , object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A thinking content block. Contains the model's reasoning.
data ThinkingBlock = ThinkingBlock
  { thinking  :: !Text
  , signature :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ThinkingBlock where
  toJSON tb = object
    [ "type"      .= ("thinking" :: Text)
    , "thinking"  .= tb.thinking
    , "signature" .= tb.signature
    ]
  toEncoding tb = E.pairs $
       "type"      .= ("thinking" :: Text)
    <> "thinking"  .= tb.thinking
    <> "signature" .= tb.signature

instance FromJSON ThinkingBlock where
  parseJSON = withObject "ThinkingBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "thinking" -> ThinkingBlock <$> o .: "thinking" <*> o .: "signature"
      _          -> fail $ "Expected type \"thinking\", got: " ++ show typ
