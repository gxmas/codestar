-- | Tool use content block types.
module Anthropic.Types.Content.ToolUse
  ( -- * Tool Use Block
    ToolUseBlock (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import Anthropic.Types.Cache (CacheControl)

-- | A tool use content block. Represents the model invoking a tool.
data ToolUseBlock = ToolUseBlock
  { id           :: !Text
  , name         :: !Text
  , input        :: !Value
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolUseBlock where
  toJSON tb = object $
    [ "type"  .= ("tool_use" :: Text)
    , "id"    .= tb.id
    , "name"  .= tb.name
    , "input" .= tb.input
    ]
    ++ maybe [] (\cc -> ["cache_control" .= cc]) tb.cacheControl
  toEncoding tb = E.pairs $
       "type"  .= ("tool_use" :: Text)
    <> "id"    .= tb.id
    <> "name"  .= tb.name
    <> "input" .= tb.input
    <> foldMap ("cache_control" .=) tb.cacheControl

instance FromJSON ToolUseBlock where
  parseJSON = withObject "ToolUseBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "tool_use" ->
        ToolUseBlock
          <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:  "input"
          <*> o .:? "cache_control"
      _ -> fail $ "Expected type \"tool_use\", got: " ++ show typ
