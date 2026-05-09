-- | Tool result content block types.
module Anthropic.Types.Content.ToolResult
  ( -- * Tool Result Block
    ToolResultBlock (..)
  , ToolResultContent (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import Anthropic.Types.Cache (CacheControl)

-- | Content of a tool result: either plain text or structured content blocks.
--
-- The structured variant wraps aeson 'Value' to avoid a circular
-- dependency with 'ContentBlock'. Consumers destructure via the
-- re-export module.
data ToolResultContent
  = ToolResultText !Text
  | ToolResultBlocks ![Value]
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolResultContent where
  toJSON (ToolResultText t)    = toJSON t
  toJSON (ToolResultBlocks bs) = toJSON bs
  toEncoding (ToolResultText t)    = toEncoding t
  toEncoding (ToolResultBlocks bs) = toEncoding bs

instance FromJSON ToolResultContent where
  parseJSON v = case v of
    Aeson.String t -> pure (ToolResultText t)
    Aeson.Array _  -> ToolResultBlocks <$> parseJSON v
    _              -> fail "ToolResultContent: expected String or Array"

-- | A tool result content block. Sent by the user to provide tool output.
data ToolResultBlock = ToolResultBlock
  { toolUseId    :: !Text
  , content      :: !(Maybe ToolResultContent)
  , isError      :: !(Maybe Bool)
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolResultBlock where
  toJSON tb = object $
    [ "type"        .= ("tool_result" :: Text)
    , "tool_use_id" .= tb.toolUseId
    ]
    ++ maybe [] (\c  -> ["content"       .= c])  tb.content
    ++ maybe [] (\ie -> ["is_error"      .= ie]) tb.isError
    ++ maybe [] (\cc -> ["cache_control" .= cc]) tb.cacheControl
  toEncoding tb = E.pairs $
       "type"        .= ("tool_result" :: Text)
    <> "tool_use_id" .= tb.toolUseId
    <> foldMap ("content"       .=) tb.content
    <> foldMap ("is_error"      .=) tb.isError
    <> foldMap ("cache_control" .=) tb.cacheControl

instance FromJSON ToolResultBlock where
  parseJSON = withObject "ToolResultBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "tool_result" ->
        ToolResultBlock
          <$> o .:  "tool_use_id"
          <*> o .:? "content"
          <*> o .:? "is_error"
          <*> o .:? "cache_control"
      _ -> fail $ "Expected type \"tool_result\", got: " ++ show typ
