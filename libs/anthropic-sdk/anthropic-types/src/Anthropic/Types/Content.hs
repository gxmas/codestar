-- | Content block union type and convenience types.
--
-- This module defines 'ContentBlock', the central union type for all
-- content that can appear in messages, plus 'MessageContent' and
-- 'SystemPrompt' with 'IsString' instances for ergonomic construction.
module Anthropic.Types.Content
  ( -- * Content Block Union
    ContentBlock (..)

    -- * Message Content
  , MessageContent (..)

    -- * System Prompt
  , SystemPrompt (..)
  , SystemBlock (..)

    -- * Re-exports
  , module Anthropic.Types.Content.Text
  , module Anthropic.Types.Content.Image
  , module Anthropic.Types.Content.Document
  , module Anthropic.Types.Content.ToolUse
  , module Anthropic.Types.Content.ToolResult
  , module Anthropic.Types.Content.Thinking
  , module Anthropic.Types.Content.Search
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Anthropic.Types.Cache (CacheControl)
import Anthropic.Types.Content.Text
import Anthropic.Types.Content.Image
import Anthropic.Types.Content.Document
import Anthropic.Types.Content.ToolUse
import Anthropic.Types.Content.ToolResult
import Anthropic.Types.Content.Thinking
import Anthropic.Types.Content.Search

-- | The central content block union type.
--
-- Every content block in a message (request or response) is one of these
-- variants. Pattern match to handle specific block types.
data ContentBlock
  = TextContent          !TextBlock
  | ImageContent         !ImageBlock
  | ToolUseContent       !ToolUseBlock
  | ToolResultContent    !ToolResultBlock
  | ThinkingContent      !ThinkingBlock
  | RedactedThinking     !(Maybe Text)
    -- ^ Redacted thinking block. The 'Maybe Text' carries the opaque @data@
    -- field when present.
  | DocumentContent      !DocumentBlock
  | SearchResultContent  !SearchResultBlock
  deriving stock (Eq, Show, Generic)

instance ToJSON ContentBlock where
  toJSON (TextContent tb)         = toJSON tb
  toJSON (ImageContent ib)        = toJSON ib
  toJSON (ToolUseContent tub)     = toJSON tub
  toJSON (ToolResultContent trb)  = toJSON trb
  toJSON (ThinkingContent tb)     = toJSON tb
  toJSON (RedactedThinking md)    = object $
    [ "type" .= ("redacted_thinking" :: Text) ]
    ++ maybe [] (\d -> ["data" .= d]) md
  toJSON (DocumentContent db)     = toJSON db
  toJSON (SearchResultContent sb) = toJSON sb
  toEncoding (TextContent tb)         = toEncoding tb
  toEncoding (ImageContent ib)        = toEncoding ib
  toEncoding (ToolUseContent tub)     = toEncoding tub
  toEncoding (ToolResultContent trb)  = toEncoding trb
  toEncoding (ThinkingContent tb)     = toEncoding tb
  toEncoding (RedactedThinking md)    = E.pairs $
       "type" .= ("redacted_thinking" :: Text)
    <> foldMap ("data" .=) md
  toEncoding (DocumentContent db)     = toEncoding db
  toEncoding (SearchResultContent sb) = toEncoding sb

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "text"              -> TextContent         <$> parseJSON (Aeson.Object o)
      "image"             -> ImageContent        <$> parseJSON (Aeson.Object o)
      "tool_use"          -> ToolUseContent       <$> parseJSON (Aeson.Object o)
      "tool_result"       -> ToolResultContent    <$> parseJSON (Aeson.Object o)
      "thinking"          -> ThinkingContent      <$> parseJSON (Aeson.Object o)
      "redacted_thinking" -> RedactedThinking     <$> o .:? "data"
      "document"          -> DocumentContent      <$> parseJSON (Aeson.Object o)
      "search_result"     -> SearchResultContent  <$> parseJSON (Aeson.Object o)
      _                   -> fail $ "Unknown ContentBlock type: " ++ T.unpack typ

-- | Content of a message: either plain text or a list of content blocks.
--
-- Has an 'IsString' instance so you can write @\"Hello\"@ directly
-- where a 'MessageContent' is expected (with @OverloadedStrings@).
data MessageContent
  = TextMessage !Text
  | BlockMessage ![ContentBlock]
  deriving stock (Eq, Show, Generic)

instance IsString MessageContent where
  fromString = TextMessage . T.pack

instance ToJSON MessageContent where
  toJSON (TextMessage t)  = toJSON t
  toJSON (BlockMessage bs) = toJSON bs
  toEncoding (TextMessage t)  = toEncoding t
  toEncoding (BlockMessage bs) = toEncoding bs

instance FromJSON MessageContent where
  parseJSON v = case v of
    Aeson.String t -> pure (TextMessage t)
    Aeson.Array _  -> BlockMessage <$> parseJSON v
    _              -> fail "MessageContent: expected String or Array"

-- | A system prompt block with optional cache control.
data SystemBlock = SystemBlock
  { text         :: !Text
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SystemBlock where
  toJSON sb = object $
    [ "type" .= ("text" :: Text)
    , "text" .= sb.text
    ]
    ++ maybe [] (\cc -> ["cache_control" .= cc]) sb.cacheControl
  toEncoding sb = E.pairs $
       "type" .= ("text" :: Text)
    <> "text" .= sb.text
    <> foldMap ("cache_control" .=) sb.cacheControl

instance FromJSON SystemBlock where
  parseJSON = withObject "SystemBlock" $ \o ->
    SystemBlock
      <$> o .:  "text"
      <*> o .:? "cache_control"

-- | System prompt: either a simple text string or structured blocks.
--
-- Has an 'IsString' instance for the common case:
-- @system = Just \"You are a helpful assistant\"@.
data SystemPrompt
  = SimpleSystem !Text
  | BlockSystem  ![SystemBlock]
  deriving stock (Eq, Show, Generic)

instance IsString SystemPrompt where
  fromString = SimpleSystem . T.pack

instance ToJSON SystemPrompt where
  toJSON (SimpleSystem t)  = toJSON t
  toJSON (BlockSystem bs)  = toJSON bs
  toEncoding (SimpleSystem t)  = toEncoding t
  toEncoding (BlockSystem bs)  = toEncoding bs

instance FromJSON SystemPrompt where
  parseJSON v = case v of
    Aeson.String t -> pure (SimpleSystem t)
    Aeson.Array _  -> BlockSystem <$> parseJSON v
    _              -> fail "SystemPrompt: expected String or Array"
