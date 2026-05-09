-- | Document content block types (PDF, plain text, content).
module Anthropic.Types.Content.Document
  ( -- * Document Block
    DocumentBlock (..)

    -- * Document Source
  , DocumentSource (..)

    -- * Document Media Type
  , DocumentMediaType (..)
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

import Anthropic.Types.Cache (CacheControl)
import Anthropic.Types.Content.Text (CitationConfig)

-- | Media type for base64-encoded documents.
data DocumentMediaType
  = ApplicationPdf
  | TextPlain
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON DocumentMediaType where
  toJSON ApplicationPdf = "application/pdf"
  toJSON TextPlain      = "text/plain"
  toEncoding ApplicationPdf = E.text "application/pdf"
  toEncoding TextPlain      = E.text "text/plain"

instance FromJSON DocumentMediaType where
  parseJSON = withText "DocumentMediaType" $ \case
    "application/pdf" -> pure ApplicationPdf
    "text/plain"      -> pure TextPlain
    other             -> fail $ "Unknown DocumentMediaType: " ++ T.unpack other

-- | Source of a document.
data DocumentSource
  = Base64Document !DocumentMediaType !Text
    -- ^ Media type and base64-encoded data
  | UrlDocument !Text
    -- ^ URL to the document
  | ContentDocument !Text
    -- ^ Inline text content
  deriving stock (Eq, Show, Generic)

instance ToJSON DocumentSource where
  toJSON (Base64Document mt d) = object
    [ "type"       .= ("base64" :: Text)
    , "media_type" .= mt
    , "data"       .= d
    ]
  toJSON (UrlDocument u) = object
    [ "type" .= ("url" :: Text)
    , "url"  .= u
    ]
  toJSON (ContentDocument c) = object
    [ "type"    .= ("content" :: Text)
    , "content" .= c
    ]
  toEncoding (Base64Document mt d) = E.pairs $
       "type"       .= ("base64" :: Text)
    <> "media_type" .= mt
    <> "data"       .= d
  toEncoding (UrlDocument u) = E.pairs $
       "type" .= ("url" :: Text)
    <> "url"  .= u
  toEncoding (ContentDocument c) = E.pairs $
       "type"    .= ("content" :: Text)
    <> "content" .= c

instance FromJSON DocumentSource where
  parseJSON = withObject "DocumentSource" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "base64"  -> Base64Document   <$> o .: "media_type" <*> o .: "data"
      "url"     -> UrlDocument      <$> o .: "url"
      "content" -> ContentDocument  <$> o .: "content"
      _         -> fail $ "Unknown DocumentSource type: " ++ T.unpack typ

-- | A document content block.
data DocumentBlock = DocumentBlock
  { source       :: !DocumentSource
  , title        :: !(Maybe Text)
  , context      :: !(Maybe Text)
  , citations    :: !(Maybe CitationConfig)
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DocumentBlock where
  toJSON db = object $
    [ "type"   .= ("document" :: Text)
    , "source" .= db.source
    ]
    ++ maybe [] (\t  -> ["title"         .= t])  db.title
    ++ maybe [] (\c  -> ["context"       .= c])  db.context
    ++ maybe [] (\ci -> ["citations"     .= ci]) db.citations
    ++ maybe [] (\cc -> ["cache_control" .= cc]) db.cacheControl
  toEncoding db = E.pairs $
       "type"   .= ("document" :: Text)
    <> "source" .= db.source
    <> foldMap ("title"         .=) db.title
    <> foldMap ("context"       .=) db.context
    <> foldMap ("citations"     .=) db.citations
    <> foldMap ("cache_control" .=) db.cacheControl

instance FromJSON DocumentBlock where
  parseJSON = withObject "DocumentBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "document" ->
        DocumentBlock
          <$> o .:  "source"
          <*> o .:? "title"
          <*> o .:? "context"
          <*> o .:? "citations"
          <*> o .:? "cache_control"
      _ -> fail $ "Expected type \"document\", got: " ++ show typ
