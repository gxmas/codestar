-- | Text content block and citation types.
module Anthropic.Types.Content.Text
  ( -- * Text Block
    TextBlock (..)

    -- * Citations
  , Citation (..)
  , CitationConfig (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

import Anthropic.Types.Cache (CacheControl)

-- | Citation attached to a text block.
--
-- Discriminated by @type@ in wire format: @char_location@, @page_location@,
-- @content_block_location@, @web_search_result_location@, @search_result_location@.
data Citation
  = CharLocationCitation
      { citedText      :: !Text
      , documentIndex  :: !Int
      , documentTitle  :: !Text
      , startCharIndex :: !Int
      , endCharIndex   :: !Int
      , fileId         :: !(Maybe Text)
      }
  | PageLocationCitation
      { citedText       :: !Text
      , documentIndex   :: !Int
      , documentTitle   :: !Text
      , startPageNumber :: !Int
      , endPageNumber   :: !Int
      , fileId          :: !(Maybe Text)
      }
  | ContentBlockLocationCitation
      { citedText       :: !Text
      , documentIndex   :: !Int
      , documentTitle   :: !Text
      , startBlockIndex :: !Int
      , endBlockIndex   :: !Int
      , fileId          :: !(Maybe Text)
      }
  | WebSearchResultLocationCitation
      { citedText      :: !Text
      , encryptedIndex :: !Text
      , title          :: !Text
      , url            :: !Text
      }
  | SearchResultLocationCitation
      { citedText         :: !Text
      , searchResultIndex :: !Int
      , source            :: !Text
      , title             :: !Text
      , startBlockIndex   :: !Int
      , endBlockIndex     :: !Int
      }
  deriving stock (Eq, Show, Generic)

instance ToJSON Citation where
  toJSON (CharLocationCitation ct di dt sci eci fi) = object $
    [ "type"             .= ("char_location" :: Text)
    , "cited_text"       .= ct
    , "document_index"   .= di
    , "document_title"   .= dt
    , "start_char_index" .= sci
    , "end_char_index"   .= eci
    ] ++ maybe [] (\f -> ["file_id" .= f]) fi
  toJSON (PageLocationCitation ct di dt spn epn fi) = object $
    [ "type"              .= ("page_location" :: Text)
    , "cited_text"        .= ct
    , "document_index"    .= di
    , "document_title"    .= dt
    , "start_page_number" .= spn
    , "end_page_number"   .= epn
    ] ++ maybe [] (\f -> ["file_id" .= f]) fi
  toJSON (ContentBlockLocationCitation ct di dt sbi ebi fi) = object $
    [ "type"              .= ("content_block_location" :: Text)
    , "cited_text"        .= ct
    , "document_index"    .= di
    , "document_title"    .= dt
    , "start_block_index" .= sbi
    , "end_block_index"   .= ebi
    ] ++ maybe [] (\f -> ["file_id" .= f]) fi
  toJSON (WebSearchResultLocationCitation ct ei t u) = object
    [ "type"            .= ("web_search_result_location" :: Text)
    , "cited_text"      .= ct
    , "encrypted_index" .= ei
    , "title"           .= t
    , "url"             .= u
    ]
  toJSON (SearchResultLocationCitation ct sri s t sbi ebi) = object
    [ "type"                .= ("search_result_location" :: Text)
    , "cited_text"          .= ct
    , "search_result_index" .= sri
    , "source"              .= s
    , "title"               .= t
    , "start_block_index"   .= sbi
    , "end_block_index"     .= ebi
    ]

  toEncoding (CharLocationCitation ct di dt sci eci fi) = E.pairs $
       "type"             .= ("char_location" :: Text)
    <> "cited_text"       .= ct
    <> "document_index"   .= di
    <> "document_title"   .= dt
    <> "start_char_index" .= sci
    <> "end_char_index"   .= eci
    <> foldMap ("file_id" .=) fi
  toEncoding (PageLocationCitation ct di dt spn epn fi) = E.pairs $
       "type"              .= ("page_location" :: Text)
    <> "cited_text"        .= ct
    <> "document_index"    .= di
    <> "document_title"    .= dt
    <> "start_page_number" .= spn
    <> "end_page_number"   .= epn
    <> foldMap ("file_id" .=) fi
  toEncoding (ContentBlockLocationCitation ct di dt sbi ebi fi) = E.pairs $
       "type"              .= ("content_block_location" :: Text)
    <> "cited_text"        .= ct
    <> "document_index"    .= di
    <> "document_title"    .= dt
    <> "start_block_index" .= sbi
    <> "end_block_index"   .= ebi
    <> foldMap ("file_id" .=) fi
  toEncoding (WebSearchResultLocationCitation ct ei t u) = E.pairs $
       "type"            .= ("web_search_result_location" :: Text)
    <> "cited_text"      .= ct
    <> "encrypted_index" .= ei
    <> "title"           .= t
    <> "url"             .= u
  toEncoding (SearchResultLocationCitation ct sri s t sbi ebi) = E.pairs $
       "type"                .= ("search_result_location" :: Text)
    <> "cited_text"          .= ct
    <> "search_result_index" .= sri
    <> "source"              .= s
    <> "title"               .= t
    <> "start_block_index"   .= sbi
    <> "end_block_index"     .= ebi

instance FromJSON Citation where
  parseJSON = withObject "Citation" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "char_location" ->
        CharLocationCitation
          <$> o .:  "cited_text"
          <*> o .:  "document_index"
          <*> o .:  "document_title"
          <*> o .:  "start_char_index"
          <*> o .:  "end_char_index"
          <*> o .:? "file_id"
      "page_location" ->
        PageLocationCitation
          <$> o .:  "cited_text"
          <*> o .:  "document_index"
          <*> o .:  "document_title"
          <*> o .:  "start_page_number"
          <*> o .:  "end_page_number"
          <*> o .:? "file_id"
      "content_block_location" ->
        ContentBlockLocationCitation
          <$> o .:  "cited_text"
          <*> o .:  "document_index"
          <*> o .:  "document_title"
          <*> o .:  "start_block_index"
          <*> o .:  "end_block_index"
          <*> o .:? "file_id"
      "web_search_result_location" ->
        WebSearchResultLocationCitation
          <$> o .: "cited_text"
          <*> o .: "encrypted_index"
          <*> o .: "title"
          <*> o .: "url"
      "search_result_location" ->
        SearchResultLocationCitation
          <$> o .: "cited_text"
          <*> o .: "search_result_index"
          <*> o .: "source"
          <*> o .: "title"
          <*> o .: "start_block_index"
          <*> o .: "end_block_index"
      _ -> fail $ "Unknown Citation type: " ++ show typ

-- | Citation configuration for document blocks.
data CitationConfig = CitationConfig
  { enabled :: !Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CitationConfig where
  toJSON cc = object ["enabled" .= cc.enabled]
  toEncoding cc = E.pairs ("enabled" .= cc.enabled)

instance FromJSON CitationConfig where
  parseJSON = withObject "CitationConfig" $ \o ->
    CitationConfig <$> o .: "enabled"

-- | A text content block.
data TextBlock = TextBlock
  { text         :: !Text
  , citations    :: !(Maybe [Citation])
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON TextBlock where
  toJSON tb = object $
    [ "type" .= ("text" :: Text)
    , "text" .= tb.text
    ]
    ++ maybe [] (\c  -> ["citations"     .= c])  tb.citations
    ++ maybe [] (\cc -> ["cache_control" .= cc]) tb.cacheControl
  toEncoding tb = E.pairs $
       "type" .= ("text" :: Text)
    <> "text" .= tb.text
    <> foldMap ("citations"     .=) tb.citations
    <> foldMap ("cache_control" .=) tb.cacheControl

instance FromJSON TextBlock where
  parseJSON = withObject "TextBlock" $ \o ->
    TextBlock
      <$> o .:  "text"
      <*> o .:? "citations"
      <*> o .:? "cache_control"
