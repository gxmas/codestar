-- | Search result content block types.
module Anthropic.Types.Content.Search
  ( -- * Search Result Block
    SearchResultBlock (..)
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
import Anthropic.Types.Content.Text (CitationConfig, TextBlock)

-- | A search result content block.
data SearchResultBlock = SearchResultBlock
  { source       :: !Text
  , title        :: !Text
  , searchContent :: ![TextBlock]
  , citations    :: !(Maybe CitationConfig)
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchResultBlock where
  toJSON sb = object $
    [ "type"    .= ("search_result" :: Text)
    , "source"  .= sb.source
    , "title"   .= sb.title
    , "content" .= sb.searchContent
    ]
    ++ maybe [] (\ci -> ["citations"     .= ci]) sb.citations
    ++ maybe [] (\cc -> ["cache_control" .= cc]) sb.cacheControl
  toEncoding sb = E.pairs $
       "type"    .= ("search_result" :: Text)
    <> "source"  .= sb.source
    <> "title"   .= sb.title
    <> "content" .= sb.searchContent
    <> foldMap ("citations"     .=) sb.citations
    <> foldMap ("cache_control" .=) sb.cacheControl

instance FromJSON SearchResultBlock where
  parseJSON = withObject "SearchResultBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "search_result" ->
        SearchResultBlock
          <$> o .:  "source"
          <*> o .:  "title"
          <*> o .:  "content"
          <*> o .:? "citations"
          <*> o .:? "cache_control"
      _ -> fail $ "Expected type \"search_result\", got: " ++ show typ
