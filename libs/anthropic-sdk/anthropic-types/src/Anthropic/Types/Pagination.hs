-- | Cursor-based pagination types for list endpoints.
module Anthropic.Types.Pagination
  ( -- * Page
    Page (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject
  )
import qualified Data.Aeson.Encoding as E
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A page of results from a list endpoint.
--
-- Use @lastId@ as @afterId@ for the next page.
-- Use @firstId@ as @beforeId@ for the previous page.
data Page a = Page
  { pageData :: ![a]
  , hasMore  :: !Bool
  , firstId  :: !(Maybe Text)
  , lastId   :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON a => ToJSON (Page a) where
  toJSON p = object $
    [ "data"     .= p.pageData
    , "has_more" .= p.hasMore
    ]
    ++ maybe [] (\x -> ["first_id" .= x]) p.firstId
    ++ maybe [] (\x -> ["last_id"  .= x]) p.lastId
  toEncoding p = E.pairs $
       "data"     .= p.pageData
    <> "has_more" .= p.hasMore
    <> foldMap ("first_id" .=) p.firstId
    <> foldMap ("last_id"  .=) p.lastId

instance FromJSON a => FromJSON (Page a) where
  parseJSON = withObject "Page" $ \o ->
    Page
      <$> o .:  "data"
      <*> o .:  "has_more"
      <*> o .:? "first_id"
      <*> o .:? "last_id"
