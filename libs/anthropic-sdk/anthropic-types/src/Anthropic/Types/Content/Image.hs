-- | Image content block types.
module Anthropic.Types.Content.Image
  ( -- * Image Block
    ImageBlock (..)

    -- * Image Source
  , ImageSource (..)

    -- * Media Type
  , MediaType (..)
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

-- | Supported image media types.
data MediaType
  = ImageJpeg
  | ImagePng
  | ImageGif
  | ImageWebp
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON MediaType where
  toJSON ImageJpeg = "image/jpeg"
  toJSON ImagePng  = "image/png"
  toJSON ImageGif  = "image/gif"
  toJSON ImageWebp = "image/webp"
  toEncoding ImageJpeg = E.text "image/jpeg"
  toEncoding ImagePng  = E.text "image/png"
  toEncoding ImageGif  = E.text "image/gif"
  toEncoding ImageWebp = E.text "image/webp"

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \case
    "image/jpeg" -> pure ImageJpeg
    "image/png"  -> pure ImagePng
    "image/gif"  -> pure ImageGif
    "image/webp" -> pure ImageWebp
    other        -> fail $ "Unknown MediaType: " ++ T.unpack other

-- | Source of an image: base64-encoded data or URL.
data ImageSource
  = Base64Image !MediaType !Text
    -- ^ Media type and base64-encoded data
  | UrlImage !Text
    -- ^ URL to the image
  deriving stock (Eq, Show, Generic)

instance ToJSON ImageSource where
  toJSON (Base64Image mt d) = object
    [ "type"       .= ("base64" :: Text)
    , "media_type" .= mt
    , "data"       .= d
    ]
  toJSON (UrlImage u) = object
    [ "type" .= ("url" :: Text)
    , "url"  .= u
    ]
  toEncoding (Base64Image mt d) = E.pairs $
       "type"       .= ("base64" :: Text)
    <> "media_type" .= mt
    <> "data"       .= d
  toEncoding (UrlImage u) = E.pairs $
       "type" .= ("url" :: Text)
    <> "url"  .= u

instance FromJSON ImageSource where
  parseJSON = withObject "ImageSource" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "base64" -> Base64Image <$> o .: "media_type" <*> o .: "data"
      "url"    -> UrlImage    <$> o .: "url"
      _        -> fail $ "Unknown ImageSource type: " ++ T.unpack typ

-- | An image content block.
data ImageBlock = ImageBlock
  { source       :: !ImageSource
  , cacheControl :: !(Maybe CacheControl)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ImageBlock where
  toJSON ib = object $
    [ "type"   .= ("image" :: Text)
    , "source" .= ib.source
    ]
    ++ maybe [] (\cc -> ["cache_control" .= cc]) ib.cacheControl
  toEncoding ib = E.pairs $
       "type"   .= ("image" :: Text)
    <> "source" .= ib.source
    <> foldMap ("cache_control" .=) ib.cacheControl

instance FromJSON ImageBlock where
  parseJSON = withObject "ImageBlock" $ \o -> do
    typ <- o .: "type" :: Parser Text
    case typ of
      "image" ->
        ImageBlock
          <$> o .:  "source"
          <*> o .:? "cache_control"
      _ -> fail $ "Expected type \"image\", got: " ++ show typ
