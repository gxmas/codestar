-- | Cache control types for prompt caching.
module Anthropic.Types.Cache
  ( -- * Cache Control
    CacheControl (..)
  , CacheTTL (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:), (.:?)
  , object, withObject, withScientific
  )
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Encoding as E
import GHC.Generics (Generic)

-- | TTL for ephemeral cache control.
data CacheTTL
  = TTL5Min   -- ^ 5 minutes (default)
  | TTL1Hour  -- ^ 1 hour (requires extended-cache-ttl beta)
  deriving stock (Eq, Show, Generic)

instance ToJSON CacheTTL where
  toJSON TTL5Min  = toJSON (300 :: Int)
  toJSON TTL1Hour = toJSON (3600 :: Int)
  toEncoding TTL5Min  = E.int 300
  toEncoding TTL1Hour = E.int 3600

instance FromJSON CacheTTL where
  parseJSON = withScientific "CacheTTL" $ \n ->
    case round n :: Int of
      300  -> pure TTL5Min
      3600 -> pure TTL1Hour
      _    -> fail $ "Unknown CacheTTL value: " ++ show n

-- | Cache control directive. Currently only @ephemeral@ type is supported.
data CacheControl = CacheControl
  { ttl :: !(Maybe CacheTTL)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CacheControl where
  toJSON cc = object $
    [ "type" .= ("ephemeral" :: String) ]
    ++ maybe [] (\t -> ["ttl" .= t]) cc.ttl
  toEncoding cc = E.pairs $
    "type" .= ("ephemeral" :: String)
    <> foldMap ("ttl" .=) cc.ttl

instance FromJSON CacheControl where
  parseJSON = withObject "CacheControl" $ \o -> do
    typ <- o .: "type" :: Parser String
    case typ of
      "ephemeral" -> CacheControl <$> o .:? "ttl"
      _           -> fail $ "Unknown CacheControl type: " ++ typ

