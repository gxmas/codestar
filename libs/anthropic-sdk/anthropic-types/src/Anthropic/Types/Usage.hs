-- | Usage, rate limit, and response metadata types.
module Anthropic.Types.Usage
  ( -- * Usage
    Usage (..)
  , ServerToolUsage (..)
  , CacheCreationUsage (..)

    -- * Rate Limit Info
  , RateLimitInfo (..)

    -- * Response Metadata
  , ResponseMeta (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..)
  , defaultOptions, genericToJSON, genericToEncoding, genericParseJSON
  , object, withObject, (.=), (.:)
  )
import Data.Aeson.Types (Options(..), camelTo2)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Anthropic.Types.Core (RequestId)

-- | Server tool usage breakdown.
data ServerToolUsage = ServerToolUsage
  { webSearchRequests :: !Int
  , webFetchRequests  :: !Int
  }
  deriving stock (Eq, Show, Generic)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

instance ToJSON ServerToolUsage where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON ServerToolUsage where
  parseJSON = genericParseJSON customOptions

-- | Cache creation usage breakdown by TTL.
data CacheCreationUsage = CacheCreationUsage
  { ephemeral5mInputTokens :: !Int
  , ephemeral1hInputTokens :: !Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CacheCreationUsage where
  toJSON (CacheCreationUsage e5m e1h) = object
    [ "ephemeral_5m_input_tokens" .= e5m
    , "ephemeral_1h_input_tokens" .= e1h
    ]

instance FromJSON CacheCreationUsage where
  parseJSON = withObject "CacheCreationUsage" $ \o ->
    CacheCreationUsage
      <$> o .: "ephemeral_5m_input_tokens"
      <*> o .: "ephemeral_1h_input_tokens"

-- | Token usage for a request/response.
data Usage = Usage
  { inputTokens                :: !Int
  , outputTokens               :: !Int
  , cacheCreationInputTokens   :: !(Maybe Int)
  , cacheReadInputTokens       :: !(Maybe Int)
  , cacheCreation              :: !(Maybe CacheCreationUsage)
  , serverToolUse              :: !(Maybe ServerToolUsage)
  , serviceTier                :: !(Maybe Text)
  , inferenceGeo               :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Usage where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Usage where
  parseJSON = genericParseJSON customOptions

-- | Rate limit information parsed from response headers.
--
-- Present on every API response (not just 429s).
data RateLimitInfo = RateLimitInfo
  { requestsLimit          :: !Int
  , requestsRemaining      :: !Int
  , requestsReset          :: !UTCTime
  , inputTokensLimit       :: !Int
  , inputTokensRemaining   :: !Int
  , inputTokensReset       :: !UTCTime
  , outputTokensLimit      :: !(Maybe Int)
  , outputTokensRemaining  :: !(Maybe Int)
  , outputTokensReset      :: !(Maybe UTCTime)
  , tokensLimit            :: !(Maybe Int)
  , tokensRemaining        :: !(Maybe Int)
  , tokensReset            :: !(Maybe UTCTime)
  , retryAfter             :: !(Maybe Int)
    -- ^ Seconds to wait before retrying (present on 429 only)
  }
  deriving stock (Eq, Show)

-- | Metadata extracted from response headers.
--
-- Available via the @onResponseMeta@ callback in 'ClientConfig'
-- and via 'getRateLimits'.
data ResponseMeta = ResponseMeta
  { rateLimits :: !RateLimitInfo
  , requestId  :: !RequestId
  }
  deriving stock (Eq, Show)
