-- | Internal: HTTP request construction and header injection.
--
-- __This module is not part of the public API.__ It is exposed for testability
-- but may change without notice.
module Anthropic.Client.Internal.Http
  ( -- * Request Construction
    buildRequest
  , buildGetRequest
  , buildDeleteRequest
  , baseUrl
  , apiVersion
  ) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Request
  , method, parseRequest_, requestBody, requestHeaders
  , responseTimeout, responseTimeoutMicro
  , RequestBody(RequestBodyLBS)
  )

import Anthropic.Types (ApiKey(..))

-- | The default Anthropic API base URL.
baseUrl :: Text
baseUrl = "https://api.anthropic.com"

-- | The API version string.
apiVersion :: ByteString
apiVersion = "2023-06-01"

-- | Build an HTTP request for a JSON POST endpoint.
--
-- Injects @x-api-key@, @anthropic-version@, @content-type@,
-- and optional @anthropic-beta@ headers.
buildRequest
  :: ToJSON a
  => ApiKey                           -- ^ API key
  -> Text                            -- ^ Base URL
  -> ByteString                      -- ^ Path (e.g., "/v1/messages")
  -> Maybe [(ByteString, ByteString)] -- ^ Additional default headers
  -> Maybe [Text]                    -- ^ Beta features
  -> Maybe Int                       -- ^ Timeout in seconds
  -> a                               -- ^ Request body (will be JSON-encoded)
  -> Request
buildRequest key base path extraHeaders betaFeatures timeoutSecs body =
  let req = buildGetRequest key base path extraHeaders betaFeatures timeoutSecs
  in req
    { method      = "POST"
    , requestBody = RequestBodyLBS (encode body)
    }

-- | Build an HTTP request for a GET endpoint (no body).
buildGetRequest
  :: ApiKey
  -> Text                             -- ^ Base URL
  -> ByteString                       -- ^ Path with query string
  -> Maybe [(ByteString, ByteString)] -- ^ Additional default headers
  -> Maybe [Text]                     -- ^ Beta features
  -> Maybe Int                        -- ^ Timeout in seconds
  -> Request
buildGetRequest key base path extraHeaders betaFeatures timeoutSecs =
  let ApiKey rawKey = key
      url = T.unpack base <> BS8.unpack path
      baseReq = parseRequest_ url
      hdrs = [ (CI.mk "x-api-key",        TE.encodeUtf8 rawKey)
             , (CI.mk "anthropic-version", apiVersion)
             , (CI.mk "content-type",     "application/json")
             ]
          ++ maybe [] (\betas -> [(CI.mk "anthropic-beta", TE.encodeUtf8 (T.intercalate "," betas))]) betaFeatures
          ++ maybe [] (map (\(k, v) -> (CI.mk k, v))) extraHeaders
      req = baseReq
        { method         = "GET"
        , requestHeaders = hdrs
        }
  in applyTimeout timeoutSecs req

-- | Build a DELETE request.
buildDeleteRequest
  :: ApiKey
  -> Text
  -> ByteString
  -> Maybe [(ByteString, ByteString)]
  -> Maybe [Text]
  -> Maybe Int
  -> Request
buildDeleteRequest key base path extraHeaders betaFeatures timeoutSecs =
  let req = buildGetRequest key base path extraHeaders betaFeatures timeoutSecs
  in req { method = "DELETE" }

applyTimeout :: Maybe Int -> Request -> Request
applyTimeout Nothing  req = req
applyTimeout (Just s) req = req { responseTimeout = responseTimeoutMicro (s * 1000000) }
