-- | API error types.
--
-- These types represent the structured error responses returned by the
-- Anthropic API. They are used by both client (in 'ClientError') and
-- server (for error response construction) packages.
module Anthropic.Types.Error
  ( -- * Error Types
    ErrorType (..)
  , ApiError (..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.=), (.:)
  , object, withObject, withText
  )
import qualified Data.Aeson.Encoding as E
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Classification of API errors by HTTP status code.
data ErrorType
  = InvalidRequestError    -- ^ 400
  | AuthenticationError    -- ^ 401
  | BillingError           -- ^ 402
  | PermissionError        -- ^ 403
  | NotFoundError          -- ^ 404
  | RequestTooLargeError   -- ^ 413
  | RateLimitError         -- ^ 429
  | InternalApiError       -- ^ 500
  | OverloadedError        -- ^ 529
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance ToJSON ErrorType where
  toJSON InvalidRequestError  = "invalid_request_error"
  toJSON AuthenticationError  = "authentication_error"
  toJSON BillingError         = "billing_error"
  toJSON PermissionError      = "permission_error"
  toJSON NotFoundError        = "not_found_error"
  toJSON RequestTooLargeError = "request_too_large"
  toJSON RateLimitError       = "rate_limit_error"
  toJSON InternalApiError     = "api_error"
  toJSON OverloadedError      = "overloaded_error"
  toEncoding InvalidRequestError  = E.text "invalid_request_error"
  toEncoding AuthenticationError  = E.text "authentication_error"
  toEncoding BillingError         = E.text "billing_error"
  toEncoding PermissionError      = E.text "permission_error"
  toEncoding NotFoundError        = E.text "not_found_error"
  toEncoding RequestTooLargeError = E.text "request_too_large"
  toEncoding RateLimitError       = E.text "rate_limit_error"
  toEncoding InternalApiError     = E.text "api_error"
  toEncoding OverloadedError      = E.text "overloaded_error"

instance FromJSON ErrorType where
  parseJSON = withText "ErrorType" $ \case
    "invalid_request_error" -> pure InvalidRequestError
    "authentication_error"  -> pure AuthenticationError
    "billing_error"         -> pure BillingError
    "permission_error"      -> pure PermissionError
    "not_found_error"       -> pure NotFoundError
    "request_too_large"     -> pure RequestTooLargeError
    "rate_limit_error"      -> pure RateLimitError
    "api_error"             -> pure InternalApiError
    "overloaded_error"      -> pure OverloadedError
    other                   -> fail $ "Unknown ErrorType: " ++ T.unpack other

-- | Structured error returned by the Anthropic API.
data ApiError = ApiError
  { errorType    :: !ErrorType
  , errorMessage :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ApiError where
  toJSON err = object
    [ "type"    .= err.errorType
    , "message" .= err.errorMessage
    ]
  toEncoding err = E.pairs
    ( "type"    .= err.errorType
   <> "message" .= err.errorMessage
    )

instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \o ->
    ApiError
      <$> o .: "type"
      <*> o .: "message"
