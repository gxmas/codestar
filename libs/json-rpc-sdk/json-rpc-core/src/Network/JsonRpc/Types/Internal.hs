{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.JsonRpc.Types.Internal
-- Stability   : unstable
-- Portability : GHC2021
--
-- __This is an internal module.__ The API may change between minor
-- versions without notice. Import "Network.JsonRpc.Types" for the
-- stable public interface.
--
-- This module exports all constructors, including 'ErrorObject' which
-- is hidden from the public API. Use this module when you need to
-- construct 'ErrorObject' values with custom code\/message pairings
-- that the smart constructors do not cover.
--
-- JSON serialization instances are provided by "Network.JsonRpc.Codec".
module Network.JsonRpc.Types.Internal
  ( -- * Id
    Id (..)

    -- * Params
  , Params (..)

    -- * Request and Notification
  , Request (..)
  , Notification (..)

    -- * Error
  , ErrorObject (..)
  , RangeError (..)

    -- * Standard error constructors
  , parseError
  , invalidRequest
  , methodNotFound
  , invalidParams
  , internalError

    -- * Range-checked error constructors
  , serverError
  , applicationError

    -- * Response
  , Success (..)
  , Failure (..)
  , Response (..)

    -- * Message
  , Message (..)

    -- * Parse result
  , ParseResult (..)
  , BatchElement (..)
  ) where

import Data.Aeson (Value)
import Data.Aeson.KeyMap (KeyMap)
import Data.Hashable (Hashable (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)

-- --------------------------------------------------------------------
-- Id
-- --------------------------------------------------------------------

-- | A JSON-RPC request identifier. The spec allows strings and numbers.
--
-- @'Id'@ is used as a @'Data.HashMap.Strict.HashMap'@ key in the
-- client's pending-request map, so it has 'Eq', 'Ord', and 'Hashable'
-- instances.
--
-- There is no @IdNull@ constructor. Null ids are represented via
-- @'Maybe' 'Id'@ on the types that need them ('Request', 'Success',
-- 'Failure'). See ADR-004.
data Id
  = -- | A string identifier.
    IdText !Text
  | -- | A numeric identifier. The spec allows fractional numbers,
    -- though it discourages them.
    IdNum !Scientific
  deriving (Eq, Ord, Show)

-- | Tag byte ensures @IdText \"1\"@ and @IdNum 1@ hash differently.
instance Hashable Id where
  hashWithSalt s (IdText t) = hashWithSalt s (0 :: Int, t)
  hashWithSalt s (IdNum n) = hashWithSalt s (1 :: Int, n)

-- --------------------------------------------------------------------
-- Params
-- --------------------------------------------------------------------

-- | Method parameters. Three-state representation that is honest about
-- the wire format:
--
-- * 'ParamsByPosition' -- a JSON Array
-- * 'ParamsByName' -- a JSON Object
-- * 'NoParams' -- the @params@ field is absent
--
-- Using a three-state type avoids splitting the \"no params\" concept
-- across a @Maybe@ wrapper and a two-state sum.
data Params
  = -- | Positional parameters (JSON Array).
    ParamsByPosition !(Vector Value)
  | -- | Named parameters (JSON Object). Uses aeson's 'KeyMap' for
    -- forward compatibility across aeson versions.
    ParamsByName !(KeyMap Value)
  | -- | The @params@ field was absent.
    NoParams
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Request and Notification
-- --------------------------------------------------------------------

-- | A JSON-RPC 2.0 request. Distinguished from 'Notification' by the
-- presence of an @id@ field.
--
-- 'requestId' is @'Maybe' 'Id'@ because the spec allows @\"id\": null@
-- on requests (section 5). 'Nothing' represents a null id; @'Just' i@
-- represents a non-null id. See ADR-004.
data Request = Request
  { requestMethod :: !Text
  , requestParams :: !Params
  , requestId :: !(Maybe Id)
  }
  deriving (Eq, Show)

-- | A JSON-RPC 2.0 notification. A notification has no @id@ field and
-- the server MUST NOT reply.
data Notification = Notification
  { notificationMethod :: !Text
  , notificationParams :: !Params
  }
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- ErrorObject
-- --------------------------------------------------------------------

-- | A JSON-RPC 2.0 error object (spec section 5.1).
--
-- The constructor is hidden from the public API. Use the smart
-- constructors ('parseError', 'invalidRequest', etc.) to build values
-- with guaranteed-correct code\/message pairings. Import
-- "Network.JsonRpc.Types.Internal" for raw constructor access.
data ErrorObject = ErrorObject
  { -- | The error code (an integer).
    errorCode :: !Int
  , -- | A short description of the error.
    errorMessage :: !Text
  , -- | Additional information about the error (optional).
    errorData :: !(Maybe Value)
  }
  deriving (Eq, Show)

-- | Indicates that a code was outside the permitted range for
-- 'serverError' or 'applicationError'.
data RangeError = RangeError
  { -- | The code that was rejected.
    givenCode :: !Int
  , -- | The inclusive range that would have been accepted.
    allowedRange :: !(Int, Int)
  }
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Standard error constructors
-- --------------------------------------------------------------------

-- | Parse error (@-32700@). Invalid JSON was received by the server.
parseError :: Maybe Value -> ErrorObject
parseError = ErrorObject (-32700) "Parse error"

-- | Invalid Request (@-32600@). The JSON sent is not a valid Request
-- object.
invalidRequest :: Maybe Value -> ErrorObject
invalidRequest = ErrorObject (-32600) "Invalid Request"

-- | Method not found (@-32601@). The method does not exist or is not
-- available.
methodNotFound :: Maybe Value -> ErrorObject
methodNotFound = ErrorObject (-32601) "Method not found"

-- | Invalid params (@-32602@). Invalid method parameter(s).
invalidParams :: Maybe Value -> ErrorObject
invalidParams = ErrorObject (-32602) "Invalid params"

-- | Internal error (@-32603@). Internal JSON-RPC error.
internalError :: Maybe Value -> ErrorObject
internalError = ErrorObject (-32603) "Internal error"

-- --------------------------------------------------------------------
-- Range-checked error constructors
-- --------------------------------------------------------------------

-- | Construct a server-defined error. The code must be in the range
-- @-32099@ to @-32000@ (inclusive). Returns 'Left' 'RangeError' if the
-- code is out of range.
serverError :: Int -> Text -> Maybe Value -> Either RangeError ErrorObject
serverError code msg d
  | code >= -32099 && code <= -32000 = Right (ErrorObject code msg d)
  | otherwise = Left (RangeError code (-32099, -32000))

-- | Construct an application-defined error. The code must NOT be in
-- the reserved range @-32768@ to @-32000@ (inclusive). Returns 'Left'
-- 'RangeError' if the code falls within the reserved range.
applicationError :: Int -> Text -> Maybe Value -> Either RangeError ErrorObject
applicationError code msg d
  | code >= -32768 && code <= -32000 = Left (RangeError code (-32768, -32000))
  | otherwise = Right (ErrorObject code msg d)

-- --------------------------------------------------------------------
-- Response
-- --------------------------------------------------------------------

-- | A successful JSON-RPC 2.0 response.
data Success = Success
  { -- | The result value. The spec requires this field on success
    -- responses; it can be any JSON value including @null@.
    successResult :: !Value
  , -- | The request id echoed back. @Nothing@ means @\"id\": null@.
    successId :: !(Maybe Id)
  }
  deriving (Eq, Show)

-- | A failed JSON-RPC 2.0 response.
data Failure = Failure
  { -- | The error object describing the failure.
    failureError :: !ErrorObject
  , -- | The request id echoed back. @Nothing@ when the id could not
    -- be determined (e.g., parse errors).
    failureId :: !(Maybe Id)
  }
  deriving (Eq, Show)

-- | A JSON-RPC 2.0 response: either a success or a failure.
data Response
  = -- | A successful response containing a result.
    ResponseSuccess !Success
  | -- | A failed response containing an error.
    ResponseFailure !Failure
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Message
-- --------------------------------------------------------------------

-- | A JSON-RPC 2.0 message. This is the top-level sum type that
-- covers all message kinds.
data Message
  = -- | A request (expects a response).
    MsgRequest !Request
  | -- | A notification (no response expected).
    MsgNotification !Notification
  | -- | A response to a previous request.
    MsgResponse !Response
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Parse result
-- --------------------------------------------------------------------

-- | The result of parsing raw bytes into JSON-RPC messages. Consumed
-- by the server's @handleRaw@ function.
data ParseResult
  = -- | A single JSON-RPC message.
    SingleMessage !Message
  | -- | A batch of JSON-RPC messages (spec section 6).
    BatchMessage ![BatchElement]
  | -- | The input could not be parsed as valid JSON-RPC at all.
    ParseFailed !ErrorObject
  deriving (Eq, Show)

-- | A single element within a batch. Each element is validated
-- independently; invalid elements produce error responses rather than
-- being silently dropped.
data BatchElement
  = -- | A successfully parsed message.
    ValidElement !Message
  | -- | An element that failed validation. Carries a 'Failure' (not
    -- just an 'ErrorObject') because the response needs the @id@ field.
    InvalidElement !Failure
  deriving (Eq, Show)
