-- |
-- Module      : Network.JsonRpc.Types
-- Stability   : stable
-- Portability : GHC2021
--
-- Core types for the JSON-RPC 2.0 protocol. This module exports the
-- full type vocabulary used across the library: identifiers, parameters,
-- requests, notifications, responses, errors, and parse results.
--
-- The 'ErrorObject' constructor is hidden. Use the smart constructors
-- ('parseError', 'invalidRequest', etc.) to build error values with
-- guaranteed-correct code\/message pairings. For raw constructor access,
-- import "Network.JsonRpc.Types.Internal".
--
-- JSON serialization instances ('Data.Aeson.FromJSON', 'Data.Aeson.ToJSON')
-- are provided by "Network.JsonRpc.Codec". Import that module to
-- enable JSON encoding and decoding.
module Network.JsonRpc.Types
  ( -- * Id
    Id (..)

    -- * Params
  , Params (..)

    -- * Request and Notification
  , Request (..)
  , Notification (..)

    -- * Error
  , ErrorObject
  , errorCode
  , errorMessage
  , errorData
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

import Network.JsonRpc.Types.Internal
