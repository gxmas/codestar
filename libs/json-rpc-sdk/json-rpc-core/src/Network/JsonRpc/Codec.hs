{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.JsonRpc.Codec
-- Stability   : stable
-- Portability : GHC2021
--
-- JSON encoding and decoding for JSON-RPC 2.0 messages.
--
-- Importing this module brings 'Data.Aeson.FromJSON' and
-- 'Data.Aeson.ToJSON' instances for all types in
-- "Network.JsonRpc.Types" into scope.
--
-- The primary entry point is 'parse', which takes raw bytes and
-- produces a 'ParseResult'. For serialization, use 'serialize',
-- 'serializeResponse', or 'serializeBatch'.
--
-- @
-- import Network.JsonRpc.Types
-- import Network.JsonRpc.Codec
--
-- example :: ByteString -> Maybe ByteString
-- example input = case parse input of
--   SingleMessage msg -> Just (serialize msg)
--   BatchMessage _    -> Nothing
--   ParseFailed _     -> Nothing
-- @
module Network.JsonRpc.Codec
  ( -- * Parsing
    parse

    -- * Serialization
  , serialize
  , serializeResponse
  , serializeBatch
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Enc
import Data.ByteString.Lazy (ByteString)
import Network.JsonRpc.Codec.Internal ()
import Network.JsonRpc.Codec.Internal qualified as Internal
import Network.JsonRpc.Types.Internal

-- | Parse a lazy 'ByteString' containing JSON into a 'ParseResult'.
--
-- This function is pure and total: every input produces a
-- 'ParseResult'. Invalid JSON yields 'ParseFailed' with a parse
-- error. Valid JSON that is not a valid JSON-RPC message yields
-- appropriate error responses per the spec.
--
-- Validation follows the order specified in the JSON-RPC 2.0
-- specification.
parse :: ByteString -> ParseResult
parse bs = case Aeson.eitherDecode' bs of
  Left _ -> ParseFailed (parseError Nothing)
  Right val -> Internal.parseValue val

-- | Serialize a 'Message' to a lazy 'ByteString' containing JSON.
--
-- This function cannot fail: the type system guarantees the message
-- is well-formed.
serialize :: Message -> ByteString
serialize = Aeson.encode

-- | Serialize a 'Response' to a lazy 'ByteString' containing JSON.
--
-- Convenience function for the common server case where you have a
-- 'Response' rather than a full 'Message'.
serializeResponse :: Response -> ByteString
serializeResponse = Aeson.encode

-- | Serialize a list of 'Message' values as a JSON array (batch).
--
-- An empty list produces @[]@. The caller is responsible for
-- ensuring the batch is non-empty per the JSON-RPC spec when sending
-- to a server.
serializeBatch :: [Message] -> ByteString
serializeBatch msgs =
  Enc.encodingToLazyByteString $
    Enc.list Aeson.toEncoding msgs
