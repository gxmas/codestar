-- |
-- Module      : Network.MCP.Codec
-- Stability   : stable
--
-- Codec typeclass and default aeson-based implementation for MCP messages.
module Network.MCP.Codec
  ( -- * Error types
    CodecError (..)
  , CodecErrorKind (..)

    -- * Codec typeclass
  , Codec (..)

    -- * Default implementation
  , McpCodec (..)
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Types (MCPMessage)

-- | Classification of codec errors.
data CodecErrorKind
  = MalformedJson
  | InvalidEnvelope
  | UnknownMethod
  | InvalidParams
  deriving stock (Eq, Show, Bounded, Enum, Generic)

-- | A codec error with kind and human-readable detail.
data CodecError = CodecError
  { codecErrorKind :: !CodecErrorKind
  , codecErrorDetail :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Typeclass for encoding and decoding MCP messages.
class Codec c where
  -- | Encode an 'MCPMessage' to bytes.
  encode :: c -> MCPMessage -> Either CodecError ByteString

  -- | Decode bytes to an 'MCPMessage'.
  decode :: c -> ByteString -> Either CodecError MCPMessage

-- | Default codec implementation using aeson.
data McpCodec = McpCodec
  deriving stock (Eq, Show)

instance Codec McpCodec where
  encode _ msg = Right (Aeson.encode msg)

  decode _ bs = case Aeson.eitherDecode' bs of
    Left err ->
      let detail = T.pack err
       in if isJsonSyntaxError err
            then Left (CodecError MalformedJson detail)
            else Left (CodecError InvalidEnvelope detail)
    Right msg -> Right msg

-- | Heuristic: aeson syntax errors mention "Failed reading" or
-- similar; envelope errors mention field names.
isJsonSyntaxError :: String -> Bool
isJsonSyntaxError err =
  any (`elem` markers) (words err)
  where
    markers =
      [ "Failed"
      , "not"
      , "parse"
      , "lexical"
      , "unexpected"
      ]
