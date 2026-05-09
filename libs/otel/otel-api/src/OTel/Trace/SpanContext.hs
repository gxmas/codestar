-- |
-- Module: OTel.Trace.SpanContext
-- Description: Core trace identification types: TraceId, SpanId, TraceFlags, SpanContext.
--
-- These types represent the immutable identification and metadata for a span
-- within a distributed trace. They follow the W3C Trace Context specification.
module OTel.Trace.SpanContext
  ( -- * TraceId
    TraceId
  , traceIdFromBytes
  , traceIdToBytes
  , traceIdFromHex
  , traceIdToHex
  , invalidTraceId

    -- * SpanId
  , SpanId
  , spanIdFromBytes
  , spanIdToBytes
  , spanIdFromHex
  , spanIdToHex
  , invalidSpanId

    -- * TraceFlags
  , TraceFlags
  , traceFlagsFromByte
  , traceFlagsToByte
  , isSampled
  , sampledFlag
  , emptyTraceFlags

    -- * SpanContext
  , SpanContext (..)
  , invalidSpanContext
  , isValid
  , isRemote
  ) where

import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import OTel.Trace.TraceState (TraceState)
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- TraceId
-------------------------------------------------------------------------------

-- | A 128-bit (16-byte) trace identifier. All-zero is invalid.
newtype TraceId = TraceId ByteString
  deriving stock (Eq, Ord)


instance Show TraceId where
  show = Text.unpack . traceIdToHex


-- | The all-zero 'TraceId', representing an invalid trace identifier.
invalidTraceId :: TraceId
invalidTraceId = TraceId (BS.replicate 16 0)


-- | Create a 'TraceId' from exactly 16 bytes.
-- Returns 'invalidTraceId' if the input is not exactly 16 bytes.
traceIdFromBytes :: ByteString -> TraceId
traceIdFromBytes bs
  | BS.length bs == 16 = TraceId bs
  | otherwise = invalidTraceId


-- | Extract the 16 raw bytes from a 'TraceId'.
traceIdToBytes :: TraceId -> ByteString
traceIdToBytes (TraceId bs) = bs


-- | Create a 'TraceId' from a 32-character lowercase hex string.
-- Returns 'invalidTraceId' on invalid input (wrong length or non-hex characters).
traceIdFromHex :: Text -> TraceId
traceIdFromHex hex
  | Text.length hex == 32
  , Just bs <- hexToBytes hex
  = TraceId bs
  | otherwise = invalidTraceId


-- | Convert a 'TraceId' to a 32-character lowercase hex string.
traceIdToHex :: TraceId -> Text
traceIdToHex (TraceId bs) = bytesToHex bs


-------------------------------------------------------------------------------
-- SpanId
-------------------------------------------------------------------------------

-- | A 64-bit (8-byte) span identifier. All-zero is invalid.
newtype SpanId = SpanId ByteString
  deriving stock (Eq, Ord)


instance Show SpanId where
  show = Text.unpack . spanIdToHex


-- | The all-zero 'SpanId', representing an invalid span identifier.
invalidSpanId :: SpanId
invalidSpanId = SpanId (BS.replicate 8 0)


-- | Create a 'SpanId' from exactly 8 bytes.
-- Returns 'invalidSpanId' if the input is not exactly 8 bytes.
spanIdFromBytes :: ByteString -> SpanId
spanIdFromBytes bs
  | BS.length bs == 8 = SpanId bs
  | otherwise = invalidSpanId


-- | Extract the 8 raw bytes from a 'SpanId'.
spanIdToBytes :: SpanId -> ByteString
spanIdToBytes (SpanId bs) = bs


-- | Create a 'SpanId' from a 16-character lowercase hex string.
-- Returns 'invalidSpanId' on invalid input (wrong length or non-hex characters).
spanIdFromHex :: Text -> SpanId
spanIdFromHex hex
  | Text.length hex == 16
  , Just bs <- hexToBytes hex
  = SpanId bs
  | otherwise = invalidSpanId


-- | Convert a 'SpanId' to a 16-character lowercase hex string.
spanIdToHex :: SpanId -> Text
spanIdToHex (SpanId bs) = bytesToHex bs


-------------------------------------------------------------------------------
-- TraceFlags
-------------------------------------------------------------------------------

-- | A single-byte bitmask carrying trace flags. Currently only the sampled
-- bit (bit 0) is defined by the W3C specification.
newtype TraceFlags = TraceFlags Word8
  deriving stock (Eq, Ord, Show)


-- | Empty trace flags with no bits set.
emptyTraceFlags :: TraceFlags
emptyTraceFlags = TraceFlags 0


-- | Trace flags with the sampled bit (bit 0) set.
sampledFlag :: TraceFlags
sampledFlag = TraceFlags 1


-- | Wrap a raw byte as 'TraceFlags'.
traceFlagsFromByte :: Word8 -> TraceFlags
traceFlagsFromByte = TraceFlags


-- | Extract the raw byte from 'TraceFlags'.
traceFlagsToByte :: TraceFlags -> Word8
traceFlagsToByte (TraceFlags w) = w


-- | Check whether the sampled bit (bit 0) is set.
isSampled :: TraceFlags -> Bool
isSampled (TraceFlags w) = w .&. 1 /= 0


-------------------------------------------------------------------------------
-- SpanContext
-------------------------------------------------------------------------------

-- | Immutable representation of the portion of a 'Span' that must be
-- serialized and propagated across process boundaries.
data SpanContext = SpanContext
  { traceId :: !TraceId
  , spanId :: !SpanId
  , traceFlags :: !TraceFlags
  , traceState :: !TraceState
  , _isRemote :: !Bool
  }
  deriving stock (Eq, Show)


-- | An invalid 'SpanContext' with all-zero identifiers, empty flags,
-- empty trace state, and not remote.
invalidSpanContext :: SpanContext
invalidSpanContext =
  SpanContext
    { traceId = invalidTraceId
    , spanId = invalidSpanId
    , traceFlags = emptyTraceFlags
    , traceState = TraceState.empty
    , _isRemote = False
    }


-- | A 'SpanContext' is valid when both its 'TraceId' and 'SpanId' are
-- non-zero (not equal to their respective invalid values).
isValid :: SpanContext -> Bool
isValid sc = sc.traceId /= invalidTraceId && sc.spanId /= invalidSpanId


-- | Whether this context was received from a remote parent.
isRemote :: SpanContext -> Bool
isRemote = _isRemote


-------------------------------------------------------------------------------
-- Hex encoding helpers (internal)
-------------------------------------------------------------------------------

-- | Encode a 'ByteString' as lowercase hex 'Text'.
bytesToHex :: ByteString -> Text
bytesToHex = Text.decodeUtf8 . BS.concatMap encodeWord8
  where
    encodeWord8 :: Word8 -> ByteString
    encodeWord8 w = BS.pack [hexDigit (shiftR w 4), hexDigit (w .&. 0x0f)]

    hexDigit :: Word8 -> Word8
    hexDigit n = BS.index "0123456789abcdef" (fromIntegral n)


-- | Decode hex 'Text' to a 'ByteString'. Returns 'Nothing' on invalid hex.
hexToBytes :: Text -> Maybe ByteString
hexToBytes hex
  | Text.all isHexDigit hex
  , even (Text.length hex)
  = Just (BS.pack (go (Text.unpack hex)))
  | otherwise = Nothing
  where
    go :: [Char] -> [Word8]
    go (hi : lo : rest) =
      fromIntegral (digitToInt hi * 16 + digitToInt lo) : go rest
    go _ = []
