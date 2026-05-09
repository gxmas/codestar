module OTel.Exporter.OTLP.GRPC.Internal.Frame
  ( Compression (..)
  , FrameError (..)
  , encodeFrame
  , decodeFrame
  ) where

import Control.Exception (SomeException, catch, evaluate)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8, Word32)
import qualified Codec.Compression.GZip as GZip


-- | Compression algorithm for gRPC frames.
data Compression = NoCompression | GzipCompression
  deriving stock (Eq, Show)


-- | Errors from gRPC frame encoding or decoding.
data FrameError
  = FrameTooShort Int
  | FrameLengthMismatch
  | FrameDecompressionError String
  | FrameUnknownCompression Word8
  deriving stock (Eq, Show)


-- | Encode a payload into a gRPC length-prefixed frame.
encodeFrame :: Compression -> ByteString -> IO ByteString
encodeFrame compression payload = do
  let (flag, body) = case compression of
        NoCompression -> (0 :: Word8, payload)
        GzipCompression -> (1, LBS.toStrict (GZip.compress (LBS.fromStrict payload)))
      len = BS.length body
      header = BS.pack
        [ flag
        , fromIntegral (len `shiftR` 24 .&. 0xFF)
        , fromIntegral (len `shiftR` 16 .&. 0xFF)
        , fromIntegral (len `shiftR` 8 .&. 0xFF)
        , fromIntegral (len .&. 0xFF)
        ]
  pure (header <> body)


-- | Decode a gRPC length-prefixed frame.
decodeFrame :: ByteString -> IO (Either FrameError (Compression, ByteString))
decodeFrame bs
  | BS.length bs < 5 = pure (Left (FrameTooShort (BS.length bs)))
  | otherwise = do
      let flag = BS.index bs 0
          len = fromIntegral (word32BE (BS.index bs 1) (BS.index bs 2) (BS.index bs 3) (BS.index bs 4))
          payload = BS.drop 5 bs
      if BS.length payload /= len
        then pure (Left FrameLengthMismatch)
        else case flag of
          0 -> pure (Right (NoCompression, payload))
          1 -> decompressGzip payload
          other -> pure (Left (FrameUnknownCompression other))


word32BE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word32BE a b c d =
  fromIntegral a `shiftL` 24
    .|. fromIntegral b `shiftL` 16
    .|. fromIntegral c `shiftL` 8
    .|. fromIntegral d


decompressGzip :: ByteString -> IO (Either FrameError (Compression, ByteString))
decompressGzip input = do
  let decompress = LBS.toStrict (GZip.decompress (LBS.fromStrict input))
  (Right . (GzipCompression,) <$> evaluate decompress)
    `catch` \(e :: SomeException) ->
      pure (Left (FrameDecompressionError (show e)))
