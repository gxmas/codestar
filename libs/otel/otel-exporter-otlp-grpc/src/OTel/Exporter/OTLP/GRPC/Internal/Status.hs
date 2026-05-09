module OTel.Exporter.OTLP.GRPC.Internal.Status
  ( GrpcStatusCode (..)
  , GrpcStatus (..)
  , parseGrpcStatus
  , isRetryable
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Types (urlDecode)


-- | Standard gRPC status codes per the gRPC specification.
data GrpcStatusCode
  = GrpcOk
  | GrpcCancelled
  | GrpcUnknown
  | GrpcInvalidArgument
  | GrpcDeadlineExceeded
  | GrpcNotFound
  | GrpcAlreadyExists
  | GrpcPermissionDenied
  | GrpcResourceExhausted
  | GrpcFailedPrecondition
  | GrpcAborted
  | GrpcOutOfRange
  | GrpcUnimplemented
  | GrpcInternal
  | GrpcUnavailable
  | GrpcDataLoss
  | GrpcUnauthenticated
  deriving stock (Eq, Ord, Show, Enum, Bounded)


-- | Parsed gRPC status with code and optional message.
data GrpcStatus = GrpcStatus
  { statusCode :: GrpcStatusCode
  , statusMessage :: Text
  } deriving stock (Eq, Show)


-- | Parse gRPC status from HTTP/2 response trailers.
parseGrpcStatus :: [(ByteString, ByteString)] -> GrpcStatus
parseGrpcStatus hdrs =
  GrpcStatus
    { statusCode = code
    , statusMessage = msg
    }
  where
    code = case lookup "grpc-status" hdrs of
      Nothing -> GrpcOk
      Just bs -> case readStatusCode bs of
        Nothing -> GrpcUnknown
        Just c -> c
    msg = case lookup "grpc-message" hdrs of
      Nothing -> T.empty
      Just bs -> case TE.decodeUtf8' (urlDecode False bs) of
        Left _ -> T.empty
        Right t -> t


readStatusCode :: ByteString -> Maybe GrpcStatusCode
readStatusCode bs = case B8.readInt bs of
  Just (n, rest) | B8.null rest, n >= 0, n <= 16 -> Just (toEnum n)
  _ -> Nothing


-- | Whether a gRPC status code should trigger a retry.
isRetryable :: GrpcStatusCode -> Bool
isRetryable GrpcCancelled = True
isRetryable GrpcDeadlineExceeded = True
isRetryable GrpcResourceExhausted = True
isRetryable GrpcAborted = True
isRetryable GrpcOutOfRange = True
isRetryable GrpcUnavailable = True
isRetryable GrpcDataLoss = True
isRetryable _ = False
