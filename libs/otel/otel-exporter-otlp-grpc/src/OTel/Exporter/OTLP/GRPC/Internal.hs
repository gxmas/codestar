-- | Internal gRPC plumbing: status codes, framing, retry, and HTTP/2 client.
module OTel.Exporter.OTLP.GRPC.Internal
  ( -- * gRPC Status
    GrpcStatusCode (..)
  , GrpcStatus (..)
  , parseGrpcStatus
  , isRetryable

    -- * Frame encoding
  , Compression (..)
  , FrameError (..)
  , encodeFrame
  , decodeFrame

    -- * Retry
  , RetryConfig (..)
  , GrpcError (..)
  , defaultRetryConfig
  , withRetry

    -- * Client
  , TlsConfig (..)
  , GrpcClientConfig (..)
  , defaultGrpcClientConfig
  , unaryRpc
  ) where

import OTel.Exporter.OTLP.GRPC.Internal.Status
import OTel.Exporter.OTLP.GRPC.Internal.Frame
import OTel.Exporter.OTLP.GRPC.Internal.Retry
import OTel.Exporter.OTLP.GRPC.Internal.Client
