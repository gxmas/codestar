{-# LANGUAGE DeriveAnyClass #-}

module Telemetry.Core.Types
  ( -- * Initialization
    TelemetryConfig (..)
  , OtelSettings (..)
  , TelemetryHandle (..)

    -- * Tracing
  , Span (..)
  , SpanData (..)
  , SpanContext (..)
  , SpanName (..)
  , TraceId (..)
  , SpanId (..)
  , AttributeValue (..)

    -- * Logging
  , Severity (..)
  , LogMessage (..)

    -- * Metrics
  , CounterName (..)
  , GaugeName (..)
  , HistogramName (..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import OpenTelemetry.Trace qualified as OTel

-- ---------------------------------------------------------------------------
-- Initialization
-- ---------------------------------------------------------------------------

data TelemetryConfig
  = NoOpConfig
  | OpenTelemetryConfig !OtelSettings
  deriving stock (Eq, Show)

data OtelSettings = OtelSettings
  { serviceName    :: !Text
  , endpoint       :: !(Maybe Text)
  , logToStderr    :: !Bool
  , metricsEnabled :: !Bool
  , metricsBindHost :: !Text
  , metricsPort    :: !(Maybe Int)
  } deriving stock (Eq, Show, Generic)

data TelemetryHandle = TelemetryHandle
  { shutdownAction :: !(IO ())
  , metricsPort    :: !(Maybe Int)
  }

-- ---------------------------------------------------------------------------
-- Tracing
-- ---------------------------------------------------------------------------

data Span
  = NoOpSpan
  | RealSpan !SpanData

data SpanData = SpanData
  { sdContext    :: !SpanContext
  , sdStartTime  :: !UTCTime
  , sdAttributes :: !(IORef (Map Text AttributeValue))
  , sdOtelSpan   :: !(Maybe OTel.Span)
  }

data SpanContext = SpanContext
  { traceId :: !TraceId
  , spanId  :: !SpanId
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype SpanName = SpanName { unSpanName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

newtype TraceId = TraceId { unTraceId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

newtype SpanId = SpanId { unSpanId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

data AttributeValue
  = TextValue !Text
  | IntValue !Int
  | DoubleValue !Double
  | BoolValue !Bool
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

data Severity = DEBUG | INFO | WARN | ERROR
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LogMessage = LogMessage { unLogMessage :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

-- ---------------------------------------------------------------------------
-- Metrics
-- ---------------------------------------------------------------------------

newtype CounterName = CounterName { unCounterName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

newtype GaugeName = GaugeName { unGaugeName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)

newtype HistogramName = HistogramName { unHistogramName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)
