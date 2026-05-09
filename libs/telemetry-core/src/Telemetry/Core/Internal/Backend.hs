{-# LANGUAGE RankNTypes #-}

module Telemetry.Core.Internal.Backend
  ( Backend (..)
  ) where

import Control.Exception (SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Telemetry.Core.Types

data Backend = Backend
  { bWithSpan          :: forall a. SpanName -> [(Text, AttributeValue)] -> IO a -> IO a
  , bStartSpan         :: SpanName -> [(Text, AttributeValue)] -> IO Span
  , bEndSpan           :: Span -> IO ()
  , bAddAttribute      :: Span -> Text -> AttributeValue -> IO ()
  , bRecordException   :: Span -> SomeException -> IO ()
  , bGetCurrentSpanCtx :: IO (Maybe SpanContext)
  , bLog               :: Severity -> LogMessage -> [(Text, AttributeValue)] -> IO ()
  , bIncrementCounter  :: CounterName -> Int -> [(Text, AttributeValue)] -> IO ()
  , bAddCounter        :: CounterName -> Double -> [(Text, AttributeValue)] -> IO ()
  , bRecordGauge       :: GaugeName -> Double -> [(Text, AttributeValue)] -> IO ()
  , bRecordHistogram   :: HistogramName -> Double -> [(Text, AttributeValue)] -> IO ()
  , bExportMetrics     :: IO ByteString
  , bShutdown          :: IO ()
  }
