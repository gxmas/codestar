module Telemetry.Core.Internal.NoOp
  ( noOpBackend
  ) where

import Data.ByteString.Lazy (ByteString)
import Telemetry.Core.Internal.Backend (Backend (..))
import Telemetry.Core.Types (Span (..))

noOpBackend :: Backend
noOpBackend = Backend
  { bWithSpan          = \_ _ action -> action
  , bStartSpan         = \_ _ -> pure NoOpSpan
  , bEndSpan           = \_ -> pure ()
  , bAddAttribute      = \_ _ _ -> pure ()
  , bRecordException   = \_ _ -> pure ()
  , bGetCurrentSpanCtx = pure Nothing
  , bLog               = \_ _ _ -> pure ()
  , bIncrementCounter  = \_ _ _ -> pure ()
  , bAddCounter        = \_ _ _ -> pure ()
  , bRecordGauge       = \_ _ _ -> pure ()
  , bRecordHistogram   = \_ _ _ -> pure ()
  , bExportMetrics     = pure (mempty :: ByteString)
  , bShutdown          = pure ()
  }
