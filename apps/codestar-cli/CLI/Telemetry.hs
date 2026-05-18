module CLI.Telemetry (mkRecorder) where

import CodeStar.Config (TelemetrySection (..), TelemetryMode (..))
import CodeStar.Telemetry
  ( OtelSettings (..)
  , TelemetryRecorder (..)
  , jsonRecorder
  , noOpRecorder
  , otlpRecorderWithHandle
  , shutdownTelemetry
  )

mkRecorder :: TelemetrySection -> IO (TelemetryRecorder, IO ())
mkRecorder tel = case tel.mode of
  TelemetryOff -> pure (noOpRecorder, pure ())
  TelemetryStderr -> pure (jsonRecorder, pure ())
  TelemetryOtlp ->
    mkOtelRecorder
      OtelSettings
        { serviceName = tel.serviceName
        , endpoint = tel.endpoint
        , logToStderr = tel.logToStderr
        , metricsEnabled = tel.metricsEnabled
        , metricsBindHost = tel.metricsBindHost
        , metricsPort = tel.metricsPort
        , sessionId = Nothing
        , userId = Nothing
        , tracesSampleRate = tel.sampleRate
        }

mkOtelRecorder :: OtelSettings -> IO (TelemetryRecorder, IO ())
mkOtelRecorder settings = do
  (recorder, handle) <- otlpRecorderWithHandle settings
  pure (recorder, shutdownTelemetry handle)
