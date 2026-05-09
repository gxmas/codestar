module CodeStar.Telemetry
  ( -- * Agent Events
    AgentEvent (..)

    -- * Recorder
  , TelemetryRecorder (..)
  , noOpRecorder
  , jsonRecorder
  , otlpRecorder
  , otlpRecorderWithHandle

    -- * Span handle
  , SpanHandle (..)

    -- * Re-exports for initialization
  , TC.TelemetryConfig (..)
  , TC.OtelSettings (..)
  , TC.TelemetryHandle
  , TC.initTelemetry
  , TC.shutdownTelemetry
  ) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as LText.IO
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import System.IO (stderr)
import Prelude hiding (log)

import Telemetry.Core
  ( AttributeValue (..)
  , CounterName (..)
  , SpanName (..)
  )
import Telemetry.Core qualified as TC

import CodeStar.Types (ControlSignal (..), ModelRole (..), TaskType (..))

-- --------------------------------------------------------------------
-- Agent Events
-- --------------------------------------------------------------------

data AgentEvent
  = EvToolStart
      { toolName :: Text
      , inputSummary :: Text
      }
  | EvToolEnd
      { toolName :: Text
      , success :: Bool
      , durationMs :: Int
      }
  | EvLlmCall
      { modelRole :: ModelRole
      , inputTokens :: Int
      , outputTokens :: Int
      , durationMs :: Int
      }
  | EvControlSignal
      { signal :: ControlSignal
      }
  | EvPlanGenerated
      { stepCount :: Int
      , taskType :: TaskType
      }
  | EvCompaction
      { historyLenBefore :: Int
      , historyLenAfter :: Int
      }
  | EvCostUpdate
      { totalInputTokens :: Int
      , totalOutputTokens :: Int
      , estimatedCostUsd :: Double
      }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- --------------------------------------------------------------------
-- Span Handle
-- --------------------------------------------------------------------

newtype SpanHandle = SpanHandle TC.Span

-- --------------------------------------------------------------------
-- Recorder
-- --------------------------------------------------------------------

data TelemetryRecorder = TelemetryRecorder
  { recordEvent :: AgentEvent -> IO ()
  , startSpan :: Text -> [(Text, Text)] -> IO SpanHandle
  , endSpan :: SpanHandle -> IO ()
  , incrementCounter :: Text -> [(Text, Text)] -> IO ()
  }

-- --------------------------------------------------------------------
-- No-op backend
-- --------------------------------------------------------------------

noOpRecorder :: TelemetryRecorder
noOpRecorder =
  TelemetryRecorder
    { recordEvent = \_ -> pure ()
    , startSpan = \name _ -> SpanHandle <$> TC.startSpan (SpanName name) []
    , endSpan = \(SpanHandle sp) -> TC.endSpan sp
    , incrementCounter = \_ _ -> pure ()
    }

-- --------------------------------------------------------------------
-- JSON logging backend
-- --------------------------------------------------------------------

jsonRecorder :: TelemetryRecorder
jsonRecorder =
  TelemetryRecorder
    { recordEvent = logEvent
    , startSpan = \name attrs ->
        SpanHandle <$> TC.startSpan (SpanName name) (map toAttr attrs)
    , endSpan = \(SpanHandle sp) -> TC.endSpan sp
    , incrementCounter = \name _ ->
        TC.incrementCounter (TC.CounterName name) 1 []
    }
 where
  toAttr (k, v) = (k, TC.TextValue v)

logEvent :: AgentEvent -> IO ()
logEvent evt = do
  now <- getCurrentTime
  let entry =
        object
          [ "timestamp" .= show now
          , "event" .= (Aeson.toJSON evt :: Aeson.Value)
          ]
  LText.IO.hPutStrLn stderr (decodeUtf8 (encode entry))

-- --------------------------------------------------------------------
-- OTLP + Prometheus backend
-- --------------------------------------------------------------------

{- | Build a recorder backed by OpenTelemetry traces and Prometheus metrics.
Calls 'TC.initTelemetry' to install the global backend, then translates
every 'AgentEvent' into structured metric and counter calls.
The caller is responsible for 'TC.shutdownTelemetry' at process exit.
-}
otlpRecorder :: TC.OtelSettings -> IO TelemetryRecorder
otlpRecorder settings = fst <$> otlpRecorderWithHandle settings

otlpRecorderWithHandle :: TC.OtelSettings -> IO (TelemetryRecorder, TC.TelemetryHandle)
otlpRecorderWithHandle settings = do
  handle <- TC.initTelemetry (TC.OpenTelemetryConfig settings)
  pure
    ( TelemetryRecorder
        { recordEvent = recordEventOtlp
        , startSpan = \name attrs ->
            SpanHandle <$> TC.startSpan (SpanName name) (map toAttr attrs)
        , endSpan = \(SpanHandle sp) -> TC.endSpan sp
        , incrementCounter = \name attrs ->
            TC.incrementCounter (CounterName name) 1 (map toAttr attrs)
        }
    , handle
    )
 where
  toAttr (k, v) = (k, TextValue v)

recordEventOtlp :: AgentEvent -> IO ()
recordEventOtlp = \case
  EvToolStart{toolName} ->
    TC.log
      TC.DEBUG
      (TC.LogMessage $ "tool.start: " <> toolName)
      [("tool.name", TextValue toolName)]
  EvToolEnd{toolName, success, durationMs} -> do
    TC.incrementCounter "codestar.tool.calls" 1 attrs
    TC.recordHistogram "codestar.tool.duration" (fromIntegral durationMs) attrs
   where
    attrs =
      [ ("tool.name", TextValue toolName)
      , ("success", BoolValue success)
      ]
  EvLlmCall{modelRole, inputTokens, outputTokens, durationMs} -> do
    let role = Text.pack (show modelRole)
        attrs = [("model.role", TextValue role)]
    TC.incrementCounter "codestar.llm.calls" 1 attrs
    TC.recordHistogram "codestar.llm.input_tokens" (fromIntegral inputTokens) attrs
    TC.recordHistogram "codestar.llm.output_tokens" (fromIntegral outputTokens) attrs
    TC.recordHistogram "codestar.llm.duration_ms" (fromIntegral durationMs) attrs
  EvControlSignal{signal} ->
    TC.incrementCounter
      "codestar.control_signals"
      1
      [("signal", TextValue (signalLabel signal))]
  EvPlanGenerated{stepCount, taskType} -> do
    TC.incrementCounter
      "codestar.plans.generated"
      1
      [("task.type", TextValue (Text.pack (show taskType)))]
    TC.recordHistogram "codestar.plan.steps" (fromIntegral stepCount) []
  EvCompaction{historyLenBefore, historyLenAfter} -> do
    TC.incrementCounter "codestar.compactions" 1 []
    TC.recordGauge
      "codestar.compaction.ratio"
      (fromIntegral historyLenAfter / fromIntegral historyLenBefore)
      []
  EvCostUpdate{totalInputTokens, totalOutputTokens, estimatedCostUsd} -> do
    TC.recordGauge "codestar.session.input_tokens" (fromIntegral totalInputTokens) []
    TC.recordGauge "codestar.session.output_tokens" (fromIntegral totalOutputTokens) []
    TC.recordGauge "codestar.session.cost_usd" estimatedCostUsd []

signalLabel :: ControlSignal -> Text
signalLabel Continue = "continue"
signalLabel (NeedsInput _) = "needs_input"
signalLabel (Blocked _) = "blocked"
signalLabel (Done _) = "done"
