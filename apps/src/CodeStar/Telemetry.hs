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

    -- * Config and lifecycle
  , TelemetryConfig (..)
  , OtelSettings (..)
  , TelemetryHandle (..)
  , initTelemetry
  , shutdownTelemetry

    -- * Helpers
  , signalLabel
  , withSpan
  ) where

import Control.Exception (SomeException, mask, try)
import Control.Exception qualified as Ce
import Control.Monad (when)
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

import OTel.Attribute
  ( AttributeValue (..)
  , InstrumentationScope (..)
  )
import OTel.Attribute qualified as OTelAttr
import OTel.Context (getCurrent, attach, detach, Token)
import OTel.Log
  ( getGlobalLoggerProvider
  , getLogger
  , setGlobalLoggerProvider
  , defaultLogRecord
  , LogBody (..)
  , SeverityNumber (..)
  , LogRecord (..)
  , SomeLoggerProvider (..)
  , emit
  )
import OTel.Metric
  ( getGlobalMeterProvider
  , getMeter
  , setGlobalMeterProvider
  , SomeMeterProvider (..)
  , createCounter
  , createUpDownCounter
  , createGauge
  , createHistogram
  , counterAdd
  , upDownCounterAdd
  , gaugeSet
  , histogramRecord
  , SomeCounter
  , SomeUpDownCounter
  , SomeGauge
  , SomeHistogram
  )
import OTel.SDK.Log
  ( newSdkLoggerProvider
  , sdkLoggerProviderShutdown
  , defaultSdkLoggerProviderConfig
  , SdkLoggerProviderConfig (..)
  )
import OTel.SDK.Log.Processor (newSimpleLogRecordProcessor, SomeLogRecordProcessor (..))
import OTel.SDK.Metric
  ( newSdkMeterProvider
  , sdkMeterProviderShutdown
  , defaultSdkMeterProviderConfig
  , SdkMeterProviderConfig (..)
  )
import OTel.SDK.Metric.Reader (SomeMetricReader (..))
import OTel.SDK.Resource (create)
import OTel.SDK.Trace
  ( newSdkTracerProvider
  , shutdown
  , defaultSdkTracerProviderConfig
  , SdkTracerProviderConfig (..)
  )
import OTel.SDK.Trace.Sampler
  ( AlwaysOnSampler (..)
  , TraceIdRatioBasedSampler (..)
  , defaultParentBasedSampler
  , SomeSampler (..)
  )
import OTel.SDK.Trace.Processor
  ( newBatchSpanProcessor
  , defaultBatchSpanProcessorConfig
  , SomeSpanProcessor (..)
  )
import OTel.Trace
  ( getGlobalTracerProvider
  , getTracer
  , setGlobalTracerProvider
  , end
  , setSpanInContext
  , defaultSpanConfig
  , SpanConfig (..)
  , StatusCode (..)
  , SomeSpan (..)
  , SomeTracerProvider (..)
  , NoOpSpan (..)
  )
import OTel.Trace qualified as OTelTrace
import OTel.Exporter.OTLP.HTTP
  ( newOtlpHttpSpanExporterFromEnv
  , newOtlpHttpLogRecordExporterFromEnv
  )
import OTel.SDK.Trace.Export (SomeSpanExporter (..))
import OTel.SDK.Log.Export (SomeLogRecordExporter (..))
import OTel.Exporter.Prometheus
  ( newPrometheusMetricReader
  , defaultPrometheusConfig
  , PrometheusConfig (..)
  )

import CodeStar.Types (ControlSignal (..), ModelRole (..), TaskType (..))

-- --------------------------------------------------------------------
-- Config and lifecycle
-- --------------------------------------------------------------------

data TelemetryConfig = NoOpConfig | OpenTelemetryConfig OtelSettings

data OtelSettings = OtelSettings
  { serviceName      :: !Text
  , endpoint         :: !(Maybe Text)
  , logToStderr      :: !Bool
  , metricsEnabled   :: !Bool
  , metricsBindHost  :: !Text
  , metricsPort      :: !(Maybe Int)
  , sessionId        :: !(Maybe Text)
  , userId           :: !(Maybe Text)
  , tracesSampleRate :: !Double   -- ^ 0.0=none, 1.0=all, 0.1=10%. Default 1.0.
  }

data TelemetryHandle = TelemetryHandle
  { shutdownAction :: !(IO ())
  , metricsPort    :: !(Maybe Int)
  }

initTelemetry :: TelemetryConfig -> IO TelemetryHandle
initTelemetry NoOpConfig =
  pure TelemetryHandle { shutdownAction = pure (), metricsPort = Nothing }
initTelemetry (OpenTelemetryConfig settings) = do
  let resourceAttrs =
        [ ("service.name", StringValue settings.serviceName) ]
        <> maybe [] (\s -> [("session.id", StringValue s)]) settings.sessionId
        <> maybe [] (\u -> [("user.id",    StringValue u)]) settings.userId
      resource = create resourceAttrs Nothing

  -- Traces: OTLP/HTTP + BatchSpanProcessor + configurable sampler
  spanExporter   <- newOtlpHttpSpanExporterFromEnv
  batchProc      <- newBatchSpanProcessor (SomeSpanExporter spanExporter) defaultBatchSpanProcessorConfig
  let sampler =
        if settings.tracesSampleRate >= 1.0
          then SomeSampler AlwaysOnSampler
          else SomeSampler $
                 defaultParentBasedSampler
                   (SomeSampler (TraceIdRatioBasedSampler settings.tracesSampleRate))
  tracerProvider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerResource   = resource
    , providerProcessors = [SomeSpanProcessor batchProc]
    , providerSampler    = sampler
    }
  setGlobalTracerProvider (SomeTracerProvider tracerProvider)

  -- Metrics: Prometheus pull reader
  let port = maybe 9464 id settings.metricsPort
  promReader <- newPrometheusMetricReader defaultPrometheusConfig
    { prometheusHost = Text.unpack settings.metricsBindHost
    , prometheusPort = port
    }
  meterProvider <- newSdkMeterProvider defaultSdkMeterProviderConfig
    { providerResource = resource
    , providerReaders  = [SomeMetricReader promReader]
    }
  setGlobalMeterProvider (SomeMeterProvider meterProvider)

  -- Logs: OTLP/HTTP + SimpleLogRecordProcessor
  logExporter    <- newOtlpHttpLogRecordExporterFromEnv
  logProcessor   <- newSimpleLogRecordProcessor (SomeLogRecordExporter logExporter)
  loggerProvider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
    { llpResource   = resource
    , llpProcessors = [SomeLogRecordProcessor logProcessor]
    }
  setGlobalLoggerProvider (SomeLoggerProvider loggerProvider)

  let shutdownAll = do
        _ <- try @SomeException (shutdown tracerProvider)
        _ <- try @SomeException (sdkMeterProviderShutdown meterProvider)
        _ <- try @SomeException (sdkLoggerProviderShutdown loggerProvider)
        pure ()

  pure TelemetryHandle
    { shutdownAction = shutdownAll
    , metricsPort    = if settings.metricsEnabled then Just port else Nothing
    }

shutdownTelemetry :: TelemetryHandle -> IO ()
shutdownTelemetry h = h.shutdownAction

-- --------------------------------------------------------------------
-- Agent Events
-- --------------------------------------------------------------------

data AgentEvent
  = EvToolStart
      { toolName     :: Text
      , inputSummary :: Text
      }
  | EvToolEnd
      { toolName    :: Text
      , success     :: Bool
      , durationMs  :: Int
      , filePath    :: Maybe Text
      , errorReason :: Maybe Text
      }
  | EvLlmCall
      { modelRole          :: ModelRole
      , inputTokens        :: Int
      , outputTokens       :: Int
      , cacheCreationTokens :: Int
      , cacheReadTokens    :: Int
      , durationMs         :: Int
      , modelId            :: Text
      , stepNumber         :: Int
      , turnNumber         :: Int
      }
  | EvControlSignal
      { signal :: ControlSignal
      }
  | EvPlanGenerated
      { stepCount :: Int
      , taskType  :: TaskType
      }
  | EvCompaction
      { historyLenBefore  :: Int
      , historyLenAfter   :: Int
      , compactionDurMs   :: Double
      , compSessionId     :: Text
      }
  | EvCompactionFailed
      { compactionError  :: Text
      , historyLen       :: Int
      , cfSessionId      :: Text
      }
  | EvVerification
      { verifiedFiles   :: [Text]
      , syntaxOk        :: Bool
      , verifyOutcome   :: Text
      , verifyReason    :: Maybe Text
      , verifyDurationMs :: Double
      , sessionId       :: Text
      }
  | EvCostUpdate
      { totalInputTokens  :: Int
      , totalOutputTokens :: Int
      , estimatedCostUsd  :: Double
      , cuSessionId       :: Text
      }
  | EvLlmRetry
      { retryError       :: Text
      , retryAttempt     :: Int
      , retryAfterHintMs :: Int
      , lrSessionId      :: Text
      }
  | EvSessionCreated
      { sessionId :: Text
      , userId    :: Text
      }
  | EvSessionTerminated
      { sessionId          :: Text
      , userId             :: Text
      , terminationReason  :: Text   -- "completed", "error", "cancelled", "timeout"
      }
  | EvAuthRejected
      { rejectionReason :: Text
      }
  | EvGuardrailDecision
      { toolName          :: Text
      , guardrailDecision :: Text   -- "allow", "deny", "require_approval"
      , guardrailReason   :: Text   -- reason string (empty for Allow)
      , isDenied          :: Bool
      , sessionId         :: Text
      , userId            :: Text
      }
  | EvWsConnectionOpen
      { wcoUserId :: Text
      }
  | EvWsConnection
      { wcUserId     :: Text
      , wcDurationMs :: Double
      }
  | EvWsCommand
      { wccCommandType :: Text
      , wccSessionId   :: Text
      , wccUserId      :: Text
      , wccDurationMs  :: Double
      , wccSuccess     :: Bool
      }
  | EvMcpCall
      { mcEndpoint  :: Text
      , mcToolName  :: Text
      , mcDurationMs :: Double
      , mcSuccess   :: Bool
      }
  | EvBudgetExhausted
      { beLimitType    :: Text
      , beSessionId    :: Text
      , beUserId       :: Text
      , beTotalTokens  :: Int
      }
  | EvHistorySize
      { hsSessionId :: Text
      , hsTokensEst :: Int
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- --------------------------------------------------------------------
-- Span Handle
-- --------------------------------------------------------------------

data SpanHandle = SpanHandle SomeSpan Token

-- --------------------------------------------------------------------
-- Recorder
-- --------------------------------------------------------------------

data TelemetryRecorder = TelemetryRecorder
  { recordEvent        :: AgentEvent -> IO ()
  , startSpan          :: Text -> [(Text, AttributeValue)] -> IO SpanHandle
  , endSpan            :: SpanHandle -> IO ()
  , setSpanAttr        :: SpanHandle -> Text -> Text -> IO ()
  , setSpanAttrTyped   :: SpanHandle -> Text -> AttributeValue -> IO ()
  , setSpanError       :: SpanHandle -> Text -> IO ()
  , adjustSessionCount :: Int -> IO ()   -- +1 on create, -1 on terminate
  }

-- --------------------------------------------------------------------
-- No-op backend
-- --------------------------------------------------------------------

noOpRecorder :: TelemetryRecorder
noOpRecorder =
  TelemetryRecorder
    { recordEvent        = \_ -> pure ()
    , startSpan          = \_ _ -> do
        ctx   <- getCurrent
        token <- attach (setSpanInContext (SomeSpan NoOpSpan) ctx)
        pure (SpanHandle (SomeSpan NoOpSpan) token)
    , endSpan            = \(SpanHandle _ token) -> detach token
    , setSpanAttr        = \_ _ _ -> pure ()
    , setSpanAttrTyped   = \_ _ _ -> pure ()
    , setSpanError       = \_ _ -> pure ()
    , adjustSessionCount = \_ -> pure ()
    }

-- --------------------------------------------------------------------
-- JSON logging backend
-- --------------------------------------------------------------------

jsonRecorder :: TelemetryRecorder
jsonRecorder =
  TelemetryRecorder
    { recordEvent        = logEvent
    , startSpan          = \_ _ -> do
        ctx   <- getCurrent
        token <- attach (setSpanInContext (SomeSpan NoOpSpan) ctx)
        pure (SpanHandle (SomeSpan NoOpSpan) token)
    , endSpan            = \(SpanHandle _ token) -> detach token
    , setSpanAttr        = \_ _ _ -> pure ()
    , setSpanAttrTyped   = \_ _ _ -> pure ()
    , setSpanError       = \_ _ -> pure ()
    , adjustSessionCount = \_ -> pure ()
    }

logEvent :: AgentEvent -> IO ()
logEvent evt = do
  now <- getCurrentTime
  let entry =
        object
          [ "timestamp" .= show now
          , "event"     .= (Aeson.toJSON evt :: Aeson.Value)
          ]
  LText.IO.hPutStrLn stderr (decodeUtf8 (encode entry))

-- --------------------------------------------------------------------
-- OTLP + Prometheus backend
-- --------------------------------------------------------------------

{- | Build a recorder backed by OpenTelemetry traces and Prometheus metrics.
Calls 'initTelemetry' to install the global providers, then translates
every 'AgentEvent' into structured metric and counter calls.
The caller is responsible for 'shutdownTelemetry' at process exit.
-}
otlpRecorder :: OtelSettings -> IO TelemetryRecorder
otlpRecorder settings = fst <$> otlpRecorderWithHandle settings

otlpRecorderWithHandle :: OtelSettings -> IO (TelemetryRecorder, TelemetryHandle)
otlpRecorderWithHandle settings = do
  handle <- initTelemetry (OpenTelemetryConfig settings)

  -- Pre-create all metric instruments once; close over handles in the recorder
  meterProvider <- getGlobalMeterProvider
  meter         <- getMeter meterProvider codestarScope

  toolCallsCounter     <- createCounter       meter "codestar.tool.calls"              Nothing
  toolDurationHist     <- createHistogram     meter "codestar.tool.duration"            Nothing
  llmCallsCounter      <- createCounter       meter "codestar.llm.calls"                Nothing
  llmInputHist         <- createHistogram     meter "codestar.llm.input_tokens"         Nothing
  llmOutputHist        <- createHistogram     meter "codestar.llm.output_tokens"        Nothing
  llmDurationHist      <- createHistogram     meter "codestar.llm.duration_ms"          Nothing
  llmCacheReadHist     <- createHistogram     meter "codestar.llm.cache_read_tokens"    Nothing
  controlSigCounter    <- createCounter       meter "codestar.control_signals"          Nothing
  plansCounter         <- createCounter       meter "codestar.plans.generated"          Nothing
  planStepsHist        <- createHistogram     meter "codestar.plan.steps"               Nothing
  compactionsCounter   <- createCounter       meter "codestar.compactions"              Nothing
  compactionRatioGauge <- createGauge         meter "codestar.compaction.ratio"         Nothing
  compactDurationHist  <- createHistogram     meter "codestar.compaction.duration_ms"   Nothing
  inputTokensGauge     <- createGauge         meter "codestar.session.input_tokens"     Nothing
  outputTokensGauge    <- createGauge         meter "codestar.session.output_tokens"    Nothing
  costGauge            <- createGauge         meter "codestar.session.cost_usd"         Nothing
  historyTokensGauge   <- createGauge         meter "codestar.session.history_tokens"   Nothing
  activeSessionsUDC    <- createUpDownCounter meter "codestar.sessions.active"          Nothing
  guardrailDenials     <- createCounter       meter "codestar.guardrail.denials"        Nothing
  activeConnsUDC       <- createUpDownCounter meter "codestar.ws.connections_active"    Nothing
  wsCommandsCounter    <- createCounter       meter "codestar.ws.command.count"         Nothing
  wsCommandDurationH   <- createHistogram     meter "codestar.ws.command.duration_ms"   Nothing
  mcpCallsCounter      <- createCounter       meter "codestar.mcp.calls"                Nothing
  mcpDurationHist      <- createHistogram     meter "codestar.mcp.duration_ms"          Nothing
  budgetExhaustCounter <- createCounter       meter "codestar.budget.exhaustions"       Nothing
  verifyDurationHist   <- createHistogram     meter "codestar.verification.duration_ms" Nothing
  verifyFailCounter    <- createCounter       meter "codestar.verification.failures"    Nothing

  let instr = Instruments
        { iToolCalls          = toolCallsCounter
        , iToolDuration       = toolDurationHist
        , iLlmCalls           = llmCallsCounter
        , iLlmInput           = llmInputHist
        , iLlmOutput          = llmOutputHist
        , iLlmDuration        = llmDurationHist
        , iLlmCacheRead       = llmCacheReadHist
        , iControlSig         = controlSigCounter
        , iPlans              = plansCounter
        , iPlanSteps          = planStepsHist
        , iCompactions        = compactionsCounter
        , iCompactRatio       = compactionRatioGauge
        , iCompactDuration    = compactDurationHist
        , iInputTokens        = inputTokensGauge
        , iOutputTokens       = outputTokensGauge
        , iCost               = costGauge
        , iHistoryTokens      = historyTokensGauge
        , iGuardrailDenials   = guardrailDenials
        , iActiveConnections  = activeConnsUDC
        , iWsCommands         = wsCommandsCounter
        , iWsCommandDuration  = wsCommandDurationH
        , iMcpCalls           = mcpCallsCounter
        , iMcpDuration        = mcpDurationHist
        , iBudgetExhaustions  = budgetExhaustCounter
        , iVerifyDuration     = verifyDurationHist
        , iVerifyFailures     = verifyFailCounter
        }

  pure
    ( TelemetryRecorder
        { recordEvent        = recordEventOtlp instr
        , startSpan          = startOtelSpan
        , endSpan            = endOtelSpan
        , setSpanAttr        = setOtelSpanAttr
        , setSpanAttrTyped   = setOtelSpanAttrTyped
        , setSpanError       = setOtelSpanError
        , adjustSessionCount = \n ->
            upDownCounterAdd activeSessionsUDC (fromIntegral n) OTelAttr.emptyAttributes
        }
    , handle
    )

-- --------------------------------------------------------------------
-- Instrument record
-- --------------------------------------------------------------------

data Instruments = Instruments
  { iToolCalls           :: SomeCounter
  , iToolDuration        :: SomeHistogram
  , iLlmCalls            :: SomeCounter
  , iLlmInput            :: SomeHistogram
  , iLlmOutput           :: SomeHistogram
  , iLlmDuration         :: SomeHistogram
  , iLlmCacheRead        :: SomeHistogram
  , iControlSig          :: SomeCounter
  , iPlans               :: SomeCounter
  , iPlanSteps           :: SomeHistogram
  , iCompactions         :: SomeCounter
  , iCompactRatio        :: SomeGauge
  , iCompactDuration     :: SomeHistogram
  , iInputTokens         :: SomeGauge
  , iOutputTokens        :: SomeGauge
  , iCost                :: SomeGauge
  , iHistoryTokens       :: SomeGauge
  , iGuardrailDenials    :: SomeCounter
  , iActiveConnections   :: SomeUpDownCounter
  , iWsCommands         :: SomeCounter
  , iWsCommandDuration  :: SomeHistogram
  , iMcpCalls           :: SomeCounter
  , iMcpDuration        :: SomeHistogram
  , iBudgetExhaustions  :: SomeCounter
  , iVerifyDuration     :: SomeHistogram
  , iVerifyFailures     :: SomeCounter
  }

-- --------------------------------------------------------------------
-- Span operations (push/pop OTel context — fixes issue-05)
-- --------------------------------------------------------------------

startOtelSpan :: Text -> [(Text, AttributeValue)] -> IO SpanHandle
startOtelSpan name attrs = do
  provider <- getGlobalTracerProvider
  tracer   <- getTracer provider codestarScope
  ctx      <- getCurrent
  sp    <- OTelTrace.startSpan tracer name ctx defaultSpanConfig
             { spanAttributes = attrs }
  token <- attach (setSpanInContext sp ctx)
  pure (SpanHandle sp token)

endOtelSpan :: SpanHandle -> IO ()
endOtelSpan (SpanHandle sp token) = do
  detach token
  end sp Nothing

setOtelSpanAttr :: SpanHandle -> Text -> Text -> IO ()
setOtelSpanAttr (SpanHandle sp _) k v =
  OTelTrace.setAttribute sp k (StringValue v)

setOtelSpanAttrTyped :: SpanHandle -> Text -> AttributeValue -> IO ()
setOtelSpanAttrTyped (SpanHandle sp _) k v =
  OTelTrace.setAttribute sp k v

setOtelSpanError :: SpanHandle -> Text -> IO ()
setOtelSpanError (SpanHandle sp _) msg =
  OTelTrace.setStatus sp Error (Just msg)

-- --------------------------------------------------------------------
-- Event recording
-- --------------------------------------------------------------------

recordEventOtlp :: Instruments -> AgentEvent -> IO ()
recordEventOtlp instr = \case
  EvToolStart{toolName} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityDebug
      , logBody           = Just (LogBodyString ("tool.start: " <> toolName))
      , logAttributes     = OTelAttr.fromList [("tool.name", StringValue toolName)]
      , logContext        = Just ctx
      }

  EvToolEnd{toolName, success, durationMs, filePath, errorReason} -> do
    let baseAttrs =
          [ ("tool.name",       StringValue toolName)
          , ("tool.is_success", BoolValue success)
          ]
        fpAttr = maybe [] (\fp -> [("file.path", StringValue fp)]) filePath
        errAttr = maybe [] (\r -> [("error.message", StringValue r)]) errorReason
        attrs = OTelAttr.fromList (baseAttrs <> fpAttr <> errAttr)
    counterAdd      instr.iToolCalls    1                          attrs
    histogramRecord instr.iToolDuration (fromIntegral durationMs)  attrs

  EvLlmCall{modelRole, inputTokens, outputTokens, cacheCreationTokens, cacheReadTokens, durationMs, modelId, stepNumber, turnNumber} -> do
    let attrs = OTelAttr.fromList
          [ ("model.role",              StringValue (Text.pack (show modelRole)))
          , ("model.id",                StringValue modelId)
          , ("step.number",             Int64Value (fromIntegral stepNumber))
          , ("turn.number",             Int64Value (fromIntegral turnNumber))
          , ("llm.cache_creation_tokens", Int64Value (fromIntegral cacheCreationTokens))
          , ("llm.cache_read_tokens",   Int64Value (fromIntegral cacheReadTokens))
          , ("llm.is_cache_hit",        BoolValue (cacheReadTokens > 0))
          ]
    counterAdd      instr.iLlmCalls     1                              attrs
    histogramRecord instr.iLlmInput     (fromIntegral inputTokens)     attrs
    histogramRecord instr.iLlmOutput    (fromIntegral outputTokens)    attrs
    histogramRecord instr.iLlmDuration  (fromIntegral durationMs)      attrs
    histogramRecord instr.iLlmCacheRead (fromIntegral cacheReadTokens) attrs

  EvControlSignal{signal} ->
    counterAdd instr.iControlSig 1
      (OTelAttr.fromList [("signal", StringValue (signalLabel signal))])

  EvPlanGenerated{stepCount, taskType} -> do
    counterAdd      instr.iPlans     1
      (OTelAttr.fromList [("task.type", StringValue (Text.pack (show taskType)))])
    histogramRecord instr.iPlanSteps (fromIntegral stepCount) OTelAttr.emptyAttributes

  EvCompaction{historyLenBefore, historyLenAfter, compactionDurMs, compSessionId} -> do
    let attrs = OTelAttr.fromList [("session.id", StringValue compSessionId)]
    counterAdd      instr.iCompactions    1                                               attrs
    gaugeSet        instr.iCompactRatio   (fromIntegral historyLenAfter / fromIntegral historyLenBefore) attrs
    histogramRecord instr.iCompactDuration compactionDurMs                               attrs

  EvCompactionFailed{compactionError, historyLen, cfSessionId} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityWarn
      , logBody           = Just (LogBodyString ("compaction.failed: " <> compactionError))
      , logAttributes     = OTelAttr.fromList
          [ ("error.message", StringValue compactionError)
          , ("history.len",   Int64Value (fromIntegral historyLen))
          , ("session.id",    StringValue cfSessionId)
          ]
      , logContext        = Just ctx
      }

  EvVerification{verifiedFiles, syntaxOk, verifyOutcome, verifyReason, verifyDurationMs, sessionId} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    let severity = if verifyOutcome == "failed" then SeverityWarn else SeverityInfo
        baseAttrs =
          [ ("verify.files",     StringValue (Text.intercalate ", " verifiedFiles))
          , ("verify.syntax_ok", BoolValue syntaxOk)
          , ("verify.outcome",   StringValue verifyOutcome)
          , ("session.id",       StringValue sessionId)
          ]
        reasonAttr = maybe [] (\r -> [("verify.reason", StringValue r)]) verifyReason
        metricAttrs = OTelAttr.fromList (baseAttrs <> reasonAttr)
    histogramRecord instr.iVerifyDuration verifyDurationMs metricAttrs
    when (verifyOutcome == "failed") $
      counterAdd instr.iVerifyFailures 1
        (OTelAttr.fromList (("error.type", StringValue (maybe "unknown" id verifyReason)) : baseAttrs))
    emit logger defaultLogRecord
      { logSeverityNumber = Just severity
      , logBody           = Just (LogBodyString ("verification: " <> verifyOutcome))
      , logAttributes     = metricAttrs
      , logContext        = Just ctx
      }

  EvCostUpdate{totalInputTokens, totalOutputTokens, estimatedCostUsd, cuSessionId} -> do
    let attrs = OTelAttr.fromList [("session.id", StringValue cuSessionId)]
    gaugeSet instr.iInputTokens  (fromIntegral totalInputTokens)  attrs
    gaugeSet instr.iOutputTokens (fromIntegral totalOutputTokens) attrs
    gaugeSet instr.iCost         estimatedCostUsd                 attrs

  EvLlmRetry{retryError, retryAttempt, retryAfterHintMs, lrSessionId} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityWarn
      , logBody           = Just (LogBodyString ("llm.retry: " <> retryError))
      , logAttributes     = OTelAttr.fromList
          [ ("error.type",          StringValue retryError)
          , ("retry.attempt",       Int64Value (fromIntegral retryAttempt))
          , ("retry.after_hint_ms", Int64Value (fromIntegral retryAfterHintMs))
          , ("session.id",          StringValue lrSessionId)
          ]
      , logContext        = Just ctx
      }

  EvSessionCreated{sessionId, userId} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityInfo
      , logBody           = Just (LogBodyString "session.created")
      , logAttributes     = OTelAttr.fromList
          [ ("session.id", StringValue sessionId)
          , ("user.id",    StringValue userId)
          ]
      , logContext        = Just ctx
      }

  EvSessionTerminated{sessionId, userId, terminationReason} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityInfo
      , logBody           = Just (LogBodyString "session.terminated")
      , logAttributes     = OTelAttr.fromList
          [ ("session.id",          StringValue sessionId)
          , ("user.id",             StringValue userId)
          , ("termination.reason",  StringValue terminationReason)
          ]
      , logContext        = Just ctx
      }

  EvAuthRejected{rejectionReason} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityWarn
      , logBody           = Just (LogBodyString "auth.rejected")
      , logAttributes     = OTelAttr.fromList
          [ ("error.message", StringValue rejectionReason)
          ]
      , logContext        = Just ctx
      }

  EvGuardrailDecision{toolName, guardrailDecision, guardrailReason, isDenied, sessionId, userId} -> do
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    let attrs = OTelAttr.fromList
          [ ("tool.name",           StringValue toolName)
          , ("guardrail.decision",  StringValue guardrailDecision)
          , ("guardrail.reason",    StringValue guardrailReason)
          , ("session.id",          StringValue sessionId)
          , ("user.id",             StringValue userId)
          ]
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityInfo
      , logBody           = Just (LogBodyString "guardrail.decision")
      , logAttributes     = attrs
      , logContext        = Just ctx
      }
    when isDenied $
      counterAdd instr.iGuardrailDenials 1 attrs

  EvWsConnectionOpen{wcoUserId = _} ->
    upDownCounterAdd instr.iActiveConnections 1 OTelAttr.emptyAttributes

  EvWsConnection{wcUserId, wcDurationMs} -> do
    upDownCounterAdd instr.iActiveConnections (-1) OTelAttr.emptyAttributes
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityInfo
      , logBody           = Just (LogBodyString "ws.connection.closed")
      , logAttributes     = OTelAttr.fromList
          [ ("user.id",       StringValue wcUserId)
          , ("duration_ms",   Float64Value wcDurationMs)
          ]
      , logContext        = Just ctx
      }

  EvWsCommand{wccCommandType, wccSessionId, wccUserId, wccDurationMs, wccSuccess} -> do
    let attrs = OTelAttr.fromList
          [ ("command.type", StringValue wccCommandType)
          , ("session.id",   StringValue wccSessionId)
          , ("user.id",      StringValue wccUserId)
          , ("success",      BoolValue wccSuccess)
          ]
    counterAdd      instr.iWsCommands        1              attrs
    histogramRecord instr.iWsCommandDuration wccDurationMs  attrs

  EvMcpCall{mcEndpoint, mcToolName, mcDurationMs, mcSuccess} -> do
    let attrs = OTelAttr.fromList
          [ ("mcp.endpoint",  StringValue mcEndpoint)
          , ("mcp.tool_name", StringValue mcToolName)
          , ("success",       BoolValue mcSuccess)
          ]
    counterAdd      instr.iMcpCalls    1            attrs
    histogramRecord instr.iMcpDuration mcDurationMs attrs

  EvBudgetExhausted{beLimitType, beSessionId, beUserId, beTotalTokens} -> do
    let attrs = OTelAttr.fromList
          [ ("limit_type",    StringValue beLimitType)
          , ("session.id",    StringValue beSessionId)
          , ("user.id",       StringValue beUserId)
          , ("total_tokens",  Int64Value (fromIntegral beTotalTokens))
          ]
    counterAdd instr.iBudgetExhaustions 1 attrs
    loggerProvider <- getGlobalLoggerProvider
    logger         <- getLogger loggerProvider codestarScope
    ctx            <- getCurrent
    emit logger defaultLogRecord
      { logSeverityNumber = Just SeverityWarn
      , logBody           = Just (LogBodyString "budget.exhausted")
      , logAttributes     = attrs
      , logContext        = Just ctx
      }

  EvHistorySize{hsSessionId, hsTokensEst} ->
    gaugeSet instr.iHistoryTokens (fromIntegral hsTokensEst)
      (OTelAttr.fromList [("session.id", StringValue hsSessionId)])

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

codestarScope :: InstrumentationScope
codestarScope = InstrumentationScope
  { scopeName       = "codestar"
  , scopeVersion    = Nothing
  , scopeSchemaUrl  = Nothing
  , scopeAttributes = Nothing
  }

signalLabel :: ControlSignal -> Text
signalLabel Continue       = "continue"
signalLabel (NeedsInput _) = "needs_input"
signalLabel (Blocked _)    = "blocked"
signalLabel (Done _)       = "done"

-- | Run an action inside a span, guaranteeing endSpan even under async
-- exceptions. Uses 'mask' to eliminate the window between startSpan
-- returning and the finally handler being installed.
withSpan
  :: TelemetryRecorder
  -> Text
  -> [(Text, AttributeValue)]
  -> (SpanHandle -> IO a)
  -> IO a
withSpan tel name attrs action =
  mask $ \restore -> do
    sp <- restore (tel.startSpan name attrs)
    restore (action sp) `Ce.finally` tel.endSpan sp
