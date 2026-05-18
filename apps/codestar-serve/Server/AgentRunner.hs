module Server.AgentRunner (runAgentWithTelemetry) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (SomeException, bracket_, finally, mask, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.AgentLoop (AgentEnv, AgentEvent (..), runAgent)
import CodeStar.Platform.SessionManager (Session (..), SessionStatus (..))
import CodeStar.Telemetry (TelemetryRecorder (..), signalLabel)
import CodeStar.Telemetry qualified as Tel
import CodeStar.Transport.Types (AgentEventEnvelope (..))
import CodeStar.Types (ControlSignal (..), SessionId (..), UserId (..))
import OTel.Attribute (AttributeValue (..))
import OTel.Context (getCurrent, attach, detach)

runAgentWithTelemetry ::
  TelemetryRecorder ->
  Session ->
  UserId ->
  (AgentEventEnvelope -> IO ()) ->
  AgentEnv ->
  Text ->
  Text ->
  IO (Async ())
runAgentWithTelemetry recorder session userId eventSinkFn env sysPrompt task = do
  parentCtx <- getCurrent
  async $ do
    ctxToken <- attach parentCtx
    let SessionId sid = session.sessionId
        UserId uid = userId
    terminationReasonRef <- newIORef "cancelled"
    let terminateWith reason = writeIORef terminationReasonRef reason
    (`finally` detach ctxToken) $
      bracket_
        (recorder.adjustSessionCount 1)
        (do reason <- readIORef terminationReasonRef
            recorder.adjustSessionCount (-1)
            recorder.recordEvent Tel.EvSessionTerminated
              { Tel.sessionId = sid
              , Tel.userId    = uid
              , Tel.terminationReason = reason
              })
        (do recorder.recordEvent Tel.EvSessionCreated
              { Tel.sessionId = sid
              , Tel.userId    = uid
              }
            mask $ \restore -> do
              spanResult <- restore $ try (recorder.startSpan "agent.turn"
                [ ("session.id", StringValue sid)
                , ("user.id",    StringValue uid)
                , ("task",       StringValue (Text.take 200 task))
                ])
              case spanResult of
                Left (spanEx :: SomeException) -> do
                  terminateWith "error"
                  eventSinkFn (AgentEventEnvelope session.sessionId
                    (AgentError ("Telemetry init failed: " <> Text.pack (show spanEx))))
                  atomically $ writeTVar session.status STerminated
                Right rootSpan ->
                  restore (do
                    result <- try (runAgent env sysPrompt task)
                    case result of
                      Right signal -> do
                        terminateWith (signalLabel signal)
                        recorder.setSpanAttr rootSpan "outcome" (signalLabel signal)
                        case signal of
                          Blocked _ -> recorder.setSpanAttr rootSpan "sampling.priority" "1"
                          _         -> pure ()
                        atomically $ writeTVar session.status (SCompleted signal)
                      Left (ex :: SomeException) -> do
                        terminateWith "error"
                        let msg = Text.pack (show ex)
                        recorder.setSpanError rootSpan msg
                        recorder.setSpanAttr rootSpan "sampling.priority" "1"
                        eventSinkFn (AgentEventEnvelope session.sessionId (AgentError msg))
                        atomically $ writeTVar session.status STerminated)
                  `finally` recorder.endSpan rootSpan)
