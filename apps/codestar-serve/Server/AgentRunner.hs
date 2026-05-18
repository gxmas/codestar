{- |
= Server.AgentRunner — async agent execution with telemetry

This module wraps the core 'runAgent' call in the infrastructure needed
for production server use:

  * __Async execution__: each agent session runs in its own 'Async' thread
    so the WebSocket handler can return immediately and the connection
    remains responsive (e.g. to @CmdStop@).

  * __OTel context propagation__: the span context from the WebSocket
    connection is captured before the thread is spawned and re-attached
    inside the new thread.  Without this, the agent's child spans would
    appear as disconnected traces in the OTel backend.

  * __Structured termination__: 'bracket_' guarantees that the session
    count gauge is decremented and a termination event is recorded even if
    the agent panics.  The termination reason (@\"done\"@, @\"blocked\"@,
    @\"error\"@, @\"cancelled\"@) is stored in an 'IORef' and updated just
    before the bracket exits.

  * __Exception safety__: 'mask' / 'restore' ensure that the root span is
    created atomically before any async exception can interrupt the thread,
    preventing orphaned active spans.
-}
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

-- | Spawn an agent session as a background 'Async' thread, bracketed by
-- telemetry instrumentation.
--
-- Parameters:
--
--   * @recorder@ — the OTel recorder; used for spans, events, and the
--     session-count gauge.
--   * @session@ — the session record from the 'SessionManager'; contains
--     the @sessionId@, status @TVar@, and blocking @MVar@s.
--   * @userId@ — attached to every span and event for multi-tenant tracing.
--   * @eventSinkFn@ — callback that serialises 'AgentEventEnvelope' values
--     and sends them back to the client as JSON-RPC notifications.
--   * @env@ — the fully-initialised 'AgentEnv' (tools, LLM client, …).
--   * @sysPrompt@ — the system prompt string.
--   * @task@ — the initial user message that starts the agent turn.
--
-- Returns the 'Async' handle so the caller can store it (for cancellation)
-- or wait on it.
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
