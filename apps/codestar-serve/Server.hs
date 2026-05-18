{- |
= codestar-serve — WebSocket agent server

The server exposes the coding agent over a __WebSocket + JSON-RPC__
protocol so that remote clients (IDEs, web frontends, other agents) can
drive it without running a local process.

== Architecture overview

@
  Client (IDE / browser)
       │  WebSocket (JSON-RPC)
       ▼
  Server.WebSocket.makeWsApp        ← authenticates, accepts connection
       │
       ▼
  handleConnection                  ← per-connection lifecycle + telemetry
       │
       ▼
  handleCommand (dispatch)          ← routes JSON-RPC commands
       │
  ┌────┴──────────────────────────────┐
  │ CmdStart → Server.SessionSetup    │  allocates per-session resources
  │            Server.AgentRunner     │  spawns async agent thread
  │ CmdRespond / Approve / Reject     │  unblocks waiting agent
  │ CmdStop                           │  destroys session
  └───────────────────────────────────┘
@

== Key design choices

  * __One thread per session__: each agent run is an 'Async' thread.
    The session manager tracks live threads and cancels them on @CmdStop@
    or inactivity timeout.
  * __Event sink__: the agent emits events (tokens, tool calls …) via a
    callback that serialises them as JSON-RPC notifications and sends them
    back over the same WebSocket connection.
  * __Telemetry__: every connection and command is bracketed with
    OpenTelemetry spans so latency and errors are observable in any OTel
    backend.
-}
module Main where

import Control.Concurrent.STM (newTVarIO)
import Control.Exception (SomeException, catch, finally)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS

import CodeStar.AgentSetup (mkRecorder)
import Server.Commands (commandType, commandSessionId, setSessionModel)
import Server.SessionSetup (spawnAgentSession)
import Server.WebSocket (buildAuthConfig, makeWsApp)
import CodeStar.Config
  ( Config (..)
  , AgentConfig
  , ServerSection (..)
  , SessionSection (..)
  , CliArgs (..)
  , CliCommand (..)
  , RunArgs (..)
  , loadConfig
  , parseCliArgs
  )
import CodeStar.Platform.Auth (Identity (..))
import CodeStar.Platform.SessionManager qualified as SM
import CodeStar.Platform.SessionManager
  ( SessionManager (..)
  , approveSession
  , newSessionManager
  , rejectSession
  , respondToSession
  )
import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Transport.JsonRpc (encodeNotification, jsonRpcTransport)
import CodeStar.Transport.Types
  ( AgentTransportDict (..)
  , Command (..)
  , CommandResult (..)
  )
import CodeStar.Transport.WebSocket (websocketRecv, websocketSend)
import CodeStar.Types (UserId (..))

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

-- | Parse CLI arguments and start the server.  Only the @run@ command
-- is accepted; other sub-commands (e.g. @fetch-grammars@) belong to the
-- CLI binary, not the server.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  args <- parseCliArgs
  case args.cliCommand of
    RunAgent runArgs -> runServer runArgs
    _ -> hPutStrLn stderr "codestar-server: use 'run' mode" >> exitFailure

-- --------------------------------------------------------------------
-- Server
-- --------------------------------------------------------------------

-- | Bootstrap the server: load config, initialise telemetry and auth,
-- create the session manager, and start Warp.
--
-- Warp is configured to:
--
--   * Listen on all IPv6 interfaces (@\"*6\"@), which also covers IPv4
--     on dual-stack kernels.
--   * Keep connections alive for up to 3600 seconds (long-lived WebSocket).
--   * Allow 30 seconds for in-flight requests to drain on shutdown.
--
-- Non-WebSocket HTTP requests get a __426 Upgrade Required__ response,
-- directing clients to use the WebSocket endpoint.
runServer :: RunArgs -> IO ()
runServer runArgs = do
  configResult <- loadConfig runArgs
  (config :: Config) <- either (\e -> hPutStrLn stderr (show e) >> exitFailure) pure configResult
  (recorder, shutdownRecorder) <- mkRecorder config.telemetry
  authCfg <- buildAuthConfig config

  let sessCfg = SM.SessionConfig
        { SM.maxSessionsPerUser = config.session.maxPerUser
        , SM.inactivityTimeout  = config.session.inactivityTimeout
        }
  sessionMgr <- newSessionManager sessCfg
  shutdownVar <- newTVarIO False

  let port = config.server.port
  Text.IO.putStrLn $
    "[codestar-server] listening on ws://[::]:"
      <> Text.pack (show port)
      <> "/agent"

  let wsApp = makeWsApp config authCfg recorder sessionMgr shutdownVar handleConnection
      warpSettings =
        Warp.setPort port $
          Warp.setHost "*6" $
            Warp.setTimeout 3600 $
              Warp.setGracefulShutdownTimeout (Just 30) $
                Warp.defaultSettings
      fallback _req respond =
        respond $
          Wai.responseLBS (toEnum 426) [] "WebSocket upgrade required"

  ( Warp.runSettings warpSettings $
      WaiWs.websocketsOr WS.defaultConnectionOptions wsApp fallback
    )
    `finally` shutdownRecorder


-- --------------------------------------------------------------------
-- Connection handler
-- --------------------------------------------------------------------

-- | Manage the lifecycle of a single authenticated WebSocket connection.
--
-- The connection is wrapped in an OTel span so we can track its duration
-- and attribute errors to it.  The JSON-RPC transport layer
-- ('jsonRpcTransport') decodes incoming frames into typed 'Command' values
-- and calls 'handleCommand' for each one.  The transport's 'listen' loop
-- runs until the client disconnects or an exception occurs; 'finally'
-- ensures the connection-close event is always recorded regardless of
-- how the loop exits.
handleConnection :: AgentConfig -> TelemetryRecorder -> SessionManager -> Identity -> WS.Connection -> IO ()
handleConnection config recorder sessionMgr identity conn = do
  let UserId uid = identity.userId
  connSpan <- recorder.startSpan "ws.connection" [("user.id", StringValue uid)]
  recorder.recordEvent Tel.EvWsConnectionOpen { Tel.wcoUserId = uid }
  t0conn <- getMonotonicTimeNSec
  let endConn = do
        t1conn <- getMonotonicTimeNSec
        let durMs = fromIntegral ((t1conn - t0conn) `div` 1_000_000) :: Double
        recorder.endSpan connSpan
        recorder.recordEvent Tel.EvWsConnection { Tel.wcUserId = uid, Tel.wcDurationMs = durMs }
  let sendBytes = websocketSend conn
      recvBytes = websocketRecv conn

  transport <- jsonRpcTransport sendBytes recvBytes

  transport.onCommand $ \cmd -> do
    cmdType <- pure (commandType cmd)
    t0cmd <- getMonotonicTimeNSec
    cmdSpan <- recorder.startSpan "ws.command"
      [ ("command.type", StringValue cmdType)
      , ("user.id",      StringValue uid)
      ]
    result <- handleCommand config recorder sessionMgr identity conn cmd
    t1cmd <- getMonotonicTimeNSec
    let cmdDurMs = fromIntegral ((t1cmd - t0cmd) `div` 1_000_000) :: Double
        success  = case result of { CmdErr _ -> False; _ -> True }
    recorder.endSpan cmdSpan
    recorder.recordEvent Tel.EvWsCommand
      { Tel.wccCommandType = cmdType
      , Tel.wccSessionId   = commandSessionId cmd
      , Tel.wccUserId      = uid
      , Tel.wccDurationMs  = cmdDurMs
      , Tel.wccSuccess     = success
      }
    pure result

  (transport.listen
    `catch` ( \(ex :: SomeException) ->
                Text.IO.hPutStrLn stderr ("[server] connection error: " <> Text.pack (show ex))
            ))
    `finally` endConn

-- --------------------------------------------------------------------
-- Command dispatch
-- --------------------------------------------------------------------

-- | Map each JSON-RPC 'Command' to the appropriate server action.
--
-- This is the protocol's __command dispatcher__ — the single function that
-- decides what happens when the client sends a message.  Each branch is
-- intentionally small; the real work is delegated to specialised modules:
--
--   * 'CmdStart' → 'spawnAgentSession' in "Server.SessionSetup"
--   * 'CmdRespond', 'CmdApprove', 'CmdReject' → 'SessionManager'
--   * 'CmdSetModel' → 'setSessionModel' in "Server.Commands"
--
-- 'CmdCompact' is acknowledged but currently a no-op at the server level;
-- compaction is driven by the agent loop itself.
handleCommand ::
  AgentConfig ->
  TelemetryRecorder ->
  SessionManager ->
  Identity ->
  WS.Connection ->
  Command ->
  IO CommandResult
handleCommand config recorder sessionMgr identity conn cmd = case cmd of
  CmdStart{sessionId = _sid, task} -> do
    let eventSinkFn envelope = websocketSend conn (encodeNotification envelope)
    sessionResult <- sessionMgr.create identity.userId eventSinkFn
    case sessionResult of
      Left err -> pure (CmdErr err)
      Right session -> spawnAgentSession config recorder session identity eventSinkFn task
  CmdRespond{sessionId = sid, response = text} ->
    respondToSession sessionMgr sid text
  CmdApprove{sessionId = sid} ->
    approveSession sessionMgr sid
  CmdReject{sessionId = sid, reason} ->
    rejectSession sessionMgr sid reason
  CmdCompact{} ->
    pure CmdOk
  CmdStop{sessionId = sid} -> do
    sessionMgr.destroy sid
    pure CmdOk
  CmdSetModel{sessionId = sid, modelName = name} ->
    setSessionModel config sessionMgr sid name



