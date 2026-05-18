module Main where

import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Control.Exception (SomeException, catch, finally)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS

import CodeStar.AgentSetup (buildClientForEntry, mkRecorder)
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
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.Platform.Auth (Identity (..))
import CodeStar.Platform.SessionManager qualified as SM
import CodeStar.Platform.SessionManager
  ( Session (..)
  , SessionManager (..)
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
import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

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

-- --------------------------------------------------------------------
-- Command helpers
-- --------------------------------------------------------------------

commandType :: Command -> Text
commandType CmdSetModel{} = "setModel"
commandType CmdStart{}   = "start"
commandType CmdRespond{} = "respond"
commandType CmdApprove{} = "approve"
commandType CmdReject{}  = "reject"
commandType CmdCompact{} = "compact"
commandType CmdStop{}    = "stop"

commandSessionId :: Command -> Text
commandSessionId cmd = let SessionId s = cmd.sessionId in s

setSessionModel :: AgentConfig -> SessionManager -> SessionId -> Text -> IO CommandResult
setSessionModel config sessionMgr sid name = do
  mSession <- sessionMgr.get sid
  case mSession of
    Nothing -> pure (CmdErr "Session not found")
    Just session -> do
      case filter (\m -> m.meName == name) config.models of
        [] -> pure (CmdErr ("Unknown model: " <> name))
        (entry:_) -> do
          newClient <- buildClientForEntry config entry
          atomically $ writeTVar session.pendingModel (Just (name, newClient))
          pure CmdOk


