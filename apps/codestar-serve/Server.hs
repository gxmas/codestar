module Main where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch, finally, try)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS

import CodeStar.AgentSetup (buildClientForEntry, mkRecorder)
import Server.SessionSetup (spawnAgentSession)
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
import CodeStar.Config.Types (AuthMode (..), JwtAuthConfig (..), ModelEntry (..))
import CodeStar.Platform.Auth (AuthConfig (..), AuthResult (..), Identity (..), authenticate)
import CodeStar.Platform.Auth.Jwks (newJwksCache)
import CodeStar.Platform.Auth.Jwt (newJwtValidator, validateToken)
import Network.HTTP.Client.TLS (newTlsManager)
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
  ( AgentEventEnvelope (..)
  , AgentTransportDict (..)
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

  authCfg <- case config.authMode of
    NoAuth -> pure NoAuthConfig
    JwtAuth jcfg -> do
      manager <- newTlsManager
      cache <- newJwksCache manager jcfg.jwksSource jcfg.cacheTtlSeconds
      pure (JwtConfig (validateToken (newJwtValidator cache jcfg)))

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

  let wsApp = makeWsApp config authCfg recorder sessionMgr shutdownVar
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
-- WebSocket application
-- --------------------------------------------------------------------

makeWsApp :: AgentConfig -> AuthConfig -> TelemetryRecorder -> SessionManager -> TVar Bool -> WS.ServerApp
makeWsApp config authCfg recorder sessionMgr shutdownVar pending = do
  isShuttingDown <- readTVarIO shutdownVar
  if isShuttingDown
    then WS.rejectRequest pending "Server shutting down"
    else do
      let headers = WS.requestHeaders (WS.pendingRequest pending)
          token = extractBearerFromHeaders headers
      authResult <- authenticate authCfg (maybe "" id token)
      case authResult of
        Unauthenticated reason -> do
          WS.rejectRequest pending (TE.encodeUtf8 reason)
            `finally` void (try @SomeException (recorder.recordEvent Tel.EvAuthRejected{ Tel.rejectionReason = reason }))
        Authenticated identity -> do
          conn <- WS.acceptRequest pending
          WS.withPingThread conn 30 (pure ()) $
            handleConnection config recorder sessionMgr identity conn

extractBearerFromHeaders :: WS.Headers -> Maybe Text
extractBearerFromHeaders headers =
  case lookup "Authorization" headers of
    Just val ->
      let t = TE.decodeUtf8 val
       in if Text.isPrefixOf "Bearer " t
            then Just (Text.drop 7 t)
            else Nothing
    Nothing -> Nothing

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


