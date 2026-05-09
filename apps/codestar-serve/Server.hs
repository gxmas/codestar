module Main where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text.IO
import System.Directory (doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.Timeout (timeout)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWs
import Network.WebSockets qualified as WS

import CodeStar.AgentLoop (AgentEnv (..), AgentEvent (..), runAgent)
import CodeStar.Compaction (CompactionState (..), emptyCompactionState)
import CodeStar.Compaction qualified as Compaction
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Config
  ( Config (..)
  , AgentConfig
  , ApiKey (..)
  , BudgetSection (..)
  , ServerSection (..)
  , ContextSection (..)
  , CompactionSection (..)
  , GuardrailsSection (..)
  , SessionSection (..)
  , TelemetrySection (..)
  , TelemetryMode (..)
  , CliArgs (..)
  , CliCommand (..)
  , RunArgs (..)
  , loadConfig
  , parseCliArgs
  )
import CodeStar.Context (ContextParts (..), assemble)
import CodeStar.Context qualified as CC
import CodeStar.Guardrails qualified as GR
import CodeStar.LLM.Anthropic (newAnthropicClient)
import CodeStar.LLM.Base (buildResolver)
import CodeStar.Memory (MemoryEntry (..), loadMemory, newMemoryStore)
import CodeStar.Permissions (newPermissionStore)
import CodeStar.Platform.Auth (AuthConfig (..), AuthResult (..), Identity (..), authenticate)
import CodeStar.Platform.CostTracker (newCostTracker)
import CodeStar.Platform.Sandbox (Sandbox, noSandbox)
import CodeStar.Platform.SessionManager qualified as SM
import CodeStar.Platform.SessionManager
  ( Session (..)
  , SessionManager (..)
  , SessionStatus (..)
  , approveSession
  , newSessionManager
  , rejectSession
  , respondToSession
  )
import CodeStar.RepoMap.Cache (RepoMapCache (..), getOrComputeTags, newRepoMapCache)
import CodeStar.RepoMap.Graph (buildSymbolGraph, defaultWeights, extractTags, pageRank)
import CodeStar.RepoMap.Render (defaultRenderConfig, renderRepoMap)
import CodeStar.RepoMap.Render qualified as RepoMap
import CodeStar.Storage (newBackend)
import CodeStar.Telemetry
  ( OtelSettings (..)
  , TelemetryRecorder
  , jsonRecorder
  , noOpRecorder
  , otlpRecorderWithHandle
  , shutdownTelemetry
  )
import CodeStar.Tools.Edit (editToolHandler)
import CodeStar.Tools.Glob (globToolHandler)
import CodeStar.Tools.Grep (grepToolHandler)
import CodeStar.Tools.MCP (connectMcpEndpoints)
import CodeStar.Tools.Read (ReadTracker, newReadTracker, readToolHandler)
import CodeStar.Tools.Registry
import CodeStar.Tools.Shell (shellToolHandler)
import CodeStar.Tools.TodoList (TodoStore, newTodoStore, todoListHandlers)
import CodeStar.Transport.JsonRpc (encodeNotification, jsonRpcTransport)
import CodeStar.Transport.Types
  ( AgentEventEnvelope (..)
  , AgentTransportDict (..)
  , Command (..)
  , CommandResult (..)
  )
import CodeStar.Transport.WebSocket (websocketRecv, websocketSend)
import CodeStar.TreeSitter (GrammarRegistry, loadGrammarRegistry)
import CodeStar.Types ()

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

  let wsApp = makeWsApp config recorder sessionMgr shutdownVar
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

makeWsApp :: AgentConfig -> TelemetryRecorder -> SessionManager -> TVar Bool -> WS.ServerApp
makeWsApp config recorder sessionMgr shutdownVar pending = do
  isShuttingDown <- readTVarIO shutdownVar
  if isShuttingDown
    then WS.rejectRequest pending "Server shutting down"
    else do
      let headers = WS.requestHeaders (WS.pendingRequest pending)
          token = extractBearerFromHeaders headers
          authCfg = NoAuthConfig
      authResult <- authenticate authCfg (maybe "" id token)
      case authResult of
        Unauthenticated reason ->
          WS.rejectRequest pending (TE.encodeUtf8 reason)
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
  let sendBytes = websocketSend conn
      recvBytes = websocketRecv conn

  transport <- jsonRpcTransport sendBytes recvBytes

  transport.onCommand $ \cmd ->
    handleCommand config recorder sessionMgr identity conn cmd

  transport.listen
    `catch` ( \(ex :: SomeException) ->
                Text.IO.hPutStrLn stderr ("[server] connection error: " <> Text.pack (show ex))
            )

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
      Right session -> do
        let ApiKey key = config.apiKey
        resolver <- buildResolver config.modelRoles (newAnthropicClient key)
        tracker <- newReadTracker
        todoStore <- newTodoStore
        globalCfgDir <- Paths.globalConfigDir
        let projDir = Paths.projectDir config.workspacePath
        perms <- newPermissionStore config.workspacePath globalCfgDir
        costTracker <- newCostTracker config.budgets.sessionTokenMax config.budgets.dailyTokenMax
        memStore <- newMemoryStore (projDir </> "memory")
        memEntries <- loadMemory memStore
        let memPaths =
              [ projDir </> "memory" </> "entries" </> Text.unpack e.meId <> ".json"
              | e <- memEntries
              ]

        grammarReg <- loadGrammarRegistry
        cacheBackend <- newBackend (projDir </> "cache")
        let repoCache = newRepoMapCache cacheBackend
        repoMapText <- buildRepoMapSafe grammarReg repoCache config.workspacePath

        let sandbox = noSandbox config.workspacePath
        mcpHandlers <- connectMcpEndpoints config.mcpEndpoints
        let onEdit = Just (repoCache.invalidate)
            registry = foldr register (buildRegistry tracker todoStore sandbox onEdit) mcpHandlers

        let ctxCfg = CC.ContextConfig
              { CC.maxContextTokens  = config.context.maxTokens
              , CC.repoMapReserve    = config.context.repoMapReserve
              , CC.memoryReserve     = config.context.memoryReserve
              , CC.compactionReserve = config.context.compactionReserve
              , CC.responseReserve   = config.context.responseReserve
              }
            compCfg = Compaction.CompactionConfig
              { Compaction.triggerFraction  = config.compaction.triggerFraction
              , Compaction.maxContextTokens = config.compaction.maxContextTokens
              }
            grCfg = GR.GuardrailConfig
              { GR.denyList       = config.guardrails.denyList
              , GR.allowList      = config.guardrails.allowList
              , GR.allowedPaths   = Nothing
              , GR.secretPatterns = config.guardrails.secretPatterns
              }

        (_ctxMsgs, ctxParts) <-
          assemble
            ctxCfg
            (buildSystemPrompt registry)
            repoMapText
            memPaths
            []
        let sysPrompt = ctxParts.systemPrompt

        let waitInput query = do
              atomically $ writeTVar session.status SWaitingForInput
              session.eventSink $
                AgentEventEnvelope
                  session.sessionId
                  (AgentProgress ("Waiting for input: " <> query))
              takeMVar session.inputMVar

            waitApproval _reason = do
              atomically $ writeTVar session.status SWaitingForApproval
              takeMVar session.approvalMVar

        let env =
              AgentEnv
                { envLlm = resolver
                , envTools = registry
                , envConfig = config
                , envTelemetry = recorder
                , envOnEvent = \ev -> eventSinkFn (AgentEventEnvelope session.sessionId ev)
                , envGuardrails = grCfg
                , envPermissions = Just perms
                , envCompaction = compCfg
                , envCompState = emptyCompactionState{csRepoMap = repoMapText}
                , envCostTracker = Just costTracker
                , envSessionId = session.sessionId
                , envUserId = identity.userId
                , envGrammarReg = grammarReg
                , envMemoryStore = Just memStore
                , envWaitForInput = Just waitInput
                , envWaitForApproval = Just waitApproval
                }

        thread <- async $ do
          result <- try (runAgent env sysPrompt task)
          case result of
            Right signal -> atomically $ writeTVar session.status (SCompleted signal)
            Left (ex :: SomeException) -> do
              let msg = Text.pack (show ex)
              eventSinkFn (AgentEventEnvelope session.sessionId (AgentError msg))
              atomically $ writeTVar session.status STerminated
        atomically $ writeTVar session.workerThread (Just thread)

        pure CmdOk
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

-- --------------------------------------------------------------------
-- Shared utilities (mirrored from CLI.hs)
-- --------------------------------------------------------------------

buildRegistry :: ReadTracker -> TodoStore -> Sandbox -> Maybe (FilePath -> IO ()) -> ToolRegistry
buildRegistry tracker todoStore sandbox mOnEdit =
  register (readToolHandler tracker) $
    register (editToolHandler tracker Nothing mOnEdit) $
      register globToolHandler $
        register grepToolHandler $
          register (shellToolHandler sandbox) $
            foldr register emptyRegistry (todoListHandlers todoStore)

buildSystemPrompt :: ToolRegistry -> Text
buildSystemPrompt registry =
  Text.unlines
    [ "You are CodeStar, an expert AI coding agent."
    , "Work methodically: read files before editing, validate changes,"
    , "and declare done only when you have evidence the task is complete."
    , ""
    , "## Available Tools"
    , ""
    , generateDocs registry
    ]

mkRecorder :: TelemetrySection -> IO (TelemetryRecorder, IO ())
mkRecorder tel = case tel.mode of
  TelemetryOff -> pure (noOpRecorder, pure ())
  TelemetryStderr -> pure (jsonRecorder, pure ())
  TelemetryOtlp -> do
    (recorder, handle) <-
      otlpRecorderWithHandle
        OtelSettings
          { serviceName = tel.serviceName
          , endpoint = tel.endpoint
          , logToStderr = tel.logToStderr
          , metricsEnabled = tel.metricsEnabled
          , metricsBindHost = tel.metricsBindHost
          , metricsPort = tel.metricsPort
          }
    pure (recorder, shutdownTelemetry handle)

listWorkspaceFiles :: FilePath -> IO [FilePath]
listWorkspaceFiles root = do
  entries <- listDirectory root
  let visible = filter (not . ("." `isPrefixOf`)) entries
  fmap concat $ forM visible $ \name -> do
    let path = root </> name
    isFile <- doesFileExist path
    if isFile
      then pure [path]
      else listWorkspaceFiles path

buildRepoMapSafe :: GrammarRegistry -> RepoMapCache -> FilePath -> IO Text
buildRepoMapSafe grammarReg repoCache workDir = do
  wsFiles <- listWorkspaceFiles workDir
  result <- timeout 5000000 $ do
    allTags <- fmap concat $ forM wsFiles $ \f -> do
      src <- BS.readFile f
      getOrComputeTags repoCache f (extractTags grammarReg f src)
    let graph = buildSymbolGraph allTags
        scores = pageRank graph [] [] defaultWeights
    pure $!
      renderRepoMap
        allTags
        scores
        graph
        defaultRenderConfig{RepoMap.maxTokens = 4096}
  case result of
    Just repoMap -> pure repoMap
    Nothing -> pure Text.empty
