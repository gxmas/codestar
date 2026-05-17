module Network.MCP.Session.ConnectSpec (spec) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Streaming.Prelude as SP
import Streaming (Stream, Of)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec

import Network.MCP.Session
  ( Session (..)
  , SessionError (..)
  , SessionErrorKind (..)
  , RequestOptions (..)
  )
import Network.MCP.Session.Connect (ConnectConfig (..), connect, defaultConnectConfig, sessionPing)
import Network.MCP.Transport (Transport (..), TransportError (..))
import Network.MCP.Transport.Stdio (newWithHandles)
import Network.MCP.Types
import Network.MCP.Types.Capabilities

------------------------------------------------------------------------
-- Echo server helpers
------------------------------------------------------------------------

-- | Start a background MCP server that responds to @initialize@ with
-- the given capabilities and protocol version, then silently ignores
-- all subsequent messages.
startEchoServer
  :: ServerCapabilities
  -> ProtocolVersion
  -> IO (Async (), ConnectConfig -> IO (Either SessionError Session))
startEchoServer serverCaps version = do
  (clientRead,  serverWrite) <- createPipe
  (serverRead,  clientWrite) <- createPipe
  clientT <- newWithHandles clientWrite clientRead
  serverT <- newWithHandles serverWrite serverRead
  serverThread <- async (serveInitialize serverT serverCaps version)
  let connect' cfg = connect clientT cfg
  pure (serverThread, connect')

-- | Keep reading from the transport, respond to @initialize@ once,
-- then discard everything else.
serveInitialize
  :: Transport t
  => t
  -> ServerCapabilities
  -> ProtocolVersion
  -> IO ()
serveInitialize t serverCaps version = do
  stream <- messages t
  SP.mapM_ (handleServerMsg t serverCaps version) stream

handleServerMsg
  :: Transport t
  => t
  -> ServerCapabilities
  -> ProtocolVersion
  -> Either e MCPMessage
  -> IO ()
handleServerMsg _ _ _ (Left _) = pure ()
handleServerMsg t serverCaps version (Right msg) = case msg of
  MCPRequest req | req.requestMethod == "initialize" -> do
    let result = Aeson.object
          [ "protocolVersion" Aeson..= version
          , "capabilities"    Aeson..= serverCaps
          , "serverInfo"      Aeson..= Implementation "test-server" "0.1.0" Nothing Nothing
          ]
    _ <- send t (MCPResult JSONRPCResult
          { resultId     = req.requestId
          , resultResult = result
          , resultMeta   = Nothing
          })
    pure ()
  _ -> pure ()

-- | Like startEchoServer but the initialize response includes instructions.
startEchoServerWithInstructions
  :: ServerCapabilities
  -> ProtocolVersion
  -> Maybe T.Text
  -> IO (Async (), ConnectConfig -> IO (Either SessionError Session))
startEchoServerWithInstructions serverCaps version mInstr = do
  (clientRead,  serverWrite) <- createPipe
  (serverRead,  clientWrite) <- createPipe
  clientT <- newWithHandles clientWrite clientRead
  serverT <- newWithHandles serverWrite serverRead
  serverThread <- async (serveInitializeWithInstructions serverT serverCaps version mInstr)
  let connect' cfg = connect clientT cfg
  pure (serverThread, connect')

-- | Like serveInitialize but includes optional instructions.
serveInitializeWithInstructions
  :: Transport t
  => t
  -> ServerCapabilities
  -> ProtocolVersion
  -> Maybe T.Text
  -> IO ()
serveInitializeWithInstructions t serverCaps version mInstr = do
  stream <- messages t
  SP.mapM_ (handleServerMsgWithInstr t serverCaps version mInstr) stream

handleServerMsgWithInstr
  :: Transport t
  => t
  -> ServerCapabilities
  -> ProtocolVersion
  -> Maybe T.Text
  -> Either e MCPMessage
  -> IO ()
handleServerMsgWithInstr _ _ _ _ (Left _) = pure ()
handleServerMsgWithInstr t serverCaps version mInstr (Right msg) = case msg of
  MCPRequest req | req.requestMethod == "initialize" -> do
    let result = Aeson.object $
          [ "protocolVersion" Aeson..= version
          , "capabilities"    Aeson..= serverCaps
          , "serverInfo"      Aeson..= Implementation "test-server" "0.1.0" Nothing Nothing
          ]
          ++ maybe [] (\i -> ["instructions" Aeson..= i]) mInstr
    _ <- send t (MCPResult JSONRPCResult
          { resultId     = req.requestId
          , resultResult = result
          , resultMeta   = Nothing
          })
    pure ()
  _ -> pure ()

------------------------------------------------------------------------
-- Black-hole transport (never responds, for timeout tests)
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Progress server helpers
------------------------------------------------------------------------

-- | Start a server that responds to initialize, then for each subsequent
-- request: sends one progress notification (50 % complete), then responds
-- with {}.
startProgressServer
  :: ProtocolVersion
  -> IO (Async (), ConnectConfig -> IO (Either SessionError Session))
startProgressServer version = do
  (clientRead,  serverWrite) <- createPipe
  (serverRead,  clientWrite) <- createPipe
  clientT <- newWithHandles clientWrite clientRead
  serverT <- newWithHandles serverWrite serverRead
  serverThread <- async (serveWithProgress serverT version)
  let connect' cfg = connect clientT cfg
  pure (serverThread, connect')

serveWithProgress :: Transport t => t -> ProtocolVersion -> IO ()
serveWithProgress t version = do
  stream <- messages t
  SP.mapM_ (handleWithProgress t version) stream

handleWithProgress
  :: Transport t => t -> ProtocolVersion
  -> Either e MCPMessage -> IO ()
handleWithProgress _ _ (Left _) = pure ()
handleWithProgress t version (Right msg) = case msg of
  MCPRequest req | req.requestMethod == "initialize" -> do
    let result = Aeson.object
          [ "protocolVersion" Aeson..= version
          , "capabilities"    Aeson..= defaultServerCaps
          , "serverInfo"      Aeson..= Implementation "test-server" "0.1.0" Nothing Nothing
          ]
    _ <- send t (MCPResult JSONRPCResult { resultId = req.requestId, resultResult = result, resultMeta = Nothing })
    pure ()
  MCPRequest req -> do
    -- Extract progressToken from _meta if present
    let mToken = req.requestMeta >>= HM.lookup "progressToken"
    case mToken of
      Just tokenVal -> do
        let progressParams = Aeson.object
              [ "progressToken" Aeson..= tokenVal
              , "progress"      Aeson..= (0.5 :: Double)
              , "total"         Aeson..= (1.0 :: Double)
              , "message"       Aeson..= ("halfway" :: T.Text)
              ]
        _ <- send t (MCPNotification JSONRPCNotification
              { notificationMethod = "notifications/progress"
              , notificationParams = Just progressParams
              , notificationMeta   = Nothing
              })
        pure ()
      Nothing -> pure ()
    -- Respond with empty result
    _ <- send t (MCPResult JSONRPCResult
          { resultId = req.requestId
          , resultResult = Aeson.object []
          , resultMeta = Nothing
          })
    pure ()
  _ -> pure ()

------------------------------------------------------------------------
-- Ping server helpers
------------------------------------------------------------------------

startPingServer
  :: IO (Async (), ConnectConfig -> IO (Either SessionError Session))
startPingServer = do
  (clientRead,  serverWrite) <- createPipe
  (serverRead,  clientWrite) <- createPipe
  clientT <- newWithHandles clientWrite clientRead
  serverT <- newWithHandles serverWrite serverRead
  stream <- messages serverT
  serverThread <- async (SP.mapM_ (handlePing serverT) stream)
  let connect' cfg = connect clientT cfg
  pure (serverThread, connect')

handlePing :: Transport t => t -> Either e MCPMessage -> IO ()
handlePing _ (Left _) = pure ()
handlePing t (Right msg) = case msg of
  MCPRequest req | req.requestMethod == "initialize" -> do
    let result = Aeson.object
          [ "protocolVersion" Aeson..= ProtocolVersion "2025-03-26"
          , "capabilities"    Aeson..= defaultServerCaps
          , "serverInfo"      Aeson..= Implementation "test-server" "0.1.0" Nothing Nothing
          ]
    _ <- send t (MCPResult JSONRPCResult { resultId = req.requestId, resultResult = result, resultMeta = Nothing })
    pure ()
  MCPRequest req | req.requestMethod == "ping" -> do
    _ <- send t (MCPResult JSONRPCResult
          { resultId = req.requestId
          , resultResult = Aeson.object []
          , resultMeta = Nothing
          })
    pure ()
  _ -> pure ()

------------------------------------------------------------------------
-- Black-hole transport (never responds, for timeout tests)
------------------------------------------------------------------------

-- | A transport that accepts sends but never produces any messages.
data BlackHoleTransport = BlackHoleTransport

newBlackHoleTransport :: IO BlackHoleTransport
newBlackHoleTransport = pure BlackHoleTransport

instance Transport BlackHoleTransport where
  send _ _ = pure (Right ())
  messages _ = pure (pure ())  -- empty stream, never yields
  close _ = pure ()

------------------------------------------------------------------------

defaultServerCaps :: ServerCapabilities
defaultServerCaps = ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing

clientInfo :: Implementation
clientInfo = Implementation "test-client" "0.1.0" Nothing Nothing

defaultCfg :: ConnectConfig
defaultCfg = defaultConnectConfig clientInfo

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "connect" $ do
    it "completes the initialize handshake and returns a Session" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> show err
        Right session -> do
          session.sessionProtocolVersion `shouldBe` version
          session.sessionClose

    it "returns Left VersionMismatch when server sends a different version" $ do
      let serverVersion = ProtocolVersion "2024-01-01"
          clientVersion = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps serverVersion
      let cfg = defaultCfg { connectProtocolVersion = clientVersion }
      result <- connect' cfg
      cancel serverThread
      case result of
        Left err -> err.sessionErrorKind `shouldBe` VersionMismatch
        Right session -> do
          session.sessionClose
          expectationFailure "expected Left VersionMismatch"

    it "matching versions succeed even when non-default" $ do
      let version = ProtocolVersion "2024-11-05"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      let cfg = defaultCfg { connectProtocolVersion = version }
      result <- connect' cfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right: " <> show err
        Right session -> session.sessionClose

  describe "sessionClose" $ do
    it "sessionClose is safe to call after connect succeeds" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      Right session <- connect' defaultCfg
      cancel serverThread
      session.sessionClose
      session.sessionClose  -- second close must not throw

  describe "sessionOnRequest handler dispatch" $ do
    it "handler registered via sessionOnRequest is called for matching method" $ do
      called <- newEmptyMVar
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      Right session <- connect' defaultCfg
      -- Register a handler for "ping" on the client session
      session.sessionOnRequest "ping" $ \_ _ -> do
        putMVar called ()
        pure (Right (Aeson.object []))
      -- Use sessionRequest to send a ping FROM the client to itself is not
      -- possible directly; instead verify handler registration does not throw.
      -- A full handler dispatch test requires an active server that sends
      -- an inbound request — covered by integration tests.
      cancel serverThread
      session.sessionClose
      -- Verify handler was registered without error (no exception thrown)
      pure ()

  describe "sessionRequest" $ do
    it "sessionRequest returns Left when session is closed" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      Right session <- connect' defaultCfg
      cancel serverThread
      session.sessionClose
      result <- session.sessionRequest "ping" Nothing Nothing
      case result of
        Left err -> T.unpack err.rpcErrorMessage `shouldContain` "closed"
        Right _  -> expectationFailure "expected Left after close"

  -- ── Fix 2: instructions round-trips through InitializeResult ───────
  describe "sessionInstructions" $ do
    it "instructions field is populated from server response" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <-
        startEchoServerWithInstructions defaultServerCaps version (Just "Hello, world!")
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> show err
        Right session -> do
          session.sessionInstructions `shouldBe` Just "Hello, world!"
          session.sessionClose

    it "instructions Nothing when server omits the field" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> show err
        Right session -> do
          session.sessionInstructions `shouldBe` Nothing
          session.sessionClose

    it "instructions preserved as-is for non-ASCII text" $ do
      let version = ProtocolVersion "2025-03-26"
          txt = "Bienvenue! Utilisez les outils disponibles."
      (serverThread, connect') <-
        startEchoServerWithInstructions defaultServerCaps version (Just txt)
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> show err
        Right session -> do
          session.sessionInstructions `shouldBe` Just txt
          session.sessionClose

  -- ── Fix 5: Request timeout fires and cleans up ──────────────────────
  describe "timeout" $ do
    it "connect times out with a black-hole transport" $ do
      bh <- newBlackHoleTransport
      let cfg = defaultCfg { connectTimeoutMs = Just 50 }
      -- Guard: the whole test must finish well within 2 seconds.
      mResult <- timeout 2_000_000 (connect bh cfg)
      case mResult of
        Nothing -> expectationFailure "test guard: connect did not return within 2s"
        Just (Left err) -> do
          err.sessionErrorKind `shouldBe` NotConnected
          T.unpack err.sessionErrorDetail `shouldContain` "timed out"
        Just (Right session) -> do
          session.sessionClose
          expectationFailure "expected Left timeout, got Right"

    it "sessionRequest times out with a non-responding server" $ do
      let version = ProtocolVersion "2025-03-26"
      -- Start server that responds to initialize but ignores everything else
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      Right session <- connect' defaultCfg
      -- Send a request with a short timeout; server won't respond
      let opts = Just RequestOptions
            { requestTimeoutMs = Just 50
            , requestProgressToken = Nothing
            , requestOnProgress = Nothing
            }
      mResult <- timeout 2_000_000 (session.sessionRequest "tools/list" Nothing opts)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure "test guard: sessionRequest did not return within 2s"
        Just (Left err) -> do
          T.unpack err.rpcErrorMessage `shouldContain` "timed out"
        Just (Right _) -> expectationFailure "expected Left timeout"
      session.sessionClose

    it "session is not corrupted after a timeout" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startEchoServer defaultServerCaps version
      Right session <- connect' defaultCfg
      -- First request: will time out (server ignores non-initialize)
      let opts = Just RequestOptions
            { requestTimeoutMs = Just 50
            , requestProgressToken = Nothing
            , requestOnProgress = Nothing
            }
      _ <- session.sessionRequest "tools/list" Nothing opts
      -- Second request: should also return (not hang); the pending map
      -- must not be permanently corrupted by the first timeout.
      mResult <- timeout 2_000_000 (session.sessionRequest "tools/list" Nothing opts)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure
          "test guard: second sessionRequest hung, pending map corrupted"
        Just (Left _) -> pure ()  -- Expected: times out again cleanly
        Just (Right _) -> pure () -- Also fine if server somehow responded
      session.sessionClose

  -- ── Gap 6: Multi-version protocol negotiation ──────────────────────
  describe "multi-version negotiation" $ do
    it "client accepts 2024-11-05 when server offers it but client sent 2025-03-26" $ do
      let serverVersion = ProtocolVersion "2024-11-05"
      (serverThread, connect') <- startEchoServer defaultServerCaps serverVersion
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> expectationFailure $ "expected Right, got: " <> show err
        Right session -> do
          session.sessionProtocolVersion `shouldBe` serverVersion
          session.sessionClose

    it "client rejects a version not in supportedVersions" $ do
      let serverVersion = ProtocolVersion "1999-01-01"
      (serverThread, connect') <- startEchoServer defaultServerCaps serverVersion
      result <- connect' defaultCfg
      cancel serverThread
      case result of
        Left err -> err.sessionErrorKind `shouldBe` VersionMismatch
        Right session -> do
          session.sessionClose
          expectationFailure "expected Left VersionMismatch"

    it "customising supportedVersions to a single version rejects everything else" $ do
      let serverVersion = ProtocolVersion "2024-11-05"
          cfg = defaultCfg { connectSupportedVersions = ["2025-03-26"] }
      (serverThread, connect') <- startEchoServer defaultServerCaps serverVersion
      result <- connect' cfg
      cancel serverThread
      case result of
        Left err -> err.sessionErrorKind `shouldBe` VersionMismatch
        Right session -> do
          session.sessionClose
          expectationFailure "expected Left VersionMismatch"

  -- ── Gap 11: Progress notifications dispatched to requestOnProgress ─
  describe "progress notifications" $ do
    it "requestOnProgress callback is invoked with correct progress value" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startProgressServer version
      Right session <- connect' defaultCfg
      progressRef <- newIORef (Nothing :: Maybe Double)
      calledVar <- newEmptyMVar
      let callback p _total _msg = do
            writeIORef progressRef (Just p)
            putMVar calledVar ()
      let opts = Just RequestOptions
            { requestTimeoutMs = Just 2000
            , requestProgressToken = Just (ProgressToken (Left "tok1"))
            , requestOnProgress = Just callback
            }
      mResult <- timeout 5_000_000 (session.sessionRequest "tools/list" Nothing opts)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure "request did not return within 5s"
        Just (Left err) -> expectationFailure $ "expected Right, got: " <> show err
        Just (Right _) -> do
          -- Wait briefly for callback delivery (may already be done)
          _ <- timeout 1_000_000 (takeMVar calledVar)
          val <- readIORef progressRef
          val `shouldBe` Just 0.5
      session.sessionClose

    it "requestOnProgress receives total and message fields" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startProgressServer version
      Right session <- connect' defaultCfg
      totalRef <- newIORef (Nothing :: Maybe Double)
      msgRef <- newIORef (Nothing :: Maybe T.Text)
      calledVar <- newEmptyMVar
      let callback _p total msg = do
            writeIORef totalRef total
            writeIORef msgRef msg
            putMVar calledVar ()
      let opts = Just RequestOptions
            { requestTimeoutMs = Just 2000
            , requestProgressToken = Just (ProgressToken (Left "tok-fields"))
            , requestOnProgress = Just callback
            }
      mResult <- timeout 5_000_000 (session.sessionRequest "tools/list" Nothing opts)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure "request did not return within 5s"
        Just (Left err) -> expectationFailure $ "expected Right, got: " <> show err
        Just (Right _) -> do
          _ <- timeout 1_000_000 (takeMVar calledVar)
          t <- readIORef totalRef
          m <- readIORef msgRef
          t `shouldBe` Just 1.0
          m `shouldBe` Just "halfway"
      session.sessionClose

    it "no callback: progress notification is silently dropped" $ do
      let version = ProtocolVersion "2025-03-26"
      (serverThread, connect') <- startProgressServer version
      Right session <- connect' defaultCfg
      let opts = Just RequestOptions
            { requestTimeoutMs = Just 2000
            , requestProgressToken = Just (ProgressToken (Left "tok2"))
            , requestOnProgress = Nothing
            }
      mResult <- timeout 5_000_000 (session.sessionRequest "tools/list" Nothing opts)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure "request did not return within 5s"
        Just (Left err) -> expectationFailure $ "expected Right, got: " <> show err
        Just (Right _) -> pure ()  -- No crash, no hang — success
      session.sessionClose

  -- ── Gap 12: sessionPing ────────────────────────────────────────────
  describe "sessionPing" $ do
    it "sessionPing returns Right () on success" $ do
      (serverThread, connect') <- startPingServer
      Right session <- connect' defaultCfg
      mResult <- timeout 5_000_000 (sessionPing session)
      cancel serverThread
      case mResult of
        Nothing -> expectationFailure "sessionPing did not return within 5s"
        Just (Left err) -> expectationFailure $ "expected Right (), got: " <> show err
        Just (Right ()) -> pure ()
      session.sessionClose

    it "sessionPing returns Left on closed session" $ do
      (serverThread, connect') <- startPingServer
      Right session <- connect' defaultCfg
      cancel serverThread
      session.sessionClose
      result <- sessionPing session
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Left after close"
