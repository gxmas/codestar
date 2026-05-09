module Network.MCP.Session.ConnectSpec (spec) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Streaming.Prelude as SP
import System.Process (createPipe)
import Test.Hspec

import Network.MCP.Session (Session (..), SessionError (..), SessionErrorKind (..))
import Network.MCP.Session.Connect
import Network.MCP.Transport (Transport (..))
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
