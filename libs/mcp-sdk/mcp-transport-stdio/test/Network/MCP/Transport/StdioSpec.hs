{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.MCP.Transport.StdioSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Streaming.Prelude as SP
import System.Process (createPipe)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Codec (McpCodec (..), Codec (..))
import Network.MCP.Transport (Transport (..), TransportError (..), TransportErrorKind (..))
import Network.MCP.Transport.Stdio (new, newWithHandles, stdioStderr)
import System.Timeout (timeout)
import Network.MCP.Types

------------------------------------------------------------------------
-- Arbitrary instances (minimal set; orphans suppressed above)
------------------------------------------------------------------------

shortText :: Gen Text
shortText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary RequestId where
  arbitrary = RequestId <$> oneof [Left <$> shortText, Right . abs <$> arbitrary]

instance Arbitrary RPCError where
  arbitrary = RPCError <$> arbitrary <*> shortText <*> pure Nothing

instance Arbitrary MCPMessage where
  arbitrary =
    oneof
      [ MCPRequest  <$> (JSONRPCRequest  <$> arbitrary <*> shortText <*> pure Nothing <*> pure Nothing)
      , MCPNotification <$> (JSONRPCNotification <$> shortText <*> pure Nothing <*> pure Nothing)
      , MCPResult   <$> (JSONRPCResult   <$> arbitrary <*> pure (Aeson.Bool True) <*> pure Nothing)
      , MCPError    <$> (JSONRPCError    <$> liftArbitrary arbitrary <*> arbitrary)
      ]

spec :: Spec
spec = do
  describe "Transport contract (cat subprocess)" $ do
    it "send then receive preserves a message" $ do
      transport <- new "cat" []
      let msg = mkPing 1
      result <- send transport msg
      result `shouldBe` Right ()
      stream <- messages transport
      items <- SP.toList_ (SP.take 1 stream)
      close transport
      items `shouldBe` [Right msg]

    it "multiple sends arrive in FIFO order" $ do
      transport <- new "cat" []
      let msgs = [mkPing 1, mkPing 2, mkPing 3]
      mapM_ (send transport) msgs
      stream <- messages transport
      items <- SP.toList_ (SP.take 3 stream)
      close transport
      map extractMsg items `shouldBe` msgs

    it "notifications round-trip through cat" $ do
      transport <- new "cat" []
      let msg = mkNotif "test/ping"
      result <- send transport msg
      result `shouldBe` Right ()
      stream <- messages transport
      items <- SP.toList_ (SP.take 1 stream)
      close transport
      items `shouldBe` [Right msg]

    it "result messages round-trip through cat" $ do
      transport <- new "cat" []
      let msg = mkResult 42
      result <- send transport msg
      result `shouldBe` Right ()
      stream <- messages transport
      items <- SP.toList_ (SP.take 1 stream)
      close transport
      items `shouldBe` [Right msg]

    it "send after close returns Left TransportClosed" $ do
      transport <- new "cat" []
      close transport
      threadDelay 10000
      result <- send transport (mkPing 1)
      case result of
        Left err -> err.transportErrorKind `shouldBe` TransportClosed
        Right () -> expectationFailure "expected Left, got Right"

    it "close is idempotent" $ do
      transport <- new "cat" []
      close transport
      close transport  -- should not throw

    it "create and immediately close does not throw" $ do
      transport <- new "cat" []
      close transport

  -- ── P0: Newline-delimited JSON codec roundtrip ────────────────────────
  describe "codec roundtrip (P0)" $ do
    prop "encode then decode recovers the original MCPMessage" $
      \(msg :: MCPMessage) ->
        case encode McpCodec msg of
          Left err -> counterexample ("encode failed: " <> show err) False
          Right bs -> decode McpCodec bs === Right msg

    prop "encode never fails on well-formed messages" $
      \(msg :: MCPMessage) ->
        case encode McpCodec msg of
          Left _  -> False
          Right _ -> True

  -- ── P1: Transport contract via in-process pipe ────────────────────────
  describe "transport contract via in-process pipe (P1)" $ do
    prop "messages arrive in FIFO order through a loopback pipe" $
      \(msgs :: [MCPMessage]) -> not (null msgs) ==> ioProperty $ do
        -- createPipe returns (readEnd, writeEnd); data written to writeEnd
        -- can be read from readEnd.
        (readEnd, writeEnd) <- createPipe
        t <- newWithHandles writeEnd readEnd
        mapM_ (send t) msgs
        stream <- messages t
        received <- SP.toList_ (SP.take (length msgs) stream)
        close t
        pure (map extractRight received === msgs)

    it "close is idempotent for handle-based transport" $ do
      (readEnd, writeEnd) <- createPipe
      t <- newWithHandles writeEnd readEnd
      close t
      close t

    it "send after close returns Left TransportClosed for handle-based transport" $ do
      (readEnd, writeEnd) <- createPipe
      t <- newWithHandles writeEnd readEnd
      close t
      threadDelay 10000
      result <- send t (mkPing 99)
      case result of
        Left err -> err.transportErrorKind `shouldBe` TransportClosed
        Right () -> expectationFailure "expected Left TransportClosed"

  -- ── Gap 8: stdioStderr captures subprocess stderr ────────────────────
  describe "stdioStderr" $ do
    it "stdioStderr returns Nothing before subprocess writes to stderr" $ do
      transport <- new "cat" []
      result <- stdioStderr transport
      result `shouldBe` Nothing
      close transport

    it "stdioStderr returns Just line when subprocess writes to stderr" $ do
      transport <- new "sh" ["-c", "echo hello >&2"]
      -- Wait for the subprocess to write and the drain loop to pick it up
      let poll n
            | n <= 0 = pure Nothing
            | otherwise = do
                r <- stdioStderr transport
                case r of
                  Just _  -> pure r
                  Nothing -> threadDelay 50_000 >> poll (n - 1)
      result <- poll (40 :: Int)  -- up to 2 seconds
      close transport
      case result of
        Just line -> line `shouldSatisfy` T.isInfixOf "hello"
        Nothing   -> expectationFailure "timed out waiting for stderr line"

    it "stdioStderr returns Nothing for handle-based transport (newWithHandles)" $ do
      (readEnd, writeEnd) <- createPipe
      t <- newWithHandles writeEnd readEnd
      result <- stdioStderr t
      result `shouldBe` Nothing
      close t

  -- ── Gap 18: Shutdown sequence ──────────────────────────────────────────
  describe "shutdown sequence" $ do
    it "close sends SIGTERM to subprocess (does not hang)" $ do
      transport <- new "sleep" ["30"]
      result <- timeout 15_000_000 (close transport)
      case result of
        Just () -> pure ()
        Nothing -> expectationFailure "close hung — SIGTERM not sent or process not killed"

    it "close is idempotent for subprocess transport" $ do
      transport <- new "cat" []
      result <- timeout 5_000_000 $ do
        close transport
        close transport
      case result of
        Just () -> pure ()
        Nothing -> expectationFailure "close hung on second call"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

mkPing :: Int -> MCPMessage
mkPing n = MCPRequest JSONRPCRequest
  { requestId = RequestId (Right n)
  , requestMethod = "ping"
  , requestParams = Nothing
  , requestMeta = Nothing
  }

mkNotif :: Text -> MCPMessage
mkNotif method = MCPNotification JSONRPCNotification
  { notificationMethod = method
  , notificationParams = Nothing
  , notificationMeta = Nothing
  }

mkResult :: Int -> MCPMessage
mkResult n = MCPResult JSONRPCResult
  { resultId = RequestId (Right n)
  , resultResult = Aeson.object ["status" Aeson..= ("ok" :: Text)]
  , resultMeta = Nothing
  }

extractMsg :: Either TransportError MCPMessage -> MCPMessage
extractMsg (Right msg) = msg
extractMsg (Left err) = error $ "expected Right, got Left: " <> show err

extractRight :: Either TransportError MCPMessage -> MCPMessage
extractRight (Right msg) = msg
extractRight (Left err)  = error $ "unexpected transport error: " <> show err
