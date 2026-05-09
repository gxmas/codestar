{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.MCP.TransportSpec (spec) where

import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Streaming (Stream, Of)
import qualified Streaming as S
import qualified Streaming.Prelude as SP
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Transport
import Network.MCP.Types

------------------------------------------------------------------------
-- LoopbackTransport
------------------------------------------------------------------------

data LoopbackTransport = LoopbackTransport
  { ltQueue :: !(TQueue (Either TransportError MCPMessage))
  , ltClosed :: !(TVar Bool)
  }

newLoopback :: IO LoopbackTransport
newLoopback = LoopbackTransport <$> newTQueueIO <*> newTVarIO False

instance Transport LoopbackTransport where
  send t msg = do
    closed <- readTVarIO t.ltClosed
    if closed
      then pure (Left (TransportError TransportClosed "transport closed"))
      else do
        atomically (writeTQueue t.ltQueue (Right msg))
        pure (Right ())

  messages t = pure (loop t)
    where
      loop :: LoopbackTransport -> Stream (Of (Either TransportError MCPMessage)) IO ()
      loop lt = do
        mitem <- S.lift $ atomically $ do
          closed <- readTVar lt.ltClosed
          if closed
            then tryReadTQueue lt.ltQueue
            else Just <$> readTQueue lt.ltQueue
        case mitem of
          Nothing -> pure ()
          Just item -> SP.yield item >> loop lt

  close t = atomically (writeTVar t.ltClosed True)

------------------------------------------------------------------------
-- Generators
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
      [ MCPRequest <$> (JSONRPCRequest <$> arbitrary <*> shortText <*> pure Nothing <*> pure Nothing)
      , MCPNotification <$> (JSONRPCNotification <$> shortText <*> pure Nothing <*> pure Nothing)
      , MCPResult <$> (JSONRPCResult <$> arbitrary <*> pure (Aeson.Bool True) <*> pure Nothing)
      , MCPError <$> (JSONRPCError <$> liftArbitrary arbitrary <*> arbitrary)
      ]

instance Arbitrary TransportErrorKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TransportError where
  arbitrary = TransportError <$> arbitrary <*> shortText

------------------------------------------------------------------------
-- State machine operations
------------------------------------------------------------------------

data TransportOp = OpSend MCPMessage | OpClose
  deriving stock (Show)

instance Arbitrary TransportOp where
  arbitrary =
    frequency
      [ (8, OpSend <$> arbitrary)
      , (2, pure OpClose)
      ]

applyOp :: LoopbackTransport -> TransportOp -> IO ()
applyOp t (OpSend msg) = do
  _ <- send t msg
  pure ()
applyOp t OpClose = close t

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

drainMessages :: LoopbackTransport -> IO [Either TransportError MCPMessage]
drainMessages t = do
  stream <- messages t
  SP.toList_ stream

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "TransportErrorKind" $
    it "has exactly 4 constructors" $
      length [minBound .. maxBound :: TransportErrorKind] `shouldBe` 4

  describe "LoopbackTransport" $ do
    it "messages sent via send appear in order" $ do
      t <- newLoopback
      let msgs =
            [ MCPNotification (JSONRPCNotification "a" Nothing Nothing)
            , MCPNotification (JSONRPCNotification "b" Nothing Nothing)
            , MCPNotification (JSONRPCNotification "c" Nothing Nothing)
            ]
      mapM_ (send t) msgs
      close t
      received <- drainMessages t
      received `shouldBe` map Right msgs

    prop "arbitrary messages preserve order" $ \(msgs :: [MCPMessage]) -> ioProperty $ do
      t <- newLoopback
      mapM_ (send t) msgs
      close t
      received <- drainMessages t
      pure (received === map Right msgs)

    it "after close, messages stream terminates with empty result" $ do
      t <- newLoopback
      close t
      received <- drainMessages t
      received `shouldBe` []

    it "send after close returns Left TransportClosed" $ do
      t <- newLoopback
      close t
      let msg = MCPNotification (JSONRPCNotification "test" Nothing Nothing)
      result <- send t msg
      case result of
        Left err -> err.transportErrorKind `shouldBe` TransportClosed
        Right () -> expectationFailure "expected Left after close"

    it "close is idempotent" $ do
      t <- newLoopback
      close t
      close t
      received <- drainMessages t
      received `shouldBe` []

    prop "no operation sequence throws an exception" $ \(ops :: [TransportOp]) -> ioProperty $ do
      t <- newLoopback
      mapM_ (applyOp t) ops
      closed <- readTVarIO t.ltClosed
      if not closed then close t else pure ()
      _ <- drainMessages t
      pure (property True)
