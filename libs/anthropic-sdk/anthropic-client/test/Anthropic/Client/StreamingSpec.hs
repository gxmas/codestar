module Anthropic.Client.StreamingSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Test.Hspec

import Anthropic.Types
import Anthropic.Client.Streaming
import Anthropic.Protocol.Message (MessageResponse(..))
import Anthropic.Protocol.Stream

spec :: Spec
spec = do
  describe "defaultEventHandler" $ do
    it "has no-op callbacks" $ do
      let handler = defaultEventHandler
          msg = MessageResponse
                  (MessageId "msg_123")
                  (ModelId "claude-sonnet-4-20250514")
                  []
                  (Just EndTurn)
                  Nothing
                  (Usage 10 20 Nothing Nothing Nothing Nothing Nothing Nothing)
                  Nothing
      handler.onMessageStart msg
      handler.onContentBlockStart (ContentBlockIndex 0) (TextContent (TextBlock "hi" Nothing Nothing))
      handler.onContentBlockDelta (ContentBlockIndex 0) (TextDelta "hello")
      handler.onContentBlockStop (ContentBlockIndex 0) (TextContent (TextBlock "hello" Nothing Nothing))
      handler.onMessageComplete msg
      handler.onError (ApiError RateLimitError "slow down")
      -- If we get here without crashing, the no-ops work correctly

  describe "EventHandler record update" $ do
    it "allows overriding individual callbacks" $ do
      ref <- newIORef (0 :: Int)
      let handler = defaultEventHandler
            { onContentBlockDelta = \_ _ -> writeIORef ref 42
            }
      handler.onContentBlockDelta (ContentBlockIndex 0) (TextDelta "test")
      val <- readIORef ref
      val `shouldBe` 42
