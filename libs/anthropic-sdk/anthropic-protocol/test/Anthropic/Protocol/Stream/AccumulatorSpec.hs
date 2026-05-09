module Anthropic.Protocol.Stream.AccumulatorSpec (spec) where

import Test.Hspec

import Anthropic.Types
import Anthropic.Protocol.Message
import Anthropic.Protocol.Stream
import Anthropic.Protocol.Stream.Accumulator

spec :: Spec
spec = do
  describe "initialStreamState" $ do
    it "starts in AwaitingStart" $
      streamPhase initialStreamState `shouldBe` AwaitingStart

    it "has no accumulated blocks" $
      accumulatedBlocks initialStreamState `shouldBe` []

    it "is not complete" $
      isComplete initialStreamState `shouldBe` False

  describe "accumulate" $ do
    it "transitions to InMessage on message_start" $ do
      let resp = mkResponse
          st = accumulate initialStreamState (EventMessageStart resp)
      streamPhase st `shouldBe` InMessage

    it "transitions to InBlock on content_block_start" $ do
      let resp = mkResponse
          st0 = accumulate initialStreamState (EventMessageStart resp)
          block = TextContent (TextBlock "" Nothing Nothing)
          st1 = accumulate st0 (EventContentBlockStart 0 block)
      streamPhase st1 `shouldBe` InBlock
      length (accumulatedBlocks st1) `shouldBe` 1

    it "accumulates text deltas" $ do
      let resp = mkResponse
          block = TextContent (TextBlock "" Nothing Nothing)
          st = foldl accumulate initialStreamState
            [ EventMessageStart resp
            , EventContentBlockStart 0 block
            , EventContentBlockDelta 0 (TextDelta "Hello")
            , EventContentBlockDelta 0 (TextDelta " world")
            ]
      case accumulatedBlocks st of
        [TextContent tb] -> tb.text `shouldBe` "Hello world"
        other -> expectationFailure $ "Expected single TextContent, got: " ++ show other

    it "transitions to BetweenBlocks on content_block_stop" $ do
      let resp = mkResponse
          block = TextContent (TextBlock "" Nothing Nothing)
          st = foldl accumulate initialStreamState
            [ EventMessageStart resp
            , EventContentBlockStart 0 block
            , EventContentBlockStop 0
            ]
      streamPhase st `shouldBe` BetweenBlocks

    it "transitions to StreamDone on message_stop" $ do
      let resp = mkResponse
          usage = Usage 10 20 Nothing Nothing Nothing Nothing Nothing Nothing
          st = foldl accumulate initialStreamState
            [ EventMessageStart resp
            , EventMessageDelta (Just EndTurn) Nothing usage
            , EventMessageStop
            ]
      streamPhase st `shouldBe` StreamDone
      isComplete st `shouldBe` True

    it "ping does not change phase" $ do
      let st = accumulate initialStreamState EventPing
      streamPhase st `shouldBe` AwaitingStart

    it "handles multiple content blocks" $ do
      let resp = mkResponse
          tb1 = TextContent (TextBlock "" Nothing Nothing)
          tb2 = TextContent (TextBlock "" Nothing Nothing)
          usage = Usage 10 20 Nothing Nothing Nothing Nothing Nothing Nothing
          st = foldl accumulate initialStreamState
            [ EventMessageStart resp
            , EventContentBlockStart 0 tb1
            , EventContentBlockDelta 0 (TextDelta "First")
            , EventContentBlockStop 0
            , EventContentBlockStart 1 tb2
            , EventContentBlockDelta 1 (TextDelta "Second")
            , EventContentBlockStop 1
            , EventMessageDelta (Just EndTurn) Nothing usage
            , EventMessageStop
            ]
      length (accumulatedBlocks st) `shouldBe` 2
      isComplete st `shouldBe` True

  describe "validateOrdering" $ do
    it "accepts message_start in AwaitingStart" $
      validateOrdering initialStreamState (EventMessageStart mkResponse)
        `shouldBe` Nothing

    it "rejects content_block_start in AwaitingStart" $
      validateOrdering initialStreamState
        (EventContentBlockStart 0 (TextContent (TextBlock "" Nothing Nothing)))
        `shouldNotBe` Nothing

    it "accepts ping in any phase" $ do
      validateOrdering initialStreamState EventPing `shouldBe` Nothing
      let st = accumulate initialStreamState (EventMessageStart mkResponse)
      validateOrdering st EventPing `shouldBe` Nothing

    it "rejects message_start after message has started" $ do
      let st = accumulate initialStreamState (EventMessageStart mkResponse)
      validateOrdering st (EventMessageStart mkResponse) `shouldNotBe` Nothing

    it "rejects events after StreamDone (except ping)" $ do
      let st = StreamState StreamDone Nothing [] Nothing
      validateOrdering st (EventContentBlockStart 0 (TextContent (TextBlock "" Nothing Nothing)))
        `shouldNotBe` Nothing
      validateOrdering st EventPing `shouldBe` Nothing

    it "accepts content_block_delta in InBlock" $ do
      let st = StreamState InBlock (Just 0) [] Nothing
      validateOrdering st (EventContentBlockDelta 0 (TextDelta "x")) `shouldBe` Nothing

    it "rejects message_stop in InBlock" $ do
      let st = StreamState InBlock (Just 0) [] Nothing
      validateOrdering st EventMessageStop `shouldNotBe` Nothing

-- Helper to build a minimal MessageResponse
mkResponse :: MessageResponse
mkResponse = MessageResponse
  { id = "msg_test"
  , model = "claude-sonnet-4-20250514"
  , content = []
  , stopReason = Nothing
  , stopSequence = Nothing
  , usage = Usage 0 0 Nothing Nothing Nothing Nothing Nothing Nothing
  , container = Nothing
  }
