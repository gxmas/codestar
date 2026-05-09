module Anthropic.Server.BatchSpec (spec) where

import Test.Hspec
import Test.QuickCheck


import Anthropic.Protocol.Batch (BatchItem (..), BatchStatus (..), BatchResponse (..), DeletedBatch (..))
import Anthropic.Protocol.Message (messageRequest, userMessage)
import Anthropic.Server.Batch

spec :: Spec
spec = do
  describe "BatchLifecycle" $ do
    it "creates batch in InProgress state" $ do
      lifecycle <- newBatchLifecycle
      let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
      result <- createBatch lifecycle items
      case result of
        Right response -> do
          let BatchResponse { processingStatus = status } = response
          status `shouldBe` BatchInProgress
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "polls batch successfully" $ do
      lifecycle <- newBatchLifecycle
      let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
      Right (BatchResponse { batchId = createdId }) <- createBatch lifecycle items
      result <- pollBatch lifecycle createdId
      case result of
        Right (BatchResponse { batchId = polledId }) -> polledId `shouldBe` createdId
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "returns BatchNotFound for unknown batch" $ do
      lifecycle <- newBatchLifecycle
      result <- pollBatch lifecycle "unknown_batch"
      result `shouldBe` Left (BatchNotFound "unknown_batch")

    describe "State transitions" $ do
      it "allows InProgress -> Canceling" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        result <- cancelBatch lifecycle bid
        case result of
          Right (BatchResponse { processingStatus = status }) -> status `shouldBe` BatchCanceling
          Left err -> expectationFailure $ "Expected success, got: " ++ show err

      it "allows InProgress -> Ended" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        result <- transitionBatch lifecycle bid BatchEnded
        case result of
          Right (BatchResponse { processingStatus = status }) -> status `shouldBe` BatchEnded
          Left err -> expectationFailure $ "Expected success, got: " ++ show err

      it "allows Canceling -> Ended" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        Right _ <- cancelBatch lifecycle bid
        result <- transitionBatch lifecycle bid BatchEnded
        case result of
          Right (BatchResponse { processingStatus = status }) -> status `shouldBe` BatchEnded
          Left err -> expectationFailure $ "Expected success, got: " ++ show err

      it "rejects Ended -> Canceling (backward transition)" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        Right _ <- transitionBatch lifecycle bid BatchEnded
        result <- transitionBatch lifecycle bid BatchCanceling
        result `shouldBe` Left (InvalidTransition BatchEnded BatchCanceling)

      it "rejects Ended -> InProgress (backward transition)" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        Right _ <- transitionBatch lifecycle bid BatchEnded
        result <- transitionBatch lifecycle bid BatchInProgress
        result `shouldBe` Left (InvalidTransition BatchEnded BatchInProgress)

      it "allows idempotent transitions (same state)" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        result <- transitionBatch lifecycle bid BatchInProgress
        case result of
          Right (BatchResponse { processingStatus = status }) -> status `shouldBe` BatchInProgress
          Left err -> expectationFailure $ "Expected success, got: " ++ show err

    describe "Deletion" $ do
      it "allows deletion of ended batch" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        Right _ <- transitionBatch lifecycle bid BatchEnded
        result <- deleteBatch lifecycle bid
        case result of
          Right (DeletedBatch { batchId = deletedId }) -> deletedId `shouldBe` bid
          Left err -> expectationFailure $ "Expected success, got: " ++ show err

      it "rejects deletion of non-ended batch" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        result <- deleteBatch lifecycle bid
        result `shouldBe` Left (BatchNotEnded bid)

      it "batch not found after deletion" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        Right _ <- transitionBatch lifecycle bid BatchEnded
        Right _ <- deleteBatch lifecycle bid
        result <- pollBatch lifecycle bid
        result `shouldBe` Left (BatchNotFound bid)

    describe "Expiry checking" $ do
      it "reports not expired for fresh batch" $ do
        lifecycle <- newBatchLifecycle
        let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
        Right (BatchResponse { batchId = bid }) <- createBatch lifecycle items
        result <- checkExpiry lifecycle bid
        result `shouldBe` Right False

      it "property: valid state transitions never fail" $ property $
        forAll (elements validTransitions) $ \(from, to) ->
          ioProperty $ do
            lifecycle <- newBatchLifecycle
            let items = [BatchItem "req1" (messageRequest "claude-sonnet-4" [userMessage "Hi"] 100)]
            Right created <- createBatch lifecycle items
            -- Transition to 'from' state
            when (from /= BatchInProgress) $
              void $ transitionBatch lifecycle created.batchId from
            -- Then transition to 'to' state
            result <- transitionBatch lifecycle created.batchId to
            pure $ isRight result

validTransitions :: [(BatchStatus, BatchStatus)]
validTransitions =
  [ (BatchInProgress, BatchInProgress)  -- Idempotent
  , (BatchInProgress, BatchCanceling)
  , (BatchInProgress, BatchEnded)
  , (BatchCanceling, BatchCanceling)    -- Idempotent
  , (BatchCanceling, BatchEnded)
  , (BatchEnded, BatchEnded)            -- Idempotent
  ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

when :: Monad m => Bool -> m () -> m ()
when True action = action
when False _ = pure ()

void :: Functor f => f a -> f ()
void = fmap (const ())
