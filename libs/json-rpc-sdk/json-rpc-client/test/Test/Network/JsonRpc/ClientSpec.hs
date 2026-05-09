{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.ClientSpec (spec) where

import Data.Aeson (Value (..))
import Network.JsonRpc.Client
import Network.JsonRpc.Types
import Test.Hspec hiding (pending)

spec :: Spec
spec = do
  describe "Request id generation" $ do
    it "produces different ids for consecutive requests" $ do
      c <- newClient defaultClientConfig
      pr1 <- request c "method1" NoParams
      pr2 <- request c "method2" NoParams
      pendingRequestId pr1 `shouldNotBe` pendingRequestId pr2

  describe "Request tracking" $ do
    it "request appears in pending" $ do
      c <- newClient defaultClientConfig
      _ <- request c "method1" NoParams
      ps <- pending c
      length ps `shouldBe` 1

    it "handleResponse removes from pending" $ do
      c <- newClient defaultClientConfig
      pr <- request c "method1" NoParams
      let reqId = pendingRequestId pr
      let resp = ResponseSuccess (Success Null (Just reqId))
      result <- handleResponse c resp
      case result of
        Matched mid _ -> mid `shouldBe` reqId
        Unmatched _ -> expectationFailure "expected Matched"
      ps <- pending c
      length ps `shouldBe` 0

  describe "Unmatched response" $ do
    it "returns Unmatched for unknown id" $ do
      c <- newClient defaultClientConfig
      let resp = ResponseSuccess (Success Null (Just (IdNum 9999)))
      result <- handleResponse c resp
      case result of
        Unmatched _ -> pure ()
        Matched _ _ -> expectationFailure "expected Unmatched"

  describe "Notification" $ do
    it "notify does not add to pending" $ do
      c <- newClient defaultClientConfig
      _ <- notify c "update" NoParams
      ps <- pending c
      length ps `shouldBe` 0

  describe "Batch building" $ do
    it "batch produces correct number of messages" $ do
      c <- newClient defaultClientConfig
      pb <- batch c
        [ IntentRequest "method1" NoParams
        , IntentNotify "update" NoParams
        , IntentRequest "method2" NoParams
        ]
      length (pendingBatchMessages pb) `shouldBe` 3
      length (pendingBatchIds pb) `shouldBe` 2

    it "batch tracks only request ids" $ do
      c <- newClient defaultClientConfig
      pb <- batch c
        [ IntentNotify "update" NoParams
        , IntentNotify "update2" NoParams
        ]
      length (pendingBatchIds pb) `shouldBe` 0

  describe "Batch response handling" $ do
    it "handleBatchResponse correlates correctly" $ do
      c <- newClient defaultClientConfig
      pb <- batch c
        [ IntentRequest "m1" NoParams
        , IntentRequest "m2" NoParams
        ]
      case pendingBatchIds pb of
        [id1, id2] -> do
          let resps =
                [ ResponseSuccess (Success Null (Just id1))
                , ResponseSuccess (Success Null (Just id2))
                ]
          results <- handleBatchResponse c resps
          length results `shouldBe` 2
          all isMatched results `shouldBe` True
          ps <- pending c
          length ps `shouldBe` 0
        ids -> expectationFailure $ "expected 2 batch ids, got " <> show (length ids)

  describe "Custom id generator" $ do
    it "uses the custom generator" $ do
      c <- newClient defaultClientConfig
      setIdGenerator c (IdGenerator (pure (IdText "custom-id")))
      pr <- request c "method1" NoParams
      pendingRequestId pr `shouldBe` IdText "custom-id"

isMatched :: MatchResult -> Bool
isMatched (Matched _ _) = True
isMatched _ = False
