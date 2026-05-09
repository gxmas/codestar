{-# LANGUAGE OverloadedStrings #-}

module Test.Network.JsonRpc.TransportSpec (spec) where

import Network.JsonRpc.Transport
import Test.Hspec

-- | A trivial mock transport that always succeeds. Exists as a
-- compile-time sanity check that the typeclass is implementable.
data MockTransport = MockTransport

instance Transport MockTransport where
  send _ _ = pure (Right ())
  close _ = pure ()

spec :: Spec
spec = do
  describe "TransportError" $ do
    it "can be constructed with no cause" $ do
      let err = TransportError "connection refused" Nothing
      transportMessage err `shouldBe` "connection refused"
      transportCause err `shouldSatisfy` \x -> case x of Nothing -> True; _ -> False

  describe "Mock transport" $ do
    it "send succeeds" $ do
      result <- send MockTransport "test"
      case result of
        Right () -> pure ()
        Left e -> expectationFailure ("expected Right, got Left: " ++ show e)

    it "close succeeds" $
      close MockTransport
