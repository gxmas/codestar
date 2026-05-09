module Anthropic.Server.ServiceTierSpec (spec) where

import Test.Hspec

import Anthropic.Types (ServiceTierPreference (..))
import Anthropic.Protocol.Message (messageRequest, userMessage)
import Anthropic.Server.ServiceTier

spec :: Spec
spec = do
  describe "ServiceTierRouter" $ do
    it "assigns tier based on preference" $ do
      router <- newServiceTierRouter
      let req = messageRequest "claude-sonnet-4" [userMessage "Hi"] 100
      assignment <- assignTier router req ServiceTierAuto
      assignment.requested `shouldBe` ServiceTierAuto

    it "assigns standard when StandardOnly requested" $ do
      router <- newServiceTierRouter
      let req = messageRequest "claude-sonnet-4" [userMessage "Hi"] 100
      assignment <- assignTier router req StandardOnly
      assignment.assigned `shouldBe` TierStandard
      assignment.requested `shouldBe` StandardOnly

    it "includes capacity burndown rate" $ do
      router <- newServiceTierRouter
      let req = messageRequest "claude-sonnet-4" [userMessage "Hi"] 100
      assignment <- assignTier router req ServiceTierAuto
      assignment.capacityBurndownRate `shouldSatisfy` isJust

    it "assigns valid tier for auto preference" $ do
      router <- newServiceTierRouter
      let req = messageRequest "claude-sonnet-4" [userMessage "Hi"] 100
      assignment <- assignTier router req ServiceTierAuto
      assignment.assigned `shouldSatisfy` isValidTier

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isValidTier :: AssignedTier -> Bool
isValidTier TierStandard = True
isValidTier TierPriority = True
isValidTier TierBatch = True
