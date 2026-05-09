module CodeStar.TypesSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Test.Hspec
import Test.QuickCheck

import CodeStar.Types hiding (spec)
import CodeStar.Types.Gen ()

-- --------------------------------------------------------------------
-- Roundtrip helper
-- --------------------------------------------------------------------

roundtrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Property
roundtrip x = fromJSON (toJSON x) === Aeson.Success x

-- --------------------------------------------------------------------
-- Tests
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "JSON roundtrip" $ do
    it "TaskType" $ property (roundtrip @TaskType)
    it "FailureClass" $ property (roundtrip @FailureClass)
    it "ModelRole" $ property (roundtrip @ModelRole)
    it "PlanningMode" $ property (roundtrip @PlanningMode)
    it "CheckResult" $ property (roundtrip @CheckResult)
    it "Evidence" $ property (roundtrip @Evidence)
    it "ControlSignal" $ property (roundtrip @ControlSignal)
    it "StepOutcome" $ property (roundtrip @StepOutcome)
    it "ObjectiveSpec" $ property (roundtrip @ObjectiveSpec)

  describe "worstSignal" $ do
    it "Done < Continue" $
      worstSignal [Done emptyEvidence, Continue] `shouldBe` Continue

    it "Continue < NeedsInput" $
      worstSignal [Continue, NeedsInput "q"] `shouldBe` NeedsInput "q"

    it "NeedsInput < Blocked" $
      worstSignal [NeedsInput "q", Blocked "r"] `shouldBe` Blocked "r"

    it "empty list returns Continue" $
      worstSignal [] `shouldBe` Continue

    it "singleton returns itself" $ property $ \sig ->
      worstSignal [sig] `shouldBe` sig

  describe "controlSignalSeverity ordering" $ do
    it "Done < Continue < NeedsInput < Blocked" $ do
      controlSignalSeverity (Done emptyEvidence) `shouldSatisfy` (< controlSignalSeverity Continue)
      controlSignalSeverity Continue `shouldSatisfy` (< controlSignalSeverity (NeedsInput ""))
      controlSignalSeverity (NeedsInput "") `shouldSatisfy` (< controlSignalSeverity (Blocked ""))

emptyEvidence :: Evidence
emptyEvidence = Evidence Passed Passed [] []
