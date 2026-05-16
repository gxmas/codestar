{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.JsonSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Monoid (Last (..))
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.Config.Json (parseJsonConfig)
import CodeStar.Config.Types qualified as CT
import CodeStar.Types (PlanningMode (..))

spec :: Spec
spec = describe "CodeStar.Config.Json" $ do
  it "parses legacy flat fields into partial config" $ do
    let input =
          BS8.pack
            "{\"provider\":\"openai\",\"planningMode\":\"dag\",\"maxSteps\":77}"
    case parseJsonConfig input of
      Left err -> expectationFailure ("Expected Right PartialConfig, got Left: " <> Text.unpack err)
      Right parsed -> do
        let CT.PartialConfig
              { CT.provider = p
              , CT.planningMode = pm
              , CT.budgets = CT.PartialBudgetSection{CT.maxSteps = ms}
              } = parsed
        getLast p `shouldBe` Just "openai"
        getLast pm `shouldBe` Just DagPlan
        getLast ms `shouldBe` Just 77

  it "parses nested legacy memoryConfig fields" $ do
    let input =
          BS8.pack
            "{\"memoryConfig\":{\"enabled\":false,\"maxEntries\":12,\"autoDiscover\":false}}"
    parsed <- either (fail . show) pure (parseJsonConfig input)
    let CT.PartialConfig
          { CT.memory =
              CT.PartialMemorySection
                { CT.enabled = en
                , CT.maxEntries = me
                , CT.autoDiscover = ad
                }
          } = parsed
    getLast en `shouldBe` Just False
    getLast me `shouldBe` Just 12
    getLast ad `shouldBe` Just False

  it "returns Left on malformed JSON" $ do
    case parseJsonConfig (BS8.pack "{bad json") of
      Left _ -> pure ()
      Right _ -> expectationFailure "Expected parse failure for malformed JSON"

