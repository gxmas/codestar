{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.DefaultsSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import CodeStar.Config.Defaults (defaultConfig, defaultModelRoles)
import CodeStar.Config.Types (Config (..), ContextSection (..), ServerSection (..), ShellSection (..))
import CodeStar.Types (ModelRole (..), PlanningMode (..))

spec :: Spec
spec = describe "CodeStar.Config.Defaults" $ do
  it "provides expected top-level defaults" $ do
    let Config{provider = p, planningMode = pm, workspacePath = wp, mcpEndpoints = mcps} = defaultConfig
    p `shouldBe` "anthropic"
    pm `shouldBe` NoPlan
    wp `shouldBe` "."
    mcps `shouldBe` []

  it "contains all required model role defaults" $ do
    Map.keysSet defaultModelRoles `shouldBe` Map.keysSet (Map.fromList [(Architect, ()), (Coder, ()), (Validator, ()), (Summarizer, ())])
    Map.size defaultModelRoles `shouldBe` 4

  it "uses expected core numeric defaults" $ do
    let Config{server = ServerSection{port = p}, shell = ShellSection{defaultTimeout = dt}, context = ContextSection{maxTokens = mt}} = defaultConfig
    p `shouldBe` 8080
    dt `shouldBe` 30000
    mt `shouldBe` 200000
