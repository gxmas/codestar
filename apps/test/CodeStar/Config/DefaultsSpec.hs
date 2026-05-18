{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.DefaultsSpec (spec) where

import Test.Hspec

import CodeStar.Config.Defaults (defaultConfig, defaultModels, defaultActiveModel)
import CodeStar.Config.Types (Config (..), ContextSection (..), ServerSection (..), ShellSection (..))
import CodeStar.Types (PlanningMode (..))

spec :: Spec
spec = describe "CodeStar.Config.Defaults" $ do
  it "provides expected top-level defaults" $ do
    let Config{provider = p, planningMode = pm, workspacePath = wp, mcpEndpoints = mcps} = defaultConfig
    p `shouldBe` "anthropic"
    pm `shouldBe` NoPlan
    wp `shouldBe` "."
    mcps `shouldBe` []

  it "contains at least one default model entry" $ do
    length defaultModels `shouldSatisfy` (>= 1)
    defaultActiveModel `shouldBe` "sonnet"

  it "uses expected core numeric defaults" $ do
    let Config{server = ServerSection{port = p}, shell = ShellSection{defaultTimeout = dt}, context = ContextSection{maxTokens = mt}} = defaultConfig
    p `shouldBe` 8080
    dt `shouldBe` 30000
    mt `shouldBe` 200000
