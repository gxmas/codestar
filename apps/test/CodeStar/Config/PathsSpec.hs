{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.PathsSpec (spec) where

import Data.List (isSuffixOf)
import Test.Hspec

import CodeStar.Config.Paths

spec :: Spec
spec = describe "CodeStar.Config.Paths" $ do
  it "projectDir appends .codestar to workspace" $
    projectDir "/tmp/ws" `shouldBe` "/tmp/ws/.codestar"

  it "XDG-based dirs are non-empty and include codestar suffix" $ do
    cfg <- globalConfigDir
    dat <- globalDataDir
    cache <- globalCacheDir
    cfg `shouldSatisfy` (not . null)
    dat `shouldSatisfy` (not . null)
    cache `shouldSatisfy` (not . null)
    cfg `shouldSatisfy` ("codestar" `isSuffixOf`)
    dat `shouldSatisfy` ("codestar" `isSuffixOf`)
    cache `shouldSatisfy` ("codestar" `isSuffixOf`)

  it "grammarsDir is nested under data dir with grammars suffix" $ do
    dat <- globalDataDir
    g <- grammarsDir
    g `shouldSatisfy` (isSuffixOf "/grammars")
    g `shouldSatisfy` (dat `isPrefixOf`)

isPrefixOf :: String -> String -> Bool
isPrefixOf pref s = take (length pref) s == pref
