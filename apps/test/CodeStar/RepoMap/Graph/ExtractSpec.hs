{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.Graph.ExtractSpec (spec) where

import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import CodeStar.RepoMap.Graph
  ( extractTagsDetailed
  , querySourceModeLabel
  , TagExtraction (..)
  )
import CodeStar.TreeSitter (GrammarRegistry (..))

spec :: Spec
spec = describe "CodeStar.RepoMap.Graph.Extract" $ do
  it "returns SkippedUnsupported for unknown file extension" $ do
    let reg = GrammarRegistry Map.empty
    result <- extractTagsDetailed reg "foo.unknown" "x"
    result `shouldBe` SkippedUnsupported

  it "returns SkippedNoGrammar when language is known but not loaded" $ do
    let reg = GrammarRegistry Map.empty
    result <- extractTagsDetailed reg "foo.py" "print(1)"
    result `shouldBe` SkippedNoGrammar

  it "querySourceModeLabel defaults to embedded when env unset" $ do
    withUnset "CODESTAR_QUERIES_DIR" $ do
      label <- querySourceModeLabel
      label `shouldBe` "embedded"

withUnset :: String -> IO a -> IO a
withUnset name action = do
  old <- lookupEnv name
  unsetEnv name
  result <- action
  case old of
    Just v -> setEnv name v
    Nothing -> unsetEnv name
  pure result
