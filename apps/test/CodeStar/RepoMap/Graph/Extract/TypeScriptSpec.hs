{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.Graph.Extract.TypeScriptSpec (spec) where

import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.RepoMap.Graph (Tag (..), TagKind (..), extractTags)
import CodeStar.TreeSitter (GrammarRegistry, loadGrammarRegistry, lookupLanguage)

spec :: Spec
spec =
  describe "RepoMap extraction invariants (TypeScript)" $
    beforeAll loadGrammarRegistry $ do
      it "extracts declared symbols and class/type references" $ \reg -> do
        ensureTypeScriptGrammar reg
        let tsSrc =
              BS.intercalate
                "\n"
                [ "interface Box { value: number }"
                , "class Greeter {"
                , "  hello(): Box {"
                , "    return { value: 1 }"
                , "  }"
                , "}"
                , "type UserId = string"
                , "function run(): Box {"
                , "  const g = new Greeter()"
                , "  return g.hello()"
                , "}"
                ]
        tags <- extractTags reg "sample.ts" tsSrc
        let defs = [t.tagName | t <- tags, t.tagKind == Definition]
            refs = [t.tagName | t <- tags, t.tagKind == Reference]
        defs `shouldSatisfy` elem "Greeter"
        defs `shouldSatisfy` elem "hello"
        defs `shouldSatisfy` elem "Box"
        defs `shouldSatisfy` elem "UserId"
        defs `shouldSatisfy` elem "run"
        refs `shouldSatisfy` elem "Greeter"
        refs `shouldSatisfy` elem "Box"

      it "keeps output names non-empty" $ \reg -> do
        ensureTypeScriptGrammar reg
        let tsSrc =
              BS.intercalate
                "\n"
                [ "class A {}"
                , "function f(): A {"
                , "  return new A()"
                , "}"
                ]
        tags <- extractTags reg "focus.ts" tsSrc
        tags `shouldSatisfy` (not . null)
        tags `shouldSatisfy` all (not . Text.null . (.tagName))

ensureTypeScriptGrammar :: GrammarRegistry -> Expectation
ensureTypeScriptGrammar reg =
  if isJust (lookupLanguage reg "typescript")
    then pure ()
    else pendingWith "typescript grammar not installed; skipping typescript extraction regression checks"
