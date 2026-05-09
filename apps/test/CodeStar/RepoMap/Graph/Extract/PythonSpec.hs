{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.Graph.Extract.PythonSpec (spec) where

import Data.ByteString qualified as BS
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.RepoMap.Graph (Tag (..), TagKind (..), extractTags)
import CodeStar.TreeSitter (GrammarRegistry, loadGrammarRegistry, lookupLanguage)

spec :: Spec
spec =
  describe "RepoMap extraction invariants (Python)" $
    beforeAll loadGrammarRegistry $ do
      it "extracts class/function definitions and call references" $ \reg -> do
        ensurePythonGrammar reg
        let pySrc =
              BS.intercalate
                "\n"
                [ "class Greeter:"
                , "    def hello(self):"
                , "        return \"hi\""
                , ""
                , "def run():"
                , "    g = Greeter()"
                , "    g.hello()"
                , "    print(\"ok\")"
                ]
        tags <- extractTags reg "sample.py" pySrc
        let defs = [t.tagName | t <- tags, t.tagKind == Definition]
            refs = [t.tagName | t <- tags, t.tagKind == Reference]
        defs `shouldSatisfy` elem "Greeter"
        defs `shouldSatisfy` elem "hello"
        defs `shouldSatisfy` elem "run"
        refs `shouldSatisfy` elem "Greeter"
        refs `shouldSatisfy` elem "hello"
        refs `shouldSatisfy` elem "print"

      it "keeps output declaration/reference focused (non-empty names)" $ \reg -> do
        ensurePythonGrammar reg
        let pySrc =
              BS.intercalate
                "\n"
                [ "class A:"
                , "    def f(self):"
                , "        pass"
                , ""
                , "def g():"
                , "    A()"
                ]
        tags <- extractTags reg "focus.py" pySrc
        tags `shouldSatisfy` (not . null)
        tags `shouldSatisfy` all (not . Text.null . (.tagName))

ensurePythonGrammar :: GrammarRegistry -> Expectation
ensurePythonGrammar reg =
  if isJust (lookupLanguage reg "python")
    then pure ()
    else pendingWith "python grammar not installed; skipping python extraction regression checks"
