module CodeStar.TreeSitter.GrammarsSpec (spec) where

import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.TreeSitter.Grammars
  ( GrammarSpec (..)
  , grammarByExtension
  , grammarLibPath
  , knownGrammars
  )

spec :: Spec
spec = do
  describe "knownGrammars" $ do
    it "is non-empty" $
      knownGrammars `shouldSatisfy` (not . null)

    it "indexes each declared extension in grammarByExtension" $
      map declaredExtsKnown knownGrammars `shouldSatisfy` and

    it "has unique language labels and extension keys" $ do
      let langs = map (.language) knownGrammars
          exts = concatMap (.extensions) knownGrammars
      length langs `shouldBe` Set.size (Set.fromList langs)
      length exts `shouldBe` Set.size (Set.fromList exts)

  describe "grammarLibPath" $
    it "uses the expected shared library basename format" $
      map hasExpectedStem knownGrammars `shouldSatisfy` and

  describe "grammarByExtension" $
    it "contains exactly all declared extensions" $ do
      let expected = Set.fromList (concatMap (.extensions) knownGrammars)
          actual = Set.fromList (Map.keys grammarByExtension)
      actual `shouldBe` expected
 where
  declaredExtsKnown grammar =
    all (\ext -> maybe False ((== grammar.language) . (.language)) (Map.lookup ext grammarByExtension)) grammar.extensions

  hasExpectedStem grammar =
    let p = grammarLibPath "/tmp/grammars" grammar
        stem = "libtree-sitter-" <> Text.unpack grammar.language
     in stem `isInfixOf` p
