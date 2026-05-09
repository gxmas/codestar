module CodeStar.TreeSitterSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Test.Hspec

import CodeStar.TreeSitter
  ( grammarCount
  , languageForFile
  , loadGrammarRegistry
  , loadedLanguages
  , lookupLanguage
  )
import CodeStar.TreeSitter.Grammars (GrammarSpec (..), knownGrammars)

spec :: Spec
spec = do
  describe "languageForFile" $ do
    it "maps known extensions to expected language names" $ do
      languageForFile "foo.py" `shouldBe` Just "python"
      languageForFile "foo.tsx" `shouldBe` Just "tsx"
      languageForFile "foo.hs" `shouldBe` Just "haskell"
      languageForFile "foo.pyi" `shouldBe` Just "python"
      languageForFile "foo.R" `shouldBe` Just "r"

    it "maps extension-less known filenames (Dockerfile style)" $ do
      languageForFile "Dockerfile" `shouldBe` Just "dockerfile"

    it "returns Nothing for unknown extensions" $
      languageForFile "foo.unknownext" `shouldBe` Nothing

    it "returns Nothing for hidden/no-extension names with no direct mapping" $ do
      languageForFile ".env" `shouldBe` Nothing
      languageForFile "README" `shouldBe` Nothing

  describe "grammar registry invariants" $
    beforeAll loadGrammarRegistry $ do
      it "grammarCount equals loadedLanguages length" $ \reg ->
        grammarCount reg `shouldBe` length (loadedLanguages reg)

      it "lookupLanguage succeeds for every loaded language" $ \reg ->
        map (isJust . lookupLanguage reg) (loadedLanguages reg) `shouldSatisfy` and

      it "loaded languages are part of known grammar language set" $ \reg -> do
        let knownLangs = Set.fromList (map (.language) knownGrammars)
        map (`Set.member` knownLangs) (loadedLanguages reg) `shouldSatisfy` and

      it "lookupLanguage returns Nothing for unknown language label" $ \reg ->
        isNothing (lookupLanguage reg (Text.pack "__definitely-unknown__")) `shouldBe` True
