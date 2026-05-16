{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.Graph.Extract.HaskellSpec (spec) where

import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import CodeStar.RepoMap.Graph (Tag (..), TagKind (..), extractTags, isIdentChar)
import CodeStar.TreeSitter (GrammarRegistry, loadGrammarRegistry)

haskellFiles :: [FilePath]
haskellFiles =
  [ "codestar-cli/CLI.hs"
  , "codestar-serve/Server.hs"
  , "codestar-client/Client.hs"
  , "src/CodeStar/RepoMap/Graph.hs"
  , "src/CodeStar/Tools/Edit.hs"
  , "test/CodeStar/ConfigSpec.hs"
  ]

haskellFileGen :: Gen FilePath
haskellFileGen = elements haskellFiles

extractFileTags :: GrammarRegistry -> FilePath -> IO [Tag]
extractFileTags reg path = do
  src <- BS.readFile path
  extractTags reg path src

spec :: Spec
spec =
  describe "RepoMap extraction invariants (Haskell)" $
    beforeAll loadGrammarRegistry $ do
      it "filters local short temp bindings from Tools/Edit map output candidates" $ \reg -> do
        tags <- extractFileTags reg "src/CodeStar/Tools/Edit.hs"
        let names = map (.tagName) tags
        names `shouldSatisfy` notElem "n"
        names `shouldSatisfy` notElem "o"
        names `shouldSatisfy` notElem "p"
        names `shouldSatisfy` notElem "r"
        names `shouldSatisfy` notElem "sr"

      it "still includes top-level declarations after filtering" $ \reg -> do
        tags <- extractFileTags reg "src/CodeStar/Tools/Edit.hs"
        let names = map (.tagName) tags
        names `shouldSatisfy` elem "parseEditInput"
        names `shouldSatisfy` elem "editToolHandler"

      it "is deterministic across repeated extraction runs" $ \reg ->
        property $ forAll haskellFileGen $ \path ->
          monadicIO $ do
            t1 <- run (extractFileTags reg path)
            t2 <- run (extractFileTags reg path)
            assert (t1 == t2)

      it "emits definition tags only for Haskell query path" $ \reg ->
        property $ forAll haskellFileGen $ \path ->
          monadicIO $ do
            tags <- run (extractFileTags reg path)
            assert (all ((== Definition) . (.tagKind)) tags)

      it "emits non-empty names containing only identifier chars" $ \reg ->
        property $ forAll haskellFileGen $ \path ->
          monadicIO $ do
            tags <- run (extractFileTags reg path)
            let validName t =
                  not (Text.null t.tagName)
                    && Text.all isIdentChar t.tagName
            assert (all validName tags)

      it "tag line numbers are non-negative" $ \reg ->
        property $ forAll haskellFileGen $ \path ->
          monadicIO $ do
            tags <- run (extractFileTags reg path)
            assert (all ((>= 0) . (.tagLine)) tags)

      it "no two definitions at the same line in the same file" $ \reg ->
        property $ forAll haskellFileGen $ \path ->
          monadicIO $ do
            tags <- run (extractFileTags reg path)
            let defs = filter ((== Definition) . (.tagKind)) tags
                keys = map (\t -> (t.tagFile, t.tagLine)) defs
            assert (length keys == length (nub keys))
