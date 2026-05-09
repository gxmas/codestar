module CodeStar.RepoMap.RenderSpec (spec) where

import Data.List (isPrefixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck

import CodeStar.RepoMap.Graph (Tag (..), TagKind (..))
import CodeStar.RepoMap.Render
  ( binarySearchFit
  , estimateTokens
  , formatSelected
  , groupByFile
  , rankDefs
  )

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genText :: Gen Text
genText = Text.pack <$> listOf (elements (['a' .. 'z'] ++ " ."))

genFile :: Gen FilePath
genFile = elements ["a.hs", "b.hs", "c.hs", "d.hs"]

genRenderTag :: Gen Tag
genRenderTag =
  Tag
    <$> genFile
    <*> (Text.pack <$> listOf1 (elements ['a' .. 'z']))
    <*> chooseInt (0, 500)
    <*> pure Definition

genScores :: [Tag] -> Gen (Map FilePath Double)
genScores tags = do
  let files = nub (map (.tagFile) tags)
  scores <- vectorOf (length files) (choose (0.0, 1.0))
  pure (Map.fromList (zip files scores))

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "estimateTokens" $ do
    it "is always >= 1" $
      property $
        forAll genText $ \t ->
          estimateTokens t >= 1

    it "is monotone in text length" $
      property $
        forAll genText $ \a ->
          forAll genText $ \b ->
            estimateTokens (a <> b) >= estimateTokens a

  describe "groupByFile" $ do
    it "preserves all tag names" $
      property $
        forAll (listOf genRenderTag) $ \tags ->
          let byFile = groupByFile tags
              allNames = Set.fromList (map (.tagName) tags)
              grouped = Set.unions (Map.elems byFile)
           in allNames `Set.isSubsetOf` grouped

    it "preserves all tag files as keys" $
      property $
        forAll (listOf genRenderTag) $ \tags ->
          let byFile = groupByFile tags
           in all (\t -> Map.member t.tagFile byFile) tags

    it "empty input produces empty map" $
      groupByFile [] `shouldBe` Map.empty

  describe "binarySearchFit" $ do
    it "result fits within the budget" $
      property $
        forAll (listOf1 genRenderTag) $ \tags ->
          forAll (chooseInt (10, 4096)) $ \budget ->
            let selected = binarySearchFit tags budget 0.15
                toks = estimateTokens (formatSelected selected)
             in counterexample ("tokens=" <> show toks <> " budget=" <> show budget) $
                  toks <= budget

    it "result is a prefix of the input" $
      property $
        forAll (listOf genRenderTag) $ \tags ->
          forAll (chooseInt (10, 4096)) $ \budget ->
            let selected = binarySearchFit tags budget 0.15
             in isPrefixOf selected tags

    it "returns all tags when budget is very large" $
      property $
        forAll (resize 5 (listOf genRenderTag)) $ \tags ->
          let toks = estimateTokens (formatSelected tags)
           in not (null tags) ==>
                binarySearchFit tags (toks * 3) 0.0 === tags

    it "empty input produces empty output" $
      binarySearchFit [] 1000 0.15 `shouldBe` []

  describe "rankDefs" $ do
    it "is a permutation of the input" $
      property $
        forAll (listOf genRenderTag) $ \tags ->
          forAll (genScores tags) $ \scores ->
            sort (rankDefs tags scores) === sort tags

    it "preserves length" $
      property $
        forAll (listOf genRenderTag) $ \tags ->
          forAll (genScores tags) $ \scores ->
            length (rankDefs tags scores) === length tags
