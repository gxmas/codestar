{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.RepoMap.GraphSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.QuickCheck

import CodeStar.RepoMap.Graph
  ( PageRankWeights (..)
  , SymbolGraph (..)
  , Tag (..)
  , TagKind (..)
  , buildSymbolGraph
  , defaultWeights
  , pageRank
  )

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genFile :: Gen FilePath
genFile = elements ["a.hs", "b.hs", "c.hs", "d.hs"]

genSymName :: Gen Text
genSymName = Text.pack <$> listOf1 (elements ['a' .. 'e'])

genTagKind :: Gen TagKind
genTagKind = elements [Definition, Reference]

genTag :: Gen Tag
genTag =
  Tag
    <$> genFile
    <*> genSymName
    <*> chooseInt (0, 100)
    <*> genTagKind

instance Arbitrary Tag where
  arbitrary = genTag
  shrink t =
    [t{tagFile = f} | f <- shrinkFile t.tagFile]
      ++ [t{tagName = n} | n <- shrinkSymName t.tagName]
      ++ [t{tagLine = ln} | ln <- shrink t.tagLine, ln >= 0]
      ++ [t{tagKind = k} | k <- shrinkTagKind t.tagKind]

genDefTag :: Gen Tag
genDefTag =
  Tag
    <$> genFile
    <*> genSymName
    <*> chooseInt (0, 100)
    <*> pure Definition

genTags :: Gen [Tag]
genTags =
  resize 20 $
    frequency
      [ (2, listOf genTag)
      , (3, genTagsWithCrossFileReference)
      ]

genTagsWithCrossFileReference :: Gen [Tag]
genTagsWithCrossFileReference = do
  sym <- genSymName
  defFile <- genFile
  refFile <- genFile `suchThat` (/= defFile)
  defLine <- chooseInt (0, 100)
  refLine <- chooseInt (0, 100)
  extras <- listOf genTag
  pure (Tag defFile sym defLine Definition : Tag refFile sym refLine Reference : extras)

-- --------------------------------------------------------------------
-- buildSymbolGraph
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "buildSymbolGraph" $ do
    it "generator produces enough cross-file references" $
      property $
        forAllShrink genTags shrink $ \tags ->
          checkCoverage $
            cover 20 (hasCrossFileReference tags) "cross-file references present" $
              property True

    it "sgFiles contains all files from tags" $
      property $
        forAllShrink genTags shrink $ \tags ->
          let g = buildSymbolGraph tags
           in all (\t -> Set.member t.tagFile g.sgFiles) tags

    it "sgEdges source files are within sgFiles" $
      property $
        forAllShrink genTags shrink $ \tags ->
          let g = buildSymbolGraph tags
           in all (`Set.member` g.sgFiles) (Map.keys g.sgEdges)

    it "sgEdges target files are within sgFiles" $
      property $
        forAllShrink genTags shrink $ \tags ->
          let g = buildSymbolGraph tags
           in all (all (`Set.member` g.sgFiles) . Set.toList) (Map.elems g.sgEdges)

    it "sgEdges has no self-loops" $
      property $
        forAllShrink genTags shrink $ \tags ->
          let g = buildSymbolGraph tags
           in all (\(f, ts) -> not (Set.member f ts)) (Map.toList g.sgEdges)

    it "sgSymbols matches naive definition map" $
      property $
        forAllShrink genTags shrink $ \tags ->
          let g = buildSymbolGraph tags
              model =
                foldl'
                  ( \m t ->
                      if t.tagKind == Definition
                        then Map.insertWith Set.union t.tagName (Set.singleton t.tagFile) m
                        else m
                  )
                  Map.empty
                  tags
           in g.sgSymbols === model

    it "empty tags produce empty graph" $
      let g = buildSymbolGraph []
       in do
            Set.null g.sgFiles `shouldBe` True
            Map.null g.sgEdges `shouldBe` True
            Map.null g.sgSymbols `shouldBe` True

    it "reference-only tags produce no sgSymbols entries" $
      property $
        forAllShrink (listOf (genTag `suchThat` (\t -> t.tagKind == Reference))) shrink $ \tags ->
          let g = buildSymbolGraph tags
           in Map.null g.sgSymbols

  describe "pageRank" $ do
    -- ----------------------------------------------------------------
    -- Oracle properties: compare production pageRank against 100
    -- iterations of the pageRankStep reference implementation.
    --
    -- pageRank uses a matrix/vector representation internally;
    -- pageRankStep uses Map FilePath Double.  They are mathematically
    -- identical, so the results should agree within floating-point
    -- epsilon (1e-9) on any graph.
    --
    -- A broken damping factor, transposed adjacency matrix, or wrong
    -- personal-weight normalisation would cause these to diverge.
    -- ----------------------------------------------------------------

    it "matches 100 iterations of pageRankStep oracle (no boost)" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g       = buildSymbolGraph tags
              files   = Set.toList g.sgFiles
              n       = length files
              initPR  = Map.fromList [(f, 1.0 / fromIntegral n) | f <- files]
              refPR   = iterate (pageRankStep g [] [] defaultWeights) initPR !! 100
              prodPR  = pageRank g [] [] defaultWeights
              diffs   = [abs (Map.findWithDefault 0 f prodPR - Map.findWithDefault 0 f refPR) | f <- files]
              maxDiff = maximum diffs
           in counterexample ("max deviation from oracle: " <> show maxDiff) $
                all (<= 1e-9) diffs

    it "matches 100 iterations of pageRankStep oracle (with chat-file boost)" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g        = buildSymbolGraph tags
              files    = Set.toList g.sgFiles
              n        = length files
              chatFile = case files of { (f:_) -> f; [] -> "" }  -- first file in sorted order gets the boost
              initPR   = Map.fromList [(f, 1.0 / fromIntegral n) | f <- files]
              refPR    = iterate (pageRankStep g [chatFile] [] defaultWeights) initPR !! 100
              prodPR   = pageRank g [chatFile] [] defaultWeights
              diffs    = [abs (Map.findWithDefault 0 f prodPR - Map.findWithDefault 0 f refPR) | f <- files]
              maxDiff  = maximum diffs
           in counterexample ("chat=" <> chatFile <> " max deviation: " <> show maxDiff) $
                all (<= 1e-9) diffs

    it "output contains all graph files" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              scores = pageRank g [] [] defaultWeights
           in all (`Map.member` scores) (Set.toList g.sgFiles)

    it "all scores are non-negative" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              scores = pageRank g [] [] defaultWeights
           in all (>= 0.0) (Map.elems scores)

    it "no scores are NaN or Infinity" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              scores = pageRank g [] [] defaultWeights
           in all (\v -> not (isNaN v) && not (isInfinite v)) (Map.elems scores)

    it "sum of all scores is bounded and positive" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              totalScore = sum (Map.elems (pageRank g [] [] defaultWeights))
           in counterexample ("total score=" <> show totalScore) $
                totalScore > 0.0 && totalScore <= 1.0 + 1e-9

    it "one extra iteration changes scores by at most epsilon" $
      property $
        forAllShrink (listOf1 genTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              pr100 = pageRank g [] [] defaultWeights
              pr101 = pageRankStep g [] [] defaultWeights pr100
              epsilon = 5e-3
           in all
                ( \f ->
                    abs (Map.findWithDefault 0 f pr101 - Map.findWithDefault 0 f pr100) <= epsilon
                )
                (Map.keys pr100)

    it "empty graph produces empty map" $
      let g = buildSymbolGraph []
       in pageRank g [] [] defaultWeights `shouldBe` Map.empty

    it "chat files receive higher scores" $
      property $
        forAllShrink (listOf1 genDefTag) shrink $ \tags ->
          let g = buildSymbolGraph tags
              files = Set.toList g.sgFiles
           in length files >= 2 ==>
                case files of
                  (f1 : _) ->
                    let withChat = pageRank g [f1] [] defaultWeights
                        withoutChat = pageRank g [] [] defaultWeights
                     in counterexample ("chat=" <> show withChat <> " no-chat=" <> show withoutChat) $
                          Map.findWithDefault 0 f1 withChat >= Map.findWithDefault 0 f1 withoutChat
                  [] -> property True

    it "disconnected nodes have minimal score versus referenced definitions" $
      let tags =
            [ Tag "a.hs" "foo" 1 Definition
            , Tag "b.hs" "foo" 2 Reference
            , Tag "c.hs" "isolated" 3 Definition
            ]
          g = buildSymbolGraph tags
          scores = pageRank g [] [] defaultWeights
          aScore = Map.findWithDefault 0 "a.hs" scores
          cScore = Map.findWithDefault 0 "c.hs" scores
       in cScore `shouldSatisfy` (<= aScore)

shrinkFile :: FilePath -> [FilePath]
shrinkFile current = filter (/= current) ["a.hs", "b.hs", "c.hs", "d.hs"]

shrinkTagKind :: TagKind -> [TagKind]
shrinkTagKind Definition = [Reference]
shrinkTagKind Reference = []

shrinkSymName :: Text -> [Text]
shrinkSymName t =
  [ Text.pack s
  | s <- shrink (Text.unpack t)
  , not (null s)
  , all (\c -> c >= 'a' && c <= 'e') s
  ]

pageRankStep ::
  SymbolGraph ->
  [FilePath] ->
  [Text] ->
  PageRankWeights ->
  Map.Map FilePath Double ->
  Map.Map FilePath Double
pageRankStep graph chatFiles mentionedIdents w prMap =
  let files = Set.toList graph.sgFiles
      n = length files
      fileAt = Map.fromList (zip [0 ..] files)
      outNeighbors from = Set.toList (Map.findWithDefault Set.empty from graph.sgEdges)
      outgoingWeight from to =
        let outs = outNeighbors from
         in if null outs
              then 0.0
              else if to `elem` outs then 1.0 / fromIntegral (length outs) else 0.0
      personalByFile = Map.fromList (zip files (personalWeights graph chatFiles mentionedIdents w files))
      next i =
        let fileI = fileAt Map.! i
            base = 0.15 * Map.findWithDefault 0 fileI personalByFile
            contrib =
              0.85
                * sum
                  [ outgoingWeight fileJ fileI * Map.findWithDefault 0 fileJ prMap
                  | j <- [0 .. n - 1]
                  , let fileJ = fileAt Map.! j
                  ]
         in base + contrib
   in Map.fromList [(fileAt Map.! i, next i) | i <- [0 .. n - 1]]

personalWeights ::
  SymbolGraph ->
  [FilePath] ->
  [Text] ->
  PageRankWeights ->
  [FilePath] ->
  [Double]
personalWeights graph chatFiles mentionedIdents w files =
  let chatSet = Set.fromList chatFiles
      mentionedFiles =
        Set.fromList
          [ f
          | ident <- mentionedIdents
          , f <- maybe [] Set.toList (Map.lookup ident graph.sgSymbols)
          ]
      raw =
        map
          ( \f ->
              1.0
                + (if Set.member f chatSet then w.chatWeight else 0.0)
                + (if Set.member f mentionedFiles then w.mentionedWeight else 0.0)
          )
          files
      totalWeight = sum raw
   in if totalWeight == 0 then map (const (1.0 / fromIntegral (length files))) raw else map (/ totalWeight) raw

hasCrossFileReference :: [Tag] -> Bool
hasCrossFileReference tags =
  any hasCrossRef refs
 where
  defsByName =
    Map.fromListWith
      Set.union
      [ (t.tagName, Set.singleton t.tagFile)
      | t <- tags
      , t.tagKind == Definition
      ]
  refs = [t | t <- tags, t.tagKind == Reference]
  hasCrossRef ref =
    case Map.lookup ref.tagName defsByName of
      Nothing -> False
      Just files -> any (/= ref.tagFile) (Set.toList files)
