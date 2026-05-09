{-# LANGUAGE OverloadedStrings #-}

module CodeStar.RepoMap.Graph.Extract.TypesSpec (spec) where

import Data.Text qualified as Text
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.RepoMap.Graph (isIdentChar, wordAt)

genLines :: Gen (V.Vector Text.Text, Int, Int)
genLines = do
  ls <- listOf (Text.pack <$> listOf (elements (['a' .. 'z'] ++ "_' ")))
  row <- chooseInt (0, length ls + 1)
  col <- chooseInt (0, 50)
  pure (V.fromList ls, row, col)

shrinkSymName :: Text.Text -> [Text.Text]
shrinkSymName t =
  [ Text.pack s
  | s <- shrink (Text.unpack t)
  , not (null s)
  , all (\c -> c >= 'a' && c <= 'z') s
  ]

shrinkLinesTriple :: (V.Vector Text.Text, Int, Int) -> [(V.Vector Text.Text, Int, Int)]
shrinkLinesTriple (ls, row, col) =
  [ (V.fromList ls', row, col)
  | ls' <- shrinkList shrinkSymName (V.toList ls)
  ]
    ++ [ (ls, row', col)
       | row' <- shrink row
       , row' >= 0
       ]
    ++ [ (ls, row, col')
       | col' <- shrink col
       , col' >= 0
       ]

spec :: Spec
spec =
  describe "wordAt" $ do
    prop "never crashes on any inputs" $
      forAllShrink genLines shrinkLinesTriple $ \(ls, row, col) ->
        checkCoverage $
          cover 30 (row < V.length ls) "in-bounds row" $
          cover 15 (row >= V.length ls) "out-of-bounds row" $
          wordAt ls row col `seq` True

    prop "returns empty for out-of-bounds row" $
      forAllShrink genLines shrinkLinesTriple $ \(ls, _, col) ->
        wordAt ls (V.length ls) col === Text.empty

    prop "result contains only identifier chars" $
      forAllShrink genLines shrinkLinesTriple $ \(ls, row, col) ->
        checkCoverage $
          cover 30 (not (Text.null (wordAt ls row col))) "non-empty result" $
          cover 30 (Text.null (wordAt ls row col)) "empty result" $
          let result = wordAt ls row col
           in Text.all isIdentChar result
