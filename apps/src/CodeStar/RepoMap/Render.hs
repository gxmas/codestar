module CodeStar.RepoMap.Render
  ( -- * Rendering
    renderRepoMap
  , RenderConfig (..)
  , defaultRenderConfig

    -- * Token estimation
  , estimateTokens

    -- * Internal (Testing)
  , groupByFile
  , formatSelected
  , binarySearchFit
  , rankDefs
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.RepoMap.Graph (SymbolGraph (..), Tag (..), TagKind (..))

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data RenderConfig = RenderConfig
  { maxTokens :: !Int
  -- ^ hard token budget
  , tolerance :: !Double
  -- ^ how close to fill the budget (0.15 = within 15%)
  }
  deriving stock (Eq, Show)

defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { maxTokens = 1024
    , tolerance = 0.15
    }

-- --------------------------------------------------------------------
-- Rendering
-- --------------------------------------------------------------------

{- | Render a repo map: select the top-ranked symbols that fit within
the token budget using binary search, then format as file+signature lines.
-}
renderRepoMap ::
  -- | all extracted tags (definitions only)
  [Tag] ->
  -- | PageRank scores per file
  Map FilePath Double ->
  SymbolGraph ->
  RenderConfig ->
  Text
renderRepoMap tags scores _graph cfg =
  let defs = [t | t <- tags, t.tagKind == Definition]
      -- Some grammars/languages currently extract references but no definitions.
      -- Fallback to all tags to avoid an always-empty repo map in those cases.
      baseTags = if null defs then tags else defs
      ranked = rankDefs baseTags scores
      selected = binarySearchFit ranked cfg.maxTokens cfg.tolerance
   in formatSelected selected

-- | Sort definitions by descending file PageRank score, then by name.
rankDefs :: [Tag] -> Map FilePath Double -> [Tag]
rankDefs defs scores =
  sortBy (comparing (\t -> Down (Map.findWithDefault 0.0 t.tagFile scores, t.tagName))) defs

{- | Binary search: find the largest prefix of ranked defs that fits
within maxTokens. Returns the selected tags.
-}
binarySearchFit :: [Tag] -> Int -> Double -> [Tag]
binarySearchFit ranked budget tol =
  let n = length ranked
      target = fromIntegral budget * (1.0 - tol)
   in go 0 n ranked target
 where
  go lo hi tags target
    | lo >= hi = take lo tags
    | otherwise =
        let mid = (lo + hi + 1) `div` 2
            slice = take mid tags
            toks = estimateTokens (formatSelected slice)
         in if fromIntegral toks <= target
              then go mid hi tags target
              else go lo (mid - 1) tags target

{- | Format selected tags as a condensed repo map.
Groups by file, renders as:
  path/to/file.py:
      functionName
      ClassName
-}
formatSelected :: [Tag] -> Text
formatSelected tags =
  let byFile = groupByFile tags
      files = Map.toAscList byFile
   in Text.intercalate "\n" (map renderFile files)
 where
  renderFile (path, names) =
    let header = Text.pack path <> ":"
        entries = map (\n -> "    " <> n) (Set.toAscList names)
     in Text.unlines (header : entries)

groupByFile :: [Tag] -> Map FilePath (Set Text)
groupByFile =
  foldr
    ( \t m ->
        Map.insertWith Set.union t.tagFile (Set.singleton t.tagName) m
    )
    Map.empty

-- --------------------------------------------------------------------
-- Token estimation
-- --------------------------------------------------------------------

-- | Rough token count: 1 token ≈ 4 characters (GPT/Claude heuristic).
estimateTokens :: Text -> Int
estimateTokens t = max 1 (Text.length t `div` 4)
