{- |
= CodeStar.RepoMap.Render — format tags into an LLM-friendly repo map

The repo map is the text that the agent injects into the LLM's context
window to give it a structural overview of the codebase.  The format is
intentionally compact:

@
  path\/to\/file.py:
      functionName
      ClassName
  another\/file.hs:
      parseCliArgs
      runAgent
@

== Fitting the token budget

The LLM's context window is finite, so the renderer must select which
files to include.  The algorithm is:

1. __Rank__ all definition tags by their file's PageRank score (most
   important files first), then alphabetically within the same file.
2. __Binary search__ for the largest prefix that fits within the
   @maxTokens@ budget (with a @tolerance@ margin so we fill close to the
   limit rather than leaving large gaps).
3. __Format__ the selected tags into the compact text block.

Token estimation uses the heuristic that one token ≈ 4 characters, which
is accurate enough for GPT/Claude-family models.
-}
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

-- | Configuration for 'renderRepoMap'.
data RenderConfig = RenderConfig
  { maxTokens :: !Int
  -- ^ Hard token budget.  The renderer will not exceed this count.
  , tolerance :: !Double
  -- ^ How close to the budget to aim for.  @0.15@ means the renderer
  --   tries to fill at least 85% of the budget rather than stopping as
  --   soon as it crosses a coarse boundary.
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

-- | Sort definition tags by descending file PageRank score, breaking ties
-- alphabetically by symbol name.  The result determines the order in which
-- files appear in the rendered map.
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

-- | Group tags by file path, collecting symbol names into a 'Set' to
-- deduplicate identical names that appear on multiple lines (e.g. multi-clause
-- function definitions in Haskell).
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
