
module CodeStar.RepoMap.Graph
  ( -- * Tags
    Tag (..)
  , TagKind (..)
  , ExtractionSkip (..)
  , TagExtraction (..)

    -- * Symbol graph
  , SymbolGraph (..)
  , buildSymbolGraph

    -- * Tag extraction
  , extractTags
  , extractTagsDetailed
  , querySourceModeLabel

    -- * PageRank
  , PageRankWeights (..)
  , defaultWeights
  , pageRank

    -- * Internal (Testing)
  , wordAt
  , isIdentChar
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import CodeStar.RepoMap.Graph.Extract (extractTags, extractTagsDetailed, querySourceModeLabel)
import CodeStar.RepoMap.Graph.Extract.Types
  ( ExtractionSkip (..)
  , Tag (..)
  , TagExtraction (..)
  , TagKind (..)
  , isIdentChar
  , wordAt
  )

-- --------------------------------------------------------------------
-- Symbol Graph
-- --------------------------------------------------------------------

{- | Dependency graph between files inferred from symbol cross-references.
Edges point from the referencing file to the defining file, so
files that are widely depended upon receive high PageRank.
-}
data SymbolGraph = SymbolGraph
  { sgFiles :: !(Set FilePath)
  , sgEdges :: !(Map FilePath (Set FilePath))
  , sgSymbols :: !(Map Text (Set FilePath))
  }
  deriving stock (Show)

buildSymbolGraph :: [Tag] -> SymbolGraph
buildSymbolGraph tags =
  let defs = [t | t <- tags, t.tagKind == Definition]
      refs = [t | t <- tags, t.tagKind == Reference]
      defMap :: Map Text (Set FilePath)
      defMap =
        foldl'
          ( \m t ->
              Map.insertWith Set.union t.tagName (Set.singleton t.tagFile) m
          )
          Map.empty
          defs
      files = Set.fromList (map (.tagFile) tags)
      edges = foldl' (addEdge defMap) Map.empty refs
   in SymbolGraph{sgFiles = files, sgEdges = edges, sgSymbols = defMap}

addEdge ::
  Map Text (Set FilePath) ->
  Map FilePath (Set FilePath) ->
  Tag ->
  Map FilePath (Set FilePath)
addEdge defMap edges ref =
  case Map.lookup ref.tagName defMap of
    Nothing -> edges
    Just defFiles ->
      let targets = Set.delete ref.tagFile defFiles
       in if Set.null targets
            then edges
            else Map.insertWith Set.union ref.tagFile targets edges

-- --------------------------------------------------------------------
-- PageRank
-- --------------------------------------------------------------------

data PageRankWeights = PageRankWeights
  { chatWeight :: !Double -- boost for open chat files (50×)
  , mentionedWeight :: !Double -- boost for files defining mentioned symbols (10×)
  , privateWeight :: !Double -- penalty multiplier for private-only files (0.1×)
  }
  deriving stock (Eq, Show)

defaultWeights :: PageRankWeights
defaultWeights =
  PageRankWeights
    { chatWeight = 50.0
    , mentionedWeight = 10.0
    , privateWeight = 0.1
    }

{- | Compute personalised PageRank over the symbol graph.
Higher scores indicate files more central to the current task.
-}
pageRank ::
  SymbolGraph ->
  -- | currently open / chat files
  [FilePath] ->
  -- | identifiers mentioned in the conversation
  [Text] ->
  PageRankWeights ->
  Map FilePath Double
pageRank graph chatFiles mentionedIdents w =
  let files = Set.toList graph.sgFiles
      n = length files
   in if n == 0
        then Map.empty
        else
          let fileIdx = Map.fromList (zip files [0 ..])
              idxFile = Map.fromList (zip [0 ..] files)
              personal = personalVector graph chatFiles mentionedIdents w files
              adj = buildAdjMatrix graph fileIdx
              -- Damping factor d: probability of following a link rather than
              -- jumping to a random node. 0.85 is the value from the original
              -- Brin/Page paper and remains the standard choice.
              dampingFactor  = 0.85 :: Double
              -- 100 iterations converges well within floating-point precision
              -- for repositories of any realistic size.
              iterationCount = 100  :: Int
              initPR  = replicate n (1.0 / fromIntegral n)
              finalPR = iterate (stepPageRank n adj personal dampingFactor) initPR !! iterationCount
           in Map.fromList [(idxFile Map.! i, finalPR !! i) | i <- [0 .. n - 1]]

personalVector ::
  SymbolGraph ->
  [FilePath] ->
  [Text] ->
  PageRankWeights ->
  [FilePath] ->
  [Double]
personalVector graph chatFiles mentionedIdents w files =
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
              let base = 1.0
                  chat = if Set.member f chatSet then w.chatWeight else 0.0
                  ment = if Set.member f mentionedFiles then w.mentionedWeight else 0.0
               in base + chat + ment
          )
          files
      total = sum raw
   in if total == 0
        then map (const (1.0 / fromIntegral (length files))) raw
        else map (/ total) raw

{- | Adjacency matrix as sparse Map (to, from) → contribution weight.
adj[i][j] = 1/out_degree(j) when file j references symbols in file i.
-}
buildAdjMatrix :: SymbolGraph -> Map FilePath Int -> Map (Int, Int) Double
buildAdjMatrix graph fileIdx =
  foldl' addEdges Map.empty (Map.toList graph.sgEdges)
 where
  addEdges m (from, tos) =
    case Map.lookup from fileIdx of
      Nothing -> m
      Just fromIdx ->
        let toIdxs = mapMaybe (`Map.lookup` fileIdx) (Set.toList tos)
            weight =
              if null toIdxs
                then 0.0
                else 1.0 / fromIntegral (length toIdxs)
         in foldl' (\m' toIdx -> Map.insert (toIdx, fromIdx) weight m') m toIdxs

stepPageRank :: Int -> Map (Int, Int) Double -> [Double] -> Double -> [Double] -> [Double]
stepPageRank n adj personal d pr =
  [ (1 - d) * personal !! i
      + d
        * sum
          [ (adj Map.! (i, j)) * (pr !! j)
          | j <- [0 .. n - 1]
          , Map.member (i, j) adj
          ]
  | i <- [0 .. n - 1]
  ]
