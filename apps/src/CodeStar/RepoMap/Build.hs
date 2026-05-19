{- |
= CodeStar.RepoMap.Build — one-shot repo-map construction

This module provides the __synchronous__ path to building a repo map: it
scans the workspace, extracts tags from every file (using the cache where
possible), runs PageRank, and renders the result in a single call.

It is used by the __server__ path ('Server.SessionSetup') where each new
agent session needs a repo map immediately at startup, before the
background 'Worker' has had time to warm up.  The CLI's interactive mode
uses the incremental 'Worker' instead.

The 5-second timeout prevents a slow or very large workspace from
blocking session initialisation.
-}
module CodeStar.RepoMap.Build
  ( buildRepoMapSafe
  , listWorkspaceFiles
  ) where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)

import CodeStar.RepoMap.Cache (RepoMapCache, getOrComputeTags)
import CodeStar.RepoMap.Graph (buildSymbolGraph, defaultWeights, extractTags, pageRank)
import CodeStar.RepoMap.Render (defaultRenderConfig, renderRepoMap)
import CodeStar.RepoMap.Render qualified as RepoMap
import CodeStar.TreeSitter (GrammarRegistry)

-- | Build a repo map for the given workspace, returning 'Text.empty' on
-- timeout.  Tags are fetched from the cache where available; missing
-- entries are computed on the fly with 'extractTags' and stored for
-- future calls.
buildRepoMapSafe :: GrammarRegistry -> RepoMapCache -> FilePath -> IO Text
buildRepoMapSafe grammarReg repoCache workDir = do
  wsFiles <- listWorkspaceFiles workDir
  result <- timeout 5000000 $ do
    allTags <- fmap concat $ forM wsFiles $ \f -> do
      src <- BS.readFile f
      getOrComputeTags repoCache f (extractTags grammarReg f src)
    let graph = buildSymbolGraph allTags
        scores = pageRank graph [] [] defaultWeights
    pure $!
      renderRepoMap
        allTags
        scores
        graph
        defaultRenderConfig{RepoMap.maxTokens = 4096}
  case result of
    Just repoMap -> pure repoMap
    Nothing -> pure Text.empty

-- | Recursively enumerate all files under @root@, skipping hidden entries
-- (names starting with @.@).  Does not skip build artefact directories
-- like @dist-newstyle@; callers that care should filter the result.
listWorkspaceFiles :: FilePath -> IO [FilePath]
listWorkspaceFiles root = do
  entries <- listDirectory root
  let visible = filter (not . ("." `isPrefixOf`)) entries
  fmap concat $ forM visible $ \name -> do
    let path = root </> name
    isFile <- doesFileExist path
    if isFile
      then pure [path]
      else listWorkspaceFiles path
