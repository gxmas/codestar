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
