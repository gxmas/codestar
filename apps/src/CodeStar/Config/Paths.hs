module CodeStar.Config.Paths
  ( globalConfigDir
  , globalDataDir
  , globalCacheDir
  , grammarsDir
  , projectDir
  ) where

import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>))

globalConfigDir :: IO FilePath
globalConfigDir = getXdgDirectory XdgConfig "codestar"

globalDataDir :: IO FilePath
globalDataDir = getXdgDirectory XdgData "codestar"

globalCacheDir :: IO FilePath
globalCacheDir = getXdgDirectory XdgCache "codestar"

grammarsDir :: IO FilePath
grammarsDir = (</> "grammars") <$> globalDataDir

projectDir :: FilePath -> FilePath
projectDir workspace = workspace </> ".codestar"
