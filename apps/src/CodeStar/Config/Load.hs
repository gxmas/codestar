module CodeStar.Config.Load
  ( loadConfig
  , globalSettingsDir
  , projectSettingsDir
  ) where

import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import CodeStar.Config.Env (loadFromEnv)
import CodeStar.Config.Paths qualified as Paths
import CodeStar.Config.Json (parseJsonConfig)
import CodeStar.Config.Toml (parseTomlConfig)
import CodeStar.Config.Types
import CodeStar.Config.Validate (resolve)
import CodeStar.Types (ModelRole (..))

loadConfig :: RunArgs -> IO (Either (NonEmpty ConfigError) Config)
loadConfig args = do
  globalResult <- loadSettingsFile globalSettingsDir
  case globalResult of
    Left err -> pure (Left err)
    Right globalPartial -> do
      let workspace = maybe "." id args.cliWorkspace
      projectResult <- loadSettingsFile (pure (projectSettingsDir workspace))
      case projectResult of
        Left err -> pure (Left err)
        Right projectPartial -> do
          envPartial <- loadFromEnv
          let cliPartial = cliToPartial args
              merged = globalPartial
                    <> projectPartial
                    <> envPartial
                    <> cliPartial
              withWorkspace = case args.cliWorkspace of
                Just w  -> merged <> workspacePartial w
                Nothing -> merged
          pure (resolve withWorkspace)

-- | Load a settings file, trying TOML first then JSON fallback.
loadSettingsFile :: IO FilePath -> IO (Either (NonEmpty ConfigError) PartialConfig)
loadSettingsFile getDir = do
  dir <- getDir
  let tomlPath = dir </> "settings.toml"
      jsonPath = dir </> "settings.json"
  tomlExists <- doesFileExist tomlPath
  if tomlExists
    then do
      contents <- BS.readFile tomlPath
      case parseTomlConfig (TE.decodeUtf8 contents) of
        Right pc -> pure (Right pc)
        Left err -> pure (Left (pure (ConfigFileError tomlPath err)))
    else do
      jsonExists <- doesFileExist jsonPath
      if jsonExists
        then do
          contents <- BS.readFile jsonPath
          case parseJsonConfig contents of
            Right pc -> pure (Right pc)
            Left err -> pure (Left (pure (ConfigFileError jsonPath err)))
        else
          pure (Right mempty)

-- | Convert CLI args into a partial config layer.
cliToPartial :: RunArgs -> PartialConfig
cliToPartial args =
  PartialConfig
    (Last Nothing)
    (cliModelRoles args.cliModel)
    (Last Nothing)
    (Last Nothing)
    mempty
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    (PartialServerSection (Last args.cliPort) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing))
    (PartialTelemetrySection (Last (Just args.cliTelemetry)) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing))
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

workspacePartial :: FilePath -> PartialConfig
workspacePartial workspace =
  PartialConfig
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    mempty
    (Last (Just workspace))
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    (Last Nothing)
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty
    mempty

cliModelRoles :: Maybe Text -> Last (Map ModelRole ModelSpec)
cliModelRoles Nothing  = Last Nothing
cliModelRoles (Just m) =
  Last $ Just $ Map.fromList
    [(Coder, ModelSpec m Nothing Nothing (Just 8192))]

globalSettingsDir :: IO FilePath
globalSettingsDir = Paths.globalConfigDir

projectSettingsDir :: FilePath -> FilePath
projectSettingsDir = Paths.projectDir
