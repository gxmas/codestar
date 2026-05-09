module CodeStar.Config.MigrateSpec (spec) where

import Control.Exception (IOException, catch)
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.Config.Migrate (migrateJsonToToml)

spec :: Spec
spec = describe "CodeStar.Config.Migrate" $ do
  it "creates settings.toml from project settings.json when missing" $
    withWorkspace $ \ws -> do
      let projectDir = ws </> ".codestar"
          jsonPath = projectDir </> "settings.json"
          tomlPath = projectDir </> "settings.toml"
      createDirectoryIfMissing True projectDir
      BS8.writeFile jsonPath "{\"provider\":\"openai\"}"
      migrateJsonToToml (Just ws)
      doesFileExist tomlPath `shouldReturn` True
      content <- Text.IO.readFile tomlPath
      content `shouldSatisfy` Text.isInfixOf "[server]"

  it "does not overwrite existing settings.toml" $
    withWorkspace $ \ws -> do
      let projectDir = ws </> ".codestar"
          jsonPath = projectDir </> "settings.json"
          tomlPath = projectDir </> "settings.toml"
          existing = "custom = true\n"
      createDirectoryIfMissing True projectDir
      BS8.writeFile jsonPath "{\"provider\":\"openai\"}"
      Text.IO.writeFile tomlPath existing
      migrateJsonToToml (Just ws)
      Text.IO.readFile tomlPath `shouldReturn` existing

withWorkspace :: (FilePath -> IO a) -> IO a
withWorkspace action = do
  tmp <- getTemporaryDirectory
  let ws = tmp </> "codestar-migrate-spec"
  ignoreIO (removePathForcibly ws)
  createDirectoryIfMissing True ws
  out <- action ws
  ignoreIO (removePathForcibly ws)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
