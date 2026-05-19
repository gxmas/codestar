{- |
= Tools.Tests — test runner tool

Exposes the @run_tests@ tool that executes the project's test suite and
returns a structured summary (pass\/fail counts, failing test names).

The agent uses this as part of its verification loop: after making code
changes it runs the tests to confirm nothing regressed, and uses the
failure output to guide further edits.

This is a 'SideEffect' risk-tier tool because test runners can have
arbitrary side effects (network calls, file system changes, processes).
-}
module CodeStar.Tools.Tests
  ( testsToolHandler
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.JsonSchema (objectSchema, optional, stringSchema, withDescription)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Process.Typed

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

testsToolHandler :: ToolHandlerDict
testsToolHandler =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "run_tests"
          , description =
              "Detect the project language from markers and run the test suite. "
                <> "Supports: Haskell (cabal), Python (pytest), JavaScript (npm), "
                <> "Go (go test), Rust (cargo), Java (mvn), Ruby (rspec)."
          , parameters =
              objectSchema
                [ optional "workspace" (stringSchema & withDescription "Project root directory (default: .)")
                , optional "subset" (stringSchema & withDescription "Test subset or pattern to run")
                ]
          , riskTier = LocalWrite
          }
    , invoke = invokeTests
    }

invokeTests :: ToolInput -> IO (Either ToolError ToolOutput)
invokeTests input =
  case parseTestsInput input of
    Left err -> pure (Left err)
    Right (workspace, mSubset) -> do
      lang <- detectLanguage workspace
      case lang of
        Nothing ->
          pure $
            Left
              ( ExecutionFailed
                  "Could not detect project language from markers"
              )
        Just (language, cmd) -> do
          let fullCmd = cmd <> maybe [] (\s -> [s]) mSubset
          runTestCmd workspace language fullCmd

parseTestsInput :: ToolInput -> Either ToolError (FilePath, Maybe String)
parseTestsInput input = do
  workspace <- optionalTextWithDefault "workspace" "." input
  subset <- optionalText "subset" input
  pure (Text.unpack workspace, fmap Text.unpack subset)

optionalText :: Text -> ToolInput -> Either ToolError (Maybe Text)
optionalText key input =
  case Map.lookup key input.arguments of
    Nothing -> Right Nothing
    Just _ -> Just <$> extractText key input

optionalTextWithDefault :: Text -> Text -> ToolInput -> Either ToolError Text
optionalTextWithDefault key def input =
  maybe def id <$> optionalText key input

detectLanguage :: FilePath -> IO (Maybe (Text, [String]))
detectLanguage root = tryEach markers
 where
  markers =
    [ ("*.cabal", ["cabal", "test", "--test-show-details=streaming"])
    , ("package.json", ["npm", "test"])
    , ("go.mod", ["go", "test", "./..."])
    , ("Cargo.toml", ["cargo", "test"])
    , ("pom.xml", ["mvn", "test", "-q"])
    , ("pyproject.toml", ["python", "-m", "pytest", "-v"])
    , ("setup.py", ["python", "-m", "pytest", "-v"])
    , ("Gemfile", ["bundle", "exec", "rspec"])
    ]

  tryEach [] = pure Nothing
  tryEach ((marker, cmd) : rest) = do
    found <- matchesMarker root marker
    if found
      then pure (Just (Text.pack (headCmd cmd), cmd))
      else tryEach rest

matchesMarker :: FilePath -> String -> IO Bool
matchesMarker root "*.cabal" = do
  entries <- listDir root
  pure (any (\e -> takeExtension' e == ".cabal") entries)
 where
  takeExtension' p =
    let (_, ext) = break (== '.') (reverse p)
     in '.' : reverse ext
matchesMarker root marker = doesFileExist (root </> marker)

listDir :: FilePath -> IO [FilePath]
listDir path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      entries <- getDirectoryContents path
      pure (filter (\e -> e /= "." && e /= "..") entries)
    else pure []

headCmd :: [String] -> String
headCmd (x : _) = x
headCmd [] = error "empty command"

runTestCmd :: FilePath -> Text -> [String] -> IO (Either ToolError ToolOutput)
runTestCmd workspace lang cmd = do
  let pc = setWorkingDir workspace $ proc (headCmd cmd) (drop 1 cmd)
  (exitCode, out, err) <- readProcess pc
  let stdout' = TE.decodeUtf8Lenient (LBS.toStrict out)
      stderr' = TE.decodeUtf8Lenient (LBS.toStrict err)
      combined = stdout' <> if Text.null stderr' then "" else "\n" <> stderr'
      header = "[" <> lang <> "] " <> Text.intercalate " " (map Text.pack cmd) <> "\n"
  pure $ case exitCode of
    ExitSuccess -> Right ToolOutput{content = header <> combined, truncated = False}
    ExitFailure n ->
      Left
        ( ExecutionFailed
            (header <> "Exit code " <> Text.pack (show n) <> "\n" <> combined)
        )
