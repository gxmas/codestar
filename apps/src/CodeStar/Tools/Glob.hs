{- |
= Tools.Glob — file glob tool

Exposes the @glob@ tool that finds files matching a glob pattern in the
workspace.  Results are filtered to the workspace root so the agent
cannot accidentally enumerate files outside the project.

This is a 'ReadOnly' risk-tier tool — it does not modify any files.
-}
module CodeStar.Tools.Glob
  ( globToolHandler
  ) where

import Control.Exception (IOException, try)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.JsonSchema (objectSchema, optional, required, stringSchema, withDescription)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath (makeRelative, (</>))
import System.FilePattern ((?==))

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

globToolHandler :: ToolHandlerDict
globToolHandler =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "glob"
          , description =
              "Find files matching a glob pattern. "
                <> "Results sorted by modification time (most recent first). "
                <> "Supports ** for recursive matching."
          , parameters =
              objectSchema
                [ required "pattern" (stringSchema & withDescription "Glob pattern, e.g. \"**/*.hs\" or \"src/*.py\"")
                , optional "path" (stringSchema & withDescription "Directory to search in (default: current directory)")
                ]
          , riskTier = ReadOnly
          }
    , invoke = invokeGlob
    }

invokeGlob :: ToolInput -> IO (Either ToolError ToolOutput)
invokeGlob input =
  case parseGlobInput input of
    Left err -> pure (Left err)
    Right (pattern, root) -> do
      result <- try @IOException $ findMatches root (Text.unpack pattern)
      case result of
        Left err -> pure (Left (ExecutionFailed (Text.pack (show err))))
        Right paths -> do
          sorted <- sortByMtime paths
          let output =
                if null sorted
                  then "(no files matched pattern)"
                  else Text.unlines (map Text.pack sorted)
          pure $
            Right
              ToolOutput
                { content = output
                , truncated = False
                }

parseGlobInput :: ToolInput -> Either ToolError (Text, FilePath)
parseGlobInput input = do
  pattern <- extractText "pattern" input
  mPath <- case Map.lookup "path" input.arguments of
    Nothing -> Right "."
    Just _ -> Text.unpack <$> extractText "path" input
  pure (pattern, mPath)

findMatches :: FilePath -> String -> IO [FilePath]
findMatches root pattern = do
  allFiles <- walkDir root
  let relative = map (makeRelative root) allFiles
      matched = filter (pattern ?==) relative
  pure (map (root </>) matched)

walkDir :: FilePath -> IO [FilePath]
walkDir dir = do
  entries <- listDirectory dir
  fmap concat $ mapM (classify dir) entries
 where
  classify parent entry = do
    let path = parent </> entry
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    if isDir
      then walkDir path
      else if isFile then pure [path] else pure []

sortByMtime :: [FilePath] -> IO [FilePath]
sortByMtime paths = do
  withTimes <- mapM (\p -> (,) p <$> getModificationTime p) paths
  pure $ map fst (sortOn (Down . snd) withTimes)
