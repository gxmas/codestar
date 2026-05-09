module CodeStar.Tools.Grep
  ( grepToolHandler
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.JsonSchema
  ( booleanSchema
  , integerSchema
  , objectSchema
  , optional
  , required
  , stringSchema
  , withDescription
  , withMinimum
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import System.Process.Typed hiding (readProcessStdout)

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

maxOutputLines :: Int
maxOutputLines = 500

grepToolHandler :: ToolHandlerDict
grepToolHandler =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "grep"
          , description =
              "Search file contents using ripgrep. "
                <> "Three output modes: files_with_matches (default), content, count. "
                <> "Supports context lines and glob filtering."
          , parameters =
              objectSchema
                [ required "pattern" (stringSchema & withDescription "Regex pattern to search for")
                , optional "path" (stringSchema & withDescription "File or directory to search (default: .)")
                , optional "glob" (stringSchema & withDescription "Glob filter, e.g. \"*.py\"")
                , optional "output_mode" (stringSchema & withDescription "files_with_matches | content | count (default: files_with_matches)")
                , optional "context" (integerSchema & withDescription "Lines of context around each match" & withMinimum 0)
                , optional "case_insensitive" (booleanSchema & withDescription "Case-insensitive search (default: false)")
                ]
          , riskTier = ReadOnly
          }
    , invoke = invokeGrep
    }

invokeGrep :: ToolInput -> IO (Either ToolError ToolOutput)
invokeGrep input =
  case parseGrepInput input of
    Left err -> pure (Left err)
    Right args -> runRipgrep args

data GrepArgs = GrepArgs
  { pattern :: Text
  , path :: Text
  , glob :: Maybe Text
  , outputMode :: Text
  , context :: Maybe Int
  , caseInsensitive :: Bool
  }

parseGrepInput :: ToolInput -> Either ToolError GrepArgs
parseGrepInput input = do
  pat <- extractText "pattern" input
  path <- optionalTextWithDefault "path" "." input
  mGlob <- optionalText "glob" input
  mode <- optionalTextWithDefault "output_mode" "files_with_matches" input
  ctx <- extractInt "context" input
  ci <- extractBool "case_insensitive" input
  pure
    GrepArgs
      { pattern = pat
      , path = path
      , glob = mGlob
      , outputMode = mode
      , context = ctx
      , caseInsensitive = ci
      }

optionalText :: Text -> ToolInput -> Either ToolError (Maybe Text)
optionalText key input =
  case Map.lookup key input.arguments of
    Nothing -> Right Nothing
    Just _ -> Just <$> extractText key input

optionalTextWithDefault :: Text -> Text -> ToolInput -> Either ToolError Text
optionalTextWithDefault key def input =
  maybe def id <$> optionalText key input

runRipgrep :: GrepArgs -> IO (Either ToolError ToolOutput)
runRipgrep args = do
  let rgArgs = buildRgArgs args
  result <- runProcess' (proc "rg" rgArgs)
  let (exitCode, out) = result
  case exitCode of
    ExitSuccess -> formatOutput args.outputMode out
    ExitFailure 1 -> pure $ Right ToolOutput{content = "(no matches)", truncated = False}
    ExitFailure code -> pure $ Left (ExecutionFailed ("rg exited with code " <> Text.pack (show code)))

runProcess' :: ProcessConfig stdin stdout stderr -> IO (ExitCode, Text)
runProcess' pc = do
  (exitCode, out, _) <- readProcess (setStdout byteStringOutput (setStderr byteStringOutput pc))
  pure (exitCode, TE.decodeUtf8Lenient (LBS.toStrict out))

buildRgArgs :: GrepArgs -> [String]
buildRgArgs args =
  [Text.unpack args.pattern]
    <> modeFlags args.outputMode
    <> ["-i" | args.caseInsensitive]
    <> maybe [] (\g -> ["-g", Text.unpack g]) args.glob
    <> maybe [] (\c -> ["-C", show c]) args.context
    <> ["--", Text.unpack args.path]

modeFlags :: Text -> [String]
modeFlags "files_with_matches" = ["-l"]
modeFlags "count" = ["-c"]
modeFlags _ = []

formatOutput :: Text -> Text -> IO (Either ToolError ToolOutput)
formatOutput _ out = do
  let ls = Text.lines out
      total = length ls
      capped = take maxOutputLines ls
      truncated = total > maxOutputLines
      body =
        Text.unlines capped
          <> if truncated
            then
              "\n[Output truncated: showing "
                <> Text.pack (show maxOutputLines)
                <> " of "
                <> Text.pack (show total)
                <> " lines]"
            else ""
  pure $ Right ToolOutput{content = body, truncated}
