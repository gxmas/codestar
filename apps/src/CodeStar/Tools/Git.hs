module CodeStar.Tools.Git
  ( gitToolHandler
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
import System.Process.Typed

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

gitToolHandler :: ToolHandlerDict
gitToolHandler =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "git"
          , description =
              "Run git operations: status, diff, commit, branch, log, push. "
                <> "Push requires explicit approval (SideEffect risk tier)."
          , parameters =
              objectSchema
                [ required
                    "operation"
                    ( stringSchema
                        & withDescription
                          "status | diff | commit | branch | log | push"
                    )
                , optional "message" (stringSchema & withDescription "Commit message (for commit)")
                , optional "branch" (stringSchema & withDescription "Branch name (for branch/push)")
                , optional "remote" (stringSchema & withDescription "Remote name (for push, default: origin)")
                , optional "staged" (booleanSchema & withDescription "Show only staged changes in diff (default: false)")
                , optional "count" (integerSchema & withDescription "Number of log entries (default: 10)" & withMinimum 1)
                ]
          , riskTier = LocalWrite
          }
    , invoke = invokeGit
    }

invokeGit :: ToolInput -> IO (Either ToolError ToolOutput)
invokeGit input =
  case extractText "operation" input of
    Left err -> pure (Left err)
    Right op -> dispatchOp op input

dispatchOp :: Text -> ToolInput -> IO (Either ToolError ToolOutput)
dispatchOp op input = case op of
  "status" -> runGit ["status", "--short"]
  "diff" -> case optionalBoolWithDefault "staged" False input of
    Left err -> pure (Left err)
    Right staged -> runGit $ ["diff"] <> ["--staged" | staged]
  "commit" -> case extractText "message" input of
    Left _ -> pure (Left (InvalidInput "commit requires message"))
    Right m -> runGit ["commit", "-m", Text.unpack m]
  "branch" -> case extractText "branch" input of
    Left err -> case missingField "branch" err of
      True -> runGit ["branch"]
      False -> pure (Left err)
    Right b -> runGit ["checkout", "-b", Text.unpack b]
  "log" -> case optionalIntWithDefault "count" 10 input of
    Left err -> pure (Left err)
    Right n ->
      if n < 1
        then pure (Left (InvalidInput "count: must be >= 1"))
        else runGit ["log", "--oneline", "-" <> show n]
  "push" -> case (optionalTextWithDefault "remote" "origin" input, optionalTextWithDefault "branch" "" input) of
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)
    (Right remote, Right branch) -> do
      let args = ["push", Text.unpack remote] <> [Text.unpack branch | not (Text.null branch)]
      runGit args
  other -> pure (Left (InvalidInput ("Unknown git operation: " <> other)))

missingField :: Text -> ToolError -> Bool
missingField key (InvalidInput msg) = msg == key <> ": missing"
missingField _ _ = False

optionalTextWithDefault :: Text -> Text -> ToolInput -> Either ToolError Text
optionalTextWithDefault key def input =
  case Map.lookup key input.arguments of
    Nothing -> Right def
    Just _ -> extractText key input

optionalBoolWithDefault :: Text -> Bool -> ToolInput -> Either ToolError Bool
optionalBoolWithDefault key def input =
  case Map.lookup key input.arguments of
    Nothing -> Right def
    Just _ -> extractBool key input

optionalIntWithDefault :: Text -> Int -> ToolInput -> Either ToolError Int
optionalIntWithDefault key def input =
  case Map.lookup key input.arguments of
    Nothing -> Right def
    Just _ -> do
      m <- extractInt key input
      pure (maybe def id m)

runGit :: [String] -> IO (Either ToolError ToolOutput)
runGit args = do
  (exitCode, out, err) <-
    readProcess $
      proc "git" args
  let output = TE.decodeUtf8Lenient (LBS.toStrict out)
      errOut = TE.decodeUtf8Lenient (LBS.toStrict err)
      combined = if Text.null errOut then output else output <> "\n" <> errOut
  pure $ case exitCode of
    ExitSuccess -> Right ToolOutput{content = combined, truncated = False}
    ExitFailure _ -> Left (ExecutionFailed combined)
