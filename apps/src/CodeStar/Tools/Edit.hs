{- |
= Tools.Edit — exact-string file edit tool

Exposes the @edit_file@ tool that replaces an exact string match in an
existing file.  The agent must have read the file first (enforced by the
'ReadTracker') so it has seen the current content before making a change.

The tool requires that the search string is __unique__ within the file to
prevent accidental multi-site edits.  If the string appears more than once,
the tool returns an error asking the agent to provide more context.

'replaceFirst' is the core replacement function, exposed for testing.
-}
module CodeStar.Tools.Edit
  ( editToolHandler

    -- * Internal (Testing)
  , replaceFirst
  , checkReplacement
  ) where

import Control.Exception (IOException, try)
import Data.Function ((&))
import Data.JsonSchema (booleanSchema, objectSchema, optional, required, stringSchema, withDescription)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Read (ReadTracker, hasBeenRead)
import CodeStar.Tools.Registry
import CodeStar.TreeSitter qualified as TS
import TreeSitter qualified

editToolHandler :: ReadTracker -> Maybe TreeSitter.Language -> Maybe (FilePath -> IO ()) -> ToolHandlerDict
editToolHandler tracker mLang mOnEdit =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "edit"
          , description = "Replace an exact string in a file. File must have been read first."
          , parameters =
              objectSchema
                [ required "path" (stringSchema & withDescription "Absolute path to the file to edit")
                , required "old" (stringSchema & withDescription "The exact string to replace")
                , required "new" (stringSchema & withDescription "The replacement string")
                , optional "replace_all" (booleanSchema & withDescription "Replace all occurrences (default: false)")
                ]
          , riskTier = LocalWrite
          }
    , invoke = invokeEdit tracker mLang mOnEdit
    }

data EditInput = EditInput
  { path :: Text
  , old :: Text
  , new :: Text
  , replaceAll :: Bool
  }

parseEditInput :: ToolInput -> Either ToolError EditInput
parseEditInput raw = do
  p <- extractText "path" raw
  o <- extractText "old" raw
  n <- extractText "new" raw
  r <- extractBool "replace_all" raw
  pure EditInput{path = p, old = o, new = n, replaceAll = r}

invokeEdit :: ReadTracker -> Maybe TreeSitter.Language -> Maybe (FilePath -> IO ()) -> ToolInput -> IO (Either ToolError ToolOutput)
invokeEdit tracker mLang mOnEdit raw =
  case parseEditInput raw of
    Left err -> pure (Left err)
    Right input -> do
      checkReadBefore tracker input.path >>= \case
        Left err -> pure (Left err)
        Right content ->
          case checkReplacement input.old input.replaceAll content of
            Left err -> pure (Left err)
            Right () -> do
              result <- applyEdit mLang input.path input.old input.new input.replaceAll content
              case result of
                Right _ -> do
                  case mOnEdit of
                    Just onEdit -> onEdit (Text.unpack input.path)
                    Nothing -> pure ()
                _ -> pure ()
              pure result

checkReadBefore :: ReadTracker -> Text -> IO (Either ToolError Text)
checkReadBefore tracker path = do
  wasRead <- hasBeenRead tracker (Text.unpack path)
  if not wasRead
    then pure (Left (InvalidInput ("File has not been read yet: " <> path)))
    else do
      result <- try @IOException $ Text.IO.readFile (Text.unpack path)
      pure $ case result of
        Left err -> Left (ExecutionFailed (Text.pack (show err)))
        Right content -> Right content

checkReplacement :: Text -> Bool -> Text -> Either ToolError ()
checkReplacement old replaceAll content
  | Text.null old =
      Left (InvalidInput "old_string cannot be empty")
  | not (Text.isInfixOf old content) =
      Left (InvalidInput "old_string not found in file")
  | not replaceAll && Text.count old content > 1 =
      Left (InvalidInput "old_string is not unique in file. Provide more context or use replace_all.")
  | otherwise = Right ()

applyEdit :: Maybe TreeSitter.Language -> Text -> Text -> Text -> Bool -> Text -> IO (Either ToolError ToolOutput)
applyEdit mLang path old new replaceAll content = do
  let newContent =
        if replaceAll
          then Text.replace old new content
          else replaceFirst old new content
  writeResult <- try @IOException $ Text.IO.writeFile (Text.unpack path) newContent
  case writeResult of
    Left err -> pure (Left (ExecutionFailed (Text.pack (show err))))
    Right () -> do
      syntaxOk <- checkSyntax mLang newContent
      let status = if syntaxOk then "Edit applied." else "Edit applied. WARNING: syntax errors detected."
      pure $ Right ToolOutput{content = status, truncated = False}

checkSyntax :: Maybe TreeSitter.Language -> Text -> IO Bool
checkSyntax Nothing _ = pure True
checkSyntax (Just lang) content = do
  sr <- TS.validate lang (Text.Encoding.encodeUtf8 content)
  pure $ case sr of
    Right r -> r.valid
    Left _ -> True

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

replaceFirst :: Text -> Text -> Text -> Text
replaceFirst old new content =
  if Text.null old
    then content
    else case Text.breakOn old content of
      (before, match)
        | Text.null match -> content
        | otherwise -> before <> new <> Text.drop (Text.length old) match
