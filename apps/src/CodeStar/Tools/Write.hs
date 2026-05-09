module CodeStar.Tools.Write
  ( writeToolHandler
  ) where

import Control.Exception (IOException, try)
import Data.Function ((&))
import Data.JsonSchema (booleanSchema, objectSchema, optional, required, stringSchema, withDescription)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import System.Directory (doesFileExist)

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry
import CodeStar.TreeSitter qualified as TS
import TreeSitter qualified

maxOverwriteLines :: Int
maxOverwriteLines = 200

writeToolHandler :: Maybe TreeSitter.Language -> ToolHandlerDict
writeToolHandler mLang =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "write"
          , description =
              "Write content to a file. Creates new files freely. "
                <> "Overwrites existing files only if they are under "
                <> Text.pack (show maxOverwriteLines)
                <> " lines; use edit for larger files."
          , parameters =
              objectSchema
                [ required "path" (stringSchema & withDescription "Absolute path to write")
                , required "content" (stringSchema & withDescription "Full file content to write")
                , optional "force" (booleanSchema & withDescription "Overwrite even if file is large (default: false)")
                ]
          , riskTier = LocalWrite
          }
    , invoke = invokeWrite mLang
    }

invokeWrite :: Maybe TreeSitter.Language -> ToolInput -> IO (Either ToolError ToolOutput)
invokeWrite mLang input =
  case parseWriteInput input of
    Left err -> pure (Left err)
    Right (path, content, force) -> do
      guardOverwrite path content force >>= \case
        Left err -> pure (Left err)
        Right () -> writeAndValidate mLang path content

parseWriteInput :: ToolInput -> Either ToolError (Text, Text, Bool)
parseWriteInput input = do
  path <- extractText "path" input
  content <- extractText "content" input
  force <- extractBool "force" input
  pure (path, content, force)

guardOverwrite :: Text -> Text -> Bool -> IO (Either ToolError ())
guardOverwrite path _content force = do
  exists <- doesFileExist (Text.unpack path)
  if not exists
    then pure (Right ())
    else do
      existing <- Text.IO.readFile (Text.unpack path)
      let lineCount = length (Text.lines existing)
      if lineCount > maxOverwriteLines && not force
        then
          pure $
            Left $
              InvalidInput $
                "File has "
                  <> Text.pack (show lineCount)
                  <> " lines (max "
                  <> Text.pack (show maxOverwriteLines)
                  <> " for overwrite). Use edit for targeted changes, or pass force=true."
        else pure (Right ())

writeAndValidate :: Maybe TreeSitter.Language -> Text -> Text -> IO (Either ToolError ToolOutput)
writeAndValidate mLang path content = do
  result <- try @IOException $ Text.IO.writeFile (Text.unpack path) content
  case result of
    Left err -> pure (Left (ExecutionFailed (Text.pack (show err))))
    Right () -> do
      syntaxOk <- checkSyntax mLang content
      let status =
            if syntaxOk
              then "File written."
              else "File written. WARNING: syntax errors detected."
      pure $ Right ToolOutput{content = status, truncated = False}

checkSyntax :: Maybe TreeSitter.Language -> Text -> IO Bool
checkSyntax Nothing _ = pure True
checkSyntax (Just lang) content = do
  result <- TS.validateWithTimeout lang (Text.Encoding.encodeUtf8 content)
  pure $ case result of
    Right r -> r.valid
    Left _ -> True
