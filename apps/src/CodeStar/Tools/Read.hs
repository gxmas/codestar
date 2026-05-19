{- |
= Tools.Read — file reading tool

Exposes the @read_file@ tool that the agent uses to read source files.
The tool returns file contents with line numbers so the agent can
reference specific lines in subsequent edits.

The 'ReadTracker' records which files have been read in the current
session.  The edit tool uses this to enforce that a file must be read
before it can be edited — preventing the agent from blindly overwriting
a file it has not seen.
-}
module CodeStar.Tools.Read
  ( readToolHandler
  , ReadTracker
  , newReadTracker
  , hasBeenRead
  )
where

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry
import Control.Exception (IOException, try)
import Data.Function ((&))
import Data.IORef
import Data.JsonSchema (integerSchema, objectSchema, optional, required, stringSchema, withDescription, withMinimum)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

-- --------------------------------------------------------------------
-- Read Tracker
-- --------------------------------------------------------------------

newtype ReadTracker = ReadTracker (IORef (Set FilePath))

newReadTracker :: IO ReadTracker
newReadTracker = ReadTracker <$> newIORef Set.empty

trackRead :: ReadTracker -> FilePath -> IO ()
trackRead (ReadTracker ref) path = modifyIORef' ref (Set.insert path)

hasBeenRead :: ReadTracker -> FilePath -> IO Bool
hasBeenRead (ReadTracker ref) path = Set.member path <$> readIORef ref

-- --------------------------------------------------------------------
-- Parsed Input
-- --------------------------------------------------------------------

data ReadInput = ReadInput
  { path :: Text
  , offset :: Maybe Int
  , limit :: Maybe Int
  }

parseReadInput :: ToolInput -> Either ToolError ReadInput
parseReadInput input = do
  p <- extractText "path" input
  o <- extractInt "offset" input
  l <- extractInt "limit" input
  case o of
    Just n | n < 0 -> Left (InvalidInput "offset: must be >= 0")
    _ -> pure ()
  case l of
    Just n | n < 1 -> Left (InvalidInput "limit: must be >= 1")
    _ -> pure ()
  pure ReadInput{path = p, offset = o, limit = l}

-- --------------------------------------------------------------------
-- Tool Handler
-- --------------------------------------------------------------------

readToolHandler :: ReadTracker -> ToolHandlerDict
readToolHandler tracker =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "read"
          , description = "Read a file with line numbers. Supports offset and limit."
          , parameters =
              objectSchema
                [ required "path" (stringSchema & withDescription "Absolute path to the file to read")
                , optional "offset" (integerSchema & withDescription "Line number to start reading from" & withMinimum 0)
                , optional "limit" (integerSchema & withDescription "Number of lines to read" & withMinimum 1)
                ]
          , riskTier = ReadOnly
          }
    , invoke = invokeRead tracker
    }

invokeRead :: ReadTracker -> ToolInput -> IO (Either ToolError ToolOutput)
invokeRead tracker raw =
  case parseReadInput raw of
    Left err -> pure (Left err)
    Right input -> do
      result <- try @IOException $ Text.IO.readFile (Text.unpack input.path)
      case result of
        Left err -> pure $ Left (ExecutionFailed (Text.pack (show err)))
        Right content -> do
          trackRead tracker (Text.unpack input.path)
          let allLines = Text.lines content
              totalLines = length allLines
              off = maybe 0 id input.offset
              selectedLines = maybe id take input.limit (drop off allLines)
              numbered = zipWith formatLine [off + 1 ..] selectedLines
              output =
                Text.unlines numbered
                  <> "\n("
                  <> Text.pack (show totalLines)
                  <> " lines total)"
          pure $
            Right
              ToolOutput
                { content = output
                , truncated = False
                }

formatLine :: Int -> Text -> Text
formatLine n line =
  let num = Text.pack (show n)
      padding = Text.replicate (6 - Text.length num) " "
   in padding <> num <> "\t" <> line
