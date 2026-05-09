module CodeStar.Tools.TodoList
  ( todoListHandlers
  , TodoStore
  , newTodoStore
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.IORef
import Data.JsonSchema (objectSchema, required, stringSchema, withDescription)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.Generics (Generic)

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

-- --------------------------------------------------------------------
-- TodoItem
-- --------------------------------------------------------------------

data TodoStatus = Pending | InProgress | Done
  deriving stock (Eq, Show, Generic)

data TodoItem = TodoItem
  { id :: Text
  , description :: Text
  , status :: TodoStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSON TodoStatus where
  toJSON Pending = String "pending"
  toJSON InProgress = String "in_progress"
  toJSON Done = String "done"

instance FromJSON TodoStatus where
  parseJSON (String "pending") = pure Pending
  parseJSON (String "in_progress") = pure InProgress
  parseJSON (String "done") = pure Done
  parseJSON (String other) = fail ("Unknown TodoStatus: " <> Text.unpack other)
  parseJSON _ = fail "TodoStatus must be a string"

-- --------------------------------------------------------------------
-- Store
-- --------------------------------------------------------------------

newtype TodoStore = TodoStore (IORef [TodoItem])

newTodoStore :: IO TodoStore
newTodoStore = TodoStore <$> newIORef []

-- --------------------------------------------------------------------
-- Tool Handlers (write + read)
-- --------------------------------------------------------------------

todoListHandlers :: TodoStore -> [ToolHandlerDict]
todoListHandlers store =
  [ todoWriteHandler store
  , todoReadHandler store
  ]

todoWriteHandler :: TodoStore -> ToolHandlerDict
todoWriteHandler store =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "todo_write"
          , description =
              "Replace the entire todo list with a new list of items. "
                <> "Each item has an id, description, and status (pending|in_progress|done)."
          , parameters =
              objectSchema
                [required "items" (stringSchema & withDescription "JSON array of TodoItem objects")]
          , riskTier = ReadOnly
          }
    , invoke = invokeWrite store
    }

todoReadHandler :: TodoStore -> ToolHandlerDict
todoReadHandler store =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "todo_read"
          , description = "Read the current todo list."
          , parameters = objectSchema []
          , riskTier = ReadOnly
          }
    , invoke = invokeRead store
    }

invokeWrite :: TodoStore -> ToolInput -> IO (Either ToolError ToolOutput)
invokeWrite (TodoStore ref) input =
  case extractText "items" input of
    Left err -> pure (Left err)
    Right jsonText ->
      case Aeson.eitherDecodeStrict' (encodeUtf8 jsonText) of
        Left err -> pure (Left (InvalidInput ("Invalid items JSON: " <> Text.pack err)))
        Right items -> do
          writeIORef ref items
          pure $
            Right
              ToolOutput
                { content = "Todo list updated with " <> Text.pack (show (length items)) <> " items."
                , truncated = False
                }
 where
  encodeUtf8 = TE.encodeUtf8

invokeRead :: TodoStore -> ToolInput -> IO (Either ToolError ToolOutput)
invokeRead (TodoStore ref) _ = do
  items <- readIORef ref
  let json = Aeson.encode items
      text = TL.toStrict (TLE.decodeUtf8 json)
  pure $ Right ToolOutput{content = text, truncated = False}
