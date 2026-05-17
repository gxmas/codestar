-- |
-- Module      : Network.MCP.Server.Tools
-- Stability   : stable
--
-- Tools feature for MCP servers. Registers @tools/list@ and
-- @tools/call@ request handlers.
module Network.MCP.Server.Tools
  ( -- * Registry
    ToolRegistry
  , newToolRegistry
  , register
  , unregister
  , attach
  , detach
  , notifyListChanged

    -- * Types
  , ToolAnnotations (..)
  , TaskSupportMode (..)
  , ToolDefinition (..)
  , ToolCallContext (..)
  , ToolResult (..)
  , ToolHandler

    -- * Errors
  , RegistryErrorKind (..)
  , RegistryError (..)

    -- * Exported for testing
  , isValidName
  , paginate
  , encodeCursor
  , decodeCursor
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import qualified Data.ByteString.Base64 as B64
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.JsonSchema (Schema)
import qualified Data.JsonSchema as JsonSchema
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import Network.MCP.Session (RequestMeta (..), Session (..))
import Network.MCP.Types (Cursor (..), RPCError (..))
import Network.MCP.Types.Content (ContentBlock, Icon, ToolAnnotations (..))

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Whether a tool supports task creation.
data TaskSupportMode
  = TaskForbidden
  | TaskOptional
  | TaskRequired
  deriving stock (Eq, Show, Bounded, Enum)

instance Aeson.ToJSON TaskSupportMode where
  toJSON TaskForbidden = "forbidden"
  toJSON TaskOptional  = "optional"
  toJSON TaskRequired  = "required"
  toEncoding TaskForbidden = E.text "forbidden"
  toEncoding TaskOptional  = E.text "optional"
  toEncoding TaskRequired  = E.text "required"

instance Aeson.FromJSON TaskSupportMode where
  parseJSON = Aeson.withText "TaskSupportMode" $ \case
    "forbidden" -> pure TaskForbidden
    "optional"  -> pure TaskOptional
    "required"  -> pure TaskRequired
    other       -> fail $ "Unknown TaskSupportMode: " ++ T.unpack other

-- | A tool definition registered with the registry.
data ToolDefinition = ToolDefinition
  { toolName         :: !Text
  , toolTitle        :: !(Maybe Text)
  , toolDescription  :: !(Maybe Text)
  , toolInputSchema  :: !Schema
  , toolOutputSchema :: !(Maybe Schema)
  , toolAnnotations  :: !(Maybe ToolAnnotations)
  , toolIcons        :: !(Maybe [Icon])
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ToolDefinition where
  toJSON d =
    Aeson.object $
      [ "name"        .= d.toolName
      , "inputSchema" .= JsonSchema.encode d.toolInputSchema
      ]
        ++ maybe [] (\t -> ["title"        .= t]) d.toolTitle
        ++ maybe [] (\t -> ["description"  .= t]) d.toolDescription
        ++ maybe [] (\s -> ["outputSchema" .= JsonSchema.encode s]) d.toolOutputSchema
        ++ maybe [] (\a -> ["annotations"  .= a]) d.toolAnnotations
        ++ maybe [] (\i -> ["icons"        .= i]) d.toolIcons
  toEncoding d =
    E.pairs $
      "name" .= d.toolName
        <> "inputSchema" .= JsonSchema.encode d.toolInputSchema
        <> foldMap ("title"       .=) d.toolTitle
        <> foldMap ("description" .=) d.toolDescription
        <> foldMap (\s -> "outputSchema" .= JsonSchema.encode s) d.toolOutputSchema
        <> foldMap ("annotations" .=) d.toolAnnotations
        <> foldMap ("icons"       .=) d.toolIcons

instance Aeson.FromJSON ToolDefinition where
  parseJSON = Aeson.withObject "ToolDefinition" $ \o -> do
    name        <- o .:  "name"
    inputVal    <- o .:  "inputSchema"
    inputSchema <- case JsonSchema.decode inputVal of
      Left err -> fail $ "Invalid inputSchema: " ++ show err
      Right s  -> pure s
    mOutputVal  <- o .:? "outputSchema"
    mOutputSchema <- case mOutputVal of
      Nothing  -> pure Nothing
      Just val -> case JsonSchema.decode val of
        Left err -> fail $ "Invalid outputSchema: " ++ show err
        Right s  -> pure (Just s)
    ToolDefinition name
      <$> o .:? "title"
      <*> o .:? "description"
      <*> pure inputSchema
      <*> pure mOutputSchema
      <*> o .:? "annotations"
      <*> o .:? "icons"

-- | Context passed to a tool handler (internal, no JSON instances).
data ToolCallContext = ToolCallContext
  { toolCallArguments :: !Aeson.Value
  , toolCallMeta      :: !RequestMeta
  }

-- | Result of a tool call.
data ToolResult
  = ToolSuccess ![ContentBlock] !(Maybe Aeson.Value)
  | ToolError   ![ContentBlock]
  deriving stock (Eq, Show)

-- | A tool handler.
type ToolHandler = ToolCallContext -> IO (Either RPCError ToolResult)

-- | Error kinds for registry operations.
data RegistryErrorKind
  = DuplicateName
  | NotFound
  | NotAttached
  deriving stock (Eq, Show)

-- | A registry operation error.
data RegistryError = RegistryError
  { registryErrorKind   :: !RegistryErrorKind
  , registryErrorDetail :: !Text
  }
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

-- | Opaque tool registry.
data ToolRegistry = ToolRegistry
  { trMap     :: !(TVar (HashMap Text (ToolDefinition, ToolHandler)))
  , trSession :: !(TVar (Maybe Session))
  }

newToolRegistry :: IO ToolRegistry
newToolRegistry = do
  mapTVar <- newTVarIO HM.empty
  sesTVar <- newTVarIO Nothing
  pure (ToolRegistry mapTVar sesTVar)

-- | Register a tool. Returns 'Left DuplicateName' if the name is already
-- registered, or 'Left' with an invalid name error.
register :: ToolRegistry -> ToolDefinition -> ToolHandler -> IO (Either RegistryError ())
register tr def handler
  | not (isValidName def.toolName) =
      pure (Left (RegistryError DuplicateName ("Invalid tool name: " <> def.toolName)))
  | otherwise = do
      entries <- readTVarIO tr.trMap
      if HM.member def.toolName entries
        then pure (Left (RegistryError DuplicateName ("Duplicate tool name: " <> def.toolName)))
        else do
          atomically $ modifyTVar' tr.trMap (HM.insert def.toolName (def, handler))
          pure (Right ())

-- | Unregister a tool by name.
unregister :: ToolRegistry -> Text -> IO (Either RegistryError ())
unregister tr name = do
  entries <- readTVarIO tr.trMap
  if HM.member name entries
    then do
      atomically $ modifyTVar' tr.trMap (HM.delete name)
      pure (Right ())
    else pure (Left (RegistryError NotFound ("Tool not found: " <> name)))

-- | Attach the registry to a session, registering handlers.
attach :: ToolRegistry -> Session -> IO ()
attach tr session = do
  atomically (writeTVar tr.trSession (Just session))
  session.sessionOnRequest "tools/list" (handleList tr)
  session.sessionOnRequest "tools/call" (handleCall tr)

-- | Detach from the current session.
detach :: ToolRegistry -> IO ()
detach tr = atomically (writeTVar tr.trSession Nothing)

-- | Send a @notifications/tools/list_changed@ notification.
notifyListChanged :: ToolRegistry -> IO ()
notifyListChanged tr = do
  mSession <- readTVarIO tr.trSession
  case mSession of
    Nothing      -> pure ()
    Just session -> session.sessionNotify "notifications/tools/list_changed" Nothing

------------------------------------------------------------------------
-- Name validation
------------------------------------------------------------------------

isValidName :: Text -> Bool
isValidName name =
  T.length name >= 1
    && T.length name <= 128
    && T.all isValidChar name
  where
    isValidChar c =
      isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '-' || c == '.'

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

handleList :: ToolRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleList tr params _meta = do
  let mcursor = case Aeson.fromJSON params of
        Aeson.Error _                 -> Nothing
        Aeson.Success (ListParams mc) -> mc
  entries <- readTVarIO tr.trMap
  let sorted = sortBy (comparing ((.toolName) . fst)) (HM.elems entries)
      defs   = map fst sorted
      (page, nextCursor) = paginate defs mcursor
  pure $ Right $ Aeson.object $
    [ "tools" .= page ]
      ++ maybe [] (\c -> ["nextCursor" .= c]) nextCursor

handleCall :: ToolRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleCall tr params meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (CallParams name margs) -> do
      entries <- readTVarIO tr.trMap
      case HM.lookup name entries of
        Nothing ->
          pure (Left (RPCError (-32602) ("Tool not found: " <> name) Nothing))
        Just (_def, handler) -> do
          let args = maybe Aeson.Null id margs
              ctx  = ToolCallContext args meta
          result <- handler ctx
          case result of
            Left err -> pure (Left err)
            Right (ToolSuccess content mStructured) ->
              pure $ Right $ Aeson.object $
                [ "content" .= content ]
                  ++ maybe [] (\s -> ["structuredContent" .= s]) mStructured
            Right (ToolError content) ->
              pure $ Right $ Aeson.object
                [ "content" .= content
                , "isError"  .= True
                ]

------------------------------------------------------------------------
-- Internal param types
------------------------------------------------------------------------

newtype ListParams = ListParams (Maybe Cursor)

instance Aeson.FromJSON ListParams where
  parseJSON = Aeson.withObject "ListParams" $ \o ->
    ListParams <$> o .:? "cursor"

data CallParams = CallParams !Text !(Maybe Aeson.Value)

instance Aeson.FromJSON CallParams where
  parseJSON = Aeson.withObject "CallParams" $ \o ->
    CallParams
      <$> o .:  "name"
      <*> o .:? "arguments"

------------------------------------------------------------------------
-- Pagination
------------------------------------------------------------------------

pageSize :: Int
pageSize = 50

encodeCursor :: Int -> Cursor
encodeCursor n =
  Cursor (TE.decodeUtf8 (B64.encode (TE.encodeUtf8 (T.pack (show n)))))

decodeCursor :: Cursor -> Maybe Int
decodeCursor (Cursor t) =
  case B64.decode (TE.encodeUtf8 t) of
    Left _   -> Nothing
    Right bs -> readMaybe (T.unpack (TE.decodeUtf8 bs))

paginate :: [a] -> Maybe Cursor -> ([a], Maybe Cursor)
paginate items mcursor =
  let offset     = maybe 0 (maybe 0 id . decodeCursor) mcursor
      page       = take pageSize (drop offset items)
      nextOffset = offset + length page
      nextCursor =
        if nextOffset < length items
          then Just (encodeCursor nextOffset)
          else Nothing
  in (page, nextCursor)
