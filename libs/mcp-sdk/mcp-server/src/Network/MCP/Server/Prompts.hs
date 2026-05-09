-- |
-- Module      : Network.MCP.Server.Prompts
-- Stability   : stable
--
-- Prompts feature for MCP servers. Registers @prompts/list@ and
-- @prompts/get@ request handlers.
module Network.MCP.Server.Prompts
  ( -- * Registry
    PromptRegistry
  , newPromptRegistry
  , register
  , unregister
  , attach
  , detach
  , notifyListChanged

    -- * Types
  , PromptArgument (..)
  , PromptDefinition (..)
  , PromptMessage (..)
  , PromptResult (..)
  , PromptContext (..)
  , PromptHandler
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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

import Network.MCP.Session (RequestMeta (..), Session (..))
import Network.MCP.Types (Cursor (..), RPCError (..))
import Network.MCP.Types.Content (ContentBlock, Icon, Role)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A single argument accepted by a prompt.
data PromptArgument = PromptArgument
  { promptArgName        :: !Text
  , promptArgDescription :: !(Maybe Text)
  , promptArgRequired    :: !Bool
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON PromptArgument where
  toJSON a =
    Aeson.object $
      [ "name"     .= a.promptArgName
      , "required" .= a.promptArgRequired
      ]
        ++ maybe [] (\d -> ["description" .= d]) a.promptArgDescription
  toEncoding a =
    E.pairs $
      "name" .= a.promptArgName
        <> "required" .= a.promptArgRequired
        <> foldMap ("description" .=) a.promptArgDescription

instance Aeson.FromJSON PromptArgument where
  parseJSON = Aeson.withObject "PromptArgument" $ \o ->
    PromptArgument
      <$> o .:  "name"
      <*> o .:? "description"
      <*> o .:  "required"

-- | A prompt definition registered with the registry.
data PromptDefinition = PromptDefinition
  { promptName        :: !Text
  , promptTitle       :: !(Maybe Text)
  , promptDescription :: !(Maybe Text)
  , promptArguments   :: !(Maybe [PromptArgument])
  , promptIcons       :: !(Maybe [Icon])
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON PromptDefinition where
  toJSON d =
    Aeson.object $
      [ "name" .= d.promptName ]
        ++ maybe [] (\t -> ["title" .= t]) d.promptTitle
        ++ maybe [] (\t -> ["description" .= t]) d.promptDescription
        ++ maybe [] (\a -> ["arguments" .= a]) d.promptArguments
        ++ maybe [] (\i -> ["icons" .= i]) d.promptIcons
  toEncoding d =
    E.pairs $
      "name" .= d.promptName
        <> foldMap ("title" .=) d.promptTitle
        <> foldMap ("description" .=) d.promptDescription
        <> foldMap ("arguments" .=) d.promptArguments
        <> foldMap ("icons" .=) d.promptIcons

instance Aeson.FromJSON PromptDefinition where
  parseJSON = Aeson.withObject "PromptDefinition" $ \o ->
    PromptDefinition
      <$> o .:  "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "arguments"
      <*> o .:? "icons"

-- | A single message in a prompt result.
data PromptMessage = PromptMessage
  { promptMsgRole    :: !Role
  , promptMsgContent :: !ContentBlock
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON PromptMessage where
  toJSON m =
    Aeson.object
      [ "role"    .= m.promptMsgRole
      , "content" .= m.promptMsgContent
      ]
  toEncoding m =
    E.pairs $
      "role" .= m.promptMsgRole
        <> "content" .= m.promptMsgContent

instance Aeson.FromJSON PromptMessage where
  parseJSON = Aeson.withObject "PromptMessage" $ \o ->
    PromptMessage
      <$> o .: "role"
      <*> o .: "content"

-- | The result returned by a prompt handler.
data PromptResult = PromptResult
  { promptResultDescription :: !(Maybe Text)
  , promptResultMessages    :: ![PromptMessage]
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON PromptResult where
  toJSON r =
    Aeson.object $
      [ "messages" .= r.promptResultMessages ]
        ++ maybe [] (\d -> ["description" .= d]) r.promptResultDescription
  toEncoding r =
    E.pairs $
      "messages" .= r.promptResultMessages
        <> foldMap ("description" .=) r.promptResultDescription

instance Aeson.FromJSON PromptResult where
  parseJSON = Aeson.withObject "PromptResult" $ \o ->
    PromptResult
      <$> o .:? "description"
      <*> o .:  "messages"

-- | Context passed to a prompt handler (internal, no JSON instances).
data PromptContext = PromptContext
  { promptCtxArguments :: !(HashMap Text Text)
  , promptCtxMeta      :: !RequestMeta
  }

-- | A prompt handler.
type PromptHandler = PromptContext -> IO (Either RPCError PromptResult)

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

-- | Opaque prompt registry.
data PromptRegistry = PromptRegistry
  { prMap     :: !(TVar (HashMap Text (PromptDefinition, PromptHandler)))
  , prSession :: !(TVar (Maybe Session))
  }

newPromptRegistry :: IO PromptRegistry
newPromptRegistry = do
  mapTVar <- newTVarIO HM.empty
  sesTVar <- newTVarIO Nothing
  pure (PromptRegistry mapTVar sesTVar)

-- | Register a prompt definition and its handler.
register :: PromptRegistry -> PromptDefinition -> PromptHandler -> IO ()
register pr def handler =
  atomically $ modifyTVar' pr.prMap (HM.insert def.promptName (def, handler))

-- | Unregister a prompt by name.
unregister :: PromptRegistry -> Text -> IO ()
unregister pr name =
  atomically $ modifyTVar' pr.prMap (HM.delete name)

-- | Attach the registry to a session, registering handlers.
attach :: PromptRegistry -> Session -> IO ()
attach pr session = do
  atomically (writeTVar pr.prSession (Just session))
  session.sessionOnRequest "prompts/list" (handleList pr)
  session.sessionOnRequest "prompts/get"  (handleGet pr)

-- | Detach from the current session.
detach :: PromptRegistry -> IO ()
detach pr = atomically (writeTVar pr.prSession Nothing)

-- | Send a @notifications/prompts/list_changed@ notification.
notifyListChanged :: PromptRegistry -> IO ()
notifyListChanged pr = do
  mSession <- readTVarIO pr.prSession
  case mSession of
    Nothing      -> pure ()
    Just session -> session.sessionNotify "notifications/prompts/list_changed" Nothing

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

handleList :: PromptRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleList pr params _meta = do
  let mcursor = case Aeson.fromJSON params of
        Aeson.Error _                    -> Nothing
        Aeson.Success (ListParams mc)    -> mc
  entries <- readTVarIO pr.prMap
  let sorted = sortBy (comparing ((.promptName) . fst)) (HM.elems entries)
      defs   = map fst sorted
      (page, nextCursor) = paginate defs mcursor
  pure $ Right $ Aeson.object $
    [ "prompts" .= page ]
      ++ maybe [] (\c -> ["nextCursor" .= c]) nextCursor

handleGet :: PromptRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleGet pr params meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (GetParams name margs) -> do
      entries <- readTVarIO pr.prMap
      case HM.lookup name entries of
        Nothing ->
          pure (Left (RPCError (-32602) ("Prompt not found: " <> name) Nothing))
        Just (def, handler) -> do
          let args = maybe HM.empty id margs
          case validateArgs def args of
            Just errMsg ->
              pure (Left (RPCError (-32602) errMsg Nothing))
            Nothing -> do
              let ctx = PromptContext args meta
              result <- handler ctx
              case result of
                Left err -> pure (Left err)
                Right r  -> pure (Right (Aeson.toJSON r))

-- | Validate required arguments. Returns an error message if a required
-- argument is missing.
validateArgs :: PromptDefinition -> HashMap Text Text -> Maybe Text
validateArgs def args =
  case def.promptArguments of
    Nothing   -> Nothing
    Just pargs ->
      let missing = [ pa.promptArgName
                    | pa <- pargs
                    , pa.promptArgRequired
                    , not (HM.member pa.promptArgName args)
                    ]
      in case missing of
        []    -> Nothing
        (n:_) -> Just ("Missing required argument: " <> n)

------------------------------------------------------------------------
-- Internal param types
------------------------------------------------------------------------

newtype ListParams = ListParams (Maybe Cursor)

instance Aeson.FromJSON ListParams where
  parseJSON = Aeson.withObject "ListParams" $ \o ->
    ListParams <$> o .:? "cursor"

data GetParams = GetParams !Text !(Maybe (HashMap Text Text))

instance Aeson.FromJSON GetParams where
  parseJSON = Aeson.withObject "GetParams" $ \o ->
    GetParams
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
