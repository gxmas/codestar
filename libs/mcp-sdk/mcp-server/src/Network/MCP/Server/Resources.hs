-- |
-- Module      : Network.MCP.Server.Resources
-- Stability   : stable
--
-- Resources feature for MCP servers. Registers @resources/list@,
-- @resources/templates/list@, @resources/read@,
-- @resources/subscribe@, and @resources/unsubscribe@ handlers.
module Network.MCP.Server.Resources
  ( -- * Registry
    ResourceRegistry
  , newResourceRegistry
  , registerResource
  , unregisterResource
  , registerTemplate
  , unregisterTemplate
  , attach
  , detach
  , notifyUpdated
  , notifyListChanged

    -- * Types
  , ResourceDefinition (..)
  , ResourceTemplateDefinition (..)
  , ResourceContext (..)
  , ResourceHandler

    -- * Exported for testing
  , matchesTemplate
  , templateSegments
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
import Network.MCP.Types.Content
  ( Annotations
  , Icon
  , ResourceContents
  , URI (..)
  )

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A concrete resource definition.
data ResourceDefinition = ResourceDefinition
  { resUri         :: !URI
  , resName        :: !Text
  , resTitle       :: !(Maybe Text)
  , resDescription :: !(Maybe Text)
  , resMimeType    :: !(Maybe Text)
  , resSize        :: !(Maybe Word)
  , resIcons       :: !(Maybe [Icon])
  , resAnnotations :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ResourceDefinition where
  toJSON d =
    Aeson.object $
      [ "uri"  .= d.resUri
      , "name" .= d.resName
      ]
        ++ maybe [] (\t -> ["title"       .= t]) d.resTitle
        ++ maybe [] (\t -> ["description" .= t]) d.resDescription
        ++ maybe [] (\m -> ["mimeType"    .= m]) d.resMimeType
        ++ maybe [] (\s -> ["size"        .= s]) d.resSize
        ++ maybe [] (\i -> ["icons"       .= i]) d.resIcons
        ++ maybe [] (\a -> ["annotations" .= a]) d.resAnnotations
  toEncoding d =
    E.pairs $
      "uri" .= d.resUri
        <> "name" .= d.resName
        <> foldMap ("title"       .=) d.resTitle
        <> foldMap ("description" .=) d.resDescription
        <> foldMap ("mimeType"    .=) d.resMimeType
        <> foldMap ("size"        .=) d.resSize
        <> foldMap ("icons"       .=) d.resIcons
        <> foldMap ("annotations" .=) d.resAnnotations

instance Aeson.FromJSON ResourceDefinition where
  parseJSON = Aeson.withObject "ResourceDefinition" $ \o ->
    ResourceDefinition
      <$> o .:  "uri"
      <*> o .:  "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "mimeType"
      <*> o .:? "size"
      <*> o .:? "icons"
      <*> o .:? "annotations"

-- | A URI template definition.
data ResourceTemplateDefinition = ResourceTemplateDefinition
  { resTplUriTemplate  :: !Text
  , resTplName         :: !Text
  , resTplTitle        :: !(Maybe Text)
  , resTplDescription  :: !(Maybe Text)
  , resTplMimeType     :: !(Maybe Text)
  , resTplIcons        :: !(Maybe [Icon])
  , resTplAnnotations  :: !(Maybe Annotations)
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON ResourceTemplateDefinition where
  toJSON d =
    Aeson.object $
      [ "uriTemplate" .= d.resTplUriTemplate
      , "name"        .= d.resTplName
      ]
        ++ maybe [] (\t -> ["title"       .= t]) d.resTplTitle
        ++ maybe [] (\t -> ["description" .= t]) d.resTplDescription
        ++ maybe [] (\m -> ["mimeType"    .= m]) d.resTplMimeType
        ++ maybe [] (\i -> ["icons"       .= i]) d.resTplIcons
        ++ maybe [] (\a -> ["annotations" .= a]) d.resTplAnnotations
  toEncoding d =
    E.pairs $
      "uriTemplate" .= d.resTplUriTemplate
        <> "name" .= d.resTplName
        <> foldMap ("title"       .=) d.resTplTitle
        <> foldMap ("description" .=) d.resTplDescription
        <> foldMap ("mimeType"    .=) d.resTplMimeType
        <> foldMap ("icons"       .=) d.resTplIcons
        <> foldMap ("annotations" .=) d.resTplAnnotations

instance Aeson.FromJSON ResourceTemplateDefinition where
  parseJSON = Aeson.withObject "ResourceTemplateDefinition" $ \o ->
    ResourceTemplateDefinition
      <$> o .:  "uriTemplate"
      <*> o .:  "name"
      <*> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "mimeType"
      <*> o .:? "icons"
      <*> o .:? "annotations"

-- | Context passed to a resource handler (internal, no JSON instances).
data ResourceContext = ResourceContext
  { resCtxUri  :: !URI
  , resCtxMeta :: !RequestMeta
  }

-- | A resource handler.
type ResourceHandler = ResourceContext -> IO (Either RPCError [ResourceContents])

------------------------------------------------------------------------
-- Registry
------------------------------------------------------------------------

-- | Opaque resource registry.
-- Keys are the text representation of URIs for 'Hashable' compatibility.
data ResourceRegistry = ResourceRegistry
  { rrConcreteMap :: !(TVar (HashMap Text (ResourceDefinition, ResourceHandler)))
  , rrTemplates   :: !(TVar [(ResourceTemplateDefinition, ResourceHandler)])
  , rrSubs        :: !(TVar (HashMap Text [URI -> IO ()]))
  , rrSession     :: !(TVar (Maybe Session))
  }

newResourceRegistry :: IO ResourceRegistry
newResourceRegistry = do
  concTVar <- newTVarIO HM.empty
  tplTVar  <- newTVarIO []
  subTVar  <- newTVarIO HM.empty
  sesTVar  <- newTVarIO Nothing
  pure (ResourceRegistry concTVar tplTVar subTVar sesTVar)

-- | Register a concrete resource.
registerResource :: ResourceRegistry -> ResourceDefinition -> ResourceHandler -> IO ()
registerResource rr def handler =
  atomically $ modifyTVar' rr.rrConcreteMap (HM.insert (uriText def.resUri) (def, handler))

-- | Unregister a concrete resource by URI.
unregisterResource :: ResourceRegistry -> URI -> IO ()
unregisterResource rr uri =
  atomically $ modifyTVar' rr.rrConcreteMap (HM.delete (uriText uri))

-- | Register a URI template resource (appended in registration order).
registerTemplate :: ResourceRegistry -> ResourceTemplateDefinition -> ResourceHandler -> IO ()
registerTemplate rr def handler =
  atomically $ modifyTVar' rr.rrTemplates (++ [(def, handler)])

-- | Unregister a template by URI template string.
unregisterTemplate :: ResourceRegistry -> Text -> IO ()
unregisterTemplate rr uriTemplate =
  atomically $ modifyTVar' rr.rrTemplates
    (filter (\(d, _) -> d.resTplUriTemplate /= uriTemplate))

-- | Attach the registry to a session, registering all handlers.
attach :: ResourceRegistry -> Session -> IO ()
attach rr session = do
  atomically (writeTVar rr.rrSession (Just session))
  session.sessionOnRequest "resources/list"              (handleList rr)
  session.sessionOnRequest "resources/templates/list"    (handleTemplateList rr)
  session.sessionOnRequest "resources/read"              (handleRead rr)
  session.sessionOnRequest "resources/subscribe"         (handleSubscribe rr)
  session.sessionOnRequest "resources/unsubscribe"       (handleUnsubscribe rr)

-- | Detach from the current session.
detach :: ResourceRegistry -> IO ()
detach rr = atomically (writeTVar rr.rrSession Nothing)

-- | Notify all subscribers that the resource at @uri@ has been updated.
notifyUpdated :: ResourceRegistry -> URI -> IO ()
notifyUpdated rr uri = do
  subs <- readTVarIO rr.rrSubs
  case HM.lookup (uriText uri) subs of
    Nothing        -> pure ()
    Just callbacks -> mapM_ ($ uri) callbacks

-- | Send a @notifications/resources/list_changed@ notification.
notifyListChanged :: ResourceRegistry -> IO ()
notifyListChanged rr = do
  mSession <- readTVarIO rr.rrSession
  case mSession of
    Nothing      -> pure ()
    Just session -> session.sessionNotify "notifications/resources/list_changed" Nothing

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

handleList :: ResourceRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleList rr params _meta = do
  let mcursor = case Aeson.fromJSON params of
        Aeson.Error _                 -> Nothing
        Aeson.Success (ListParams mc) -> mc
  entries <- readTVarIO rr.rrConcreteMap
  let sorted = sortBy (comparing (uriText . (.resUri) . fst)) (HM.elems entries)
      defs   = map fst sorted
      (page, nextCursor) = paginate defs mcursor
  pure $ Right $ Aeson.object $
    [ "resources" .= page ]
      ++ maybe [] (\c -> ["nextCursor" .= c]) nextCursor

handleTemplateList :: ResourceRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleTemplateList rr params _meta = do
  let mcursor = case Aeson.fromJSON params of
        Aeson.Error _                 -> Nothing
        Aeson.Success (ListParams mc) -> mc
  templates <- readTVarIO rr.rrTemplates
  let defs = map fst templates
      (page, nextCursor) = paginate defs mcursor
  pure $ Right $ Aeson.object $
    [ "resourceTemplates" .= page ]
      ++ maybe [] (\c -> ["nextCursor" .= c]) nextCursor

handleRead :: ResourceRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleRead rr params meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (ReadParams uri) -> do
      entries <- readTVarIO rr.rrConcreteMap
      case HM.lookup (uriText uri) entries of
        Just (_def, handler) -> callHandler handler uri meta
        Nothing -> do
          templates <- readTVarIO rr.rrTemplates
          case findTemplate uri templates of
            Nothing ->
              pure (Left (RPCError (-32002) ("Resource not found: " <> uriText uri) Nothing))
            Just (_tpl, handler) -> callHandler handler uri meta

handleSubscribe :: ResourceRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleSubscribe rr params _meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (SubscribeParams uri) -> do
      mSession <- readTVarIO rr.rrSession
      let callback = \u -> case mSession of
            Nothing      -> pure ()
            Just session ->
              session.sessionNotify "notifications/resources/updated"
                (Just (Aeson.object ["uri" .= u]))
      atomically $ modifyTVar' rr.rrSubs (HM.insertWith (++) (uriText uri) [callback])
      pure (Right (Aeson.object []))

handleUnsubscribe :: ResourceRegistry -> Aeson.Value -> RequestMeta -> IO (Either RPCError Aeson.Value)
handleUnsubscribe rr params _meta =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure (Left (RPCError (-32602) (T.pack $ "Invalid params: " ++ err) Nothing))
    Aeson.Success (SubscribeParams uri) -> do
      atomically $ modifyTVar' rr.rrSubs (HM.delete (uriText uri))
      pure (Right (Aeson.object []))

callHandler :: ResourceHandler -> URI -> RequestMeta -> IO (Either RPCError Aeson.Value)
callHandler handler uri meta = do
  result <- handler (ResourceContext uri meta)
  case result of
    Left err       -> pure (Left err)
    Right contents -> pure (Right (Aeson.object ["contents" .= contents]))

------------------------------------------------------------------------
-- Template matching (Level-1 RFC 6570 subset)
------------------------------------------------------------------------

-- | Split a URI template on @{...}@ expressions to get literal segments.
-- E.g. @"resource://{id}/data"@ → @["resource://", "/data"]@.
templateSegments :: Text -> [Text]
templateSegments tpl = go tpl []
  where
    go "" acc = reverse acc
    go t acc =
      case T.breakOn "{" t of
        (before, "") -> reverse (before : acc)
        (before, rest) ->
          case T.breakOn "}" (T.drop 1 rest) of
            (_, "")     -> reverse (before : acc)
            (_, after)  -> go (T.drop 1 after) (before : acc)

-- | Check whether a URI matches a Level-1 template.
-- A URI matches if it starts with the first segment and ends with the
-- last segment (with content in between for each variable slot).
matchesTemplate :: URI -> Text -> Bool
matchesTemplate (URI uriTxt) tpl =
  let segs = templateSegments tpl
  in case segs of
    []     -> True
    [s]    -> uriTxt == s
    (s:ss) ->
      T.isPrefixOf s uriTxt
        && T.isSuffixOf (last ss) uriTxt
        && T.length uriTxt >= T.length s + T.length (last ss)

-- | Find the first template that matches the given URI.
findTemplate
  :: URI
  -> [(ResourceTemplateDefinition, ResourceHandler)]
  -> Maybe (ResourceTemplateDefinition, ResourceHandler)
findTemplate uri = foldr check Nothing
  where
    check entry@(def, _) acc
      | matchesTemplate uri def.resTplUriTemplate = Just entry
      | otherwise = acc

uriText :: URI -> Text
uriText (URI t) = t

------------------------------------------------------------------------
-- Internal param types
------------------------------------------------------------------------

newtype ListParams = ListParams (Maybe Cursor)

instance Aeson.FromJSON ListParams where
  parseJSON = Aeson.withObject "ListParams" $ \o ->
    ListParams <$> o .:? "cursor"

newtype ReadParams = ReadParams URI

instance Aeson.FromJSON ReadParams where
  parseJSON = Aeson.withObject "ReadParams" $ \o ->
    ReadParams <$> o .: "uri"

newtype SubscribeParams = SubscribeParams URI

instance Aeson.FromJSON SubscribeParams where
  parseJSON = Aeson.withObject "SubscribeParams" $ \o ->
    SubscribeParams <$> o .: "uri"

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
