-- |
-- Module      : Network.MCP.Client.Resources
-- Stability   : experimental
--
-- Client-side resource discovery and reading. Sends @resources/list@
-- and @resources/read@ requests over a connected 'Session'.
module Network.MCP.Client.Resources
  ( -- * Types
    ClientResourceDef (..)

    -- * Discovery
  , listResources
  , listResourcesPage

    -- * Reading
  , readResource
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Session (Session (..))
import Network.MCP.Types (Cursor (..), RPCError (..))
import Network.MCP.Types.Content (ResourceContents)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A resource advertised by an MCP server, as seen from the client side.
data ClientResourceDef = ClientResourceDef
  { crdUri         :: !Text
  , crdName        :: !Text
  , crdDescription :: !(Maybe Text)
  , crdMimeType    :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON ClientResourceDef where
  parseJSON = Aeson.withObject "ClientResourceDef" $ \o ->
    ClientResourceDef
      <$> o .: "uri"
      <*> o .: "name"
      <*> o .:? "description"
      <*> o .:? "mimeType"

------------------------------------------------------------------------
-- Discovery
------------------------------------------------------------------------

-- | List all resources from a connected server, driving pagination
-- automatically.
listResources :: Session -> IO (Either RPCError [ClientResourceDef])
listResources session = go Nothing []
  where
    go cursor acc = do
      result <- listResourcesPage session cursor
      case result of
        Left err -> pure (Left err)
        Right (resources, Nothing) -> pure (Right (acc ++ resources))
        Right (resources, Just nextCursor) -> go (Just nextCursor) (acc ++ resources)

-- | List a single page of resources from a connected server.
listResourcesPage
  :: Session
  -> Maybe Cursor
  -> IO (Either RPCError ([ClientResourceDef], Maybe Cursor))
listResourcesPage session cursor = do
  let params = case cursor of
        Nothing -> Nothing
        Just (Cursor c) -> Just (Aeson.object ["cursor" .= c])
  result <- session.sessionRequest "resources/list" params Nothing
  case result of
    Left err -> pure (Left err)
    Right val -> case parseResourcesListResult val of
      Left msg -> pure (Left RPCError
        { rpcErrorCode = -32602
        , rpcErrorMessage = msg
        , rpcErrorData = Nothing
        })
      Right r -> pure (Right r)

parseResourcesListResult :: Aeson.Value -> Either Text ([ClientResourceDef], Maybe Cursor)
parseResourcesListResult val = case val of
  Aeson.Object o -> do
    resources <- case KM.lookup "resources" o of
      Nothing -> Left "Missing 'resources' field in response"
      Just v -> case Aeson.fromJSON v of
        Aeson.Error e -> Left ("Failed to parse resources: " <> T.pack e)
        Aeson.Success rs -> Right rs
    let nextCursor = case KM.lookup "nextCursor" o of
          Just (Aeson.String c) -> Just (Cursor c)
          _ -> Nothing
    Right (resources, nextCursor)
  _ -> Left "resources/list result is not an object"

------------------------------------------------------------------------
-- Reading
------------------------------------------------------------------------

-- | Read a resource from a connected MCP server by URI.
readResource :: Session -> Text -> IO (Either RPCError [ResourceContents])
readResource session uri = do
  let params = Just $ Aeson.object ["uri" .= uri]
  result <- session.sessionRequest "resources/read" params Nothing
  case result of
    Left err -> pure (Left err)
    Right val -> case val of
      Aeson.Object o -> case KM.lookup "contents" o of
        Nothing -> pure (Left RPCError
          { rpcErrorCode = -32602
          , rpcErrorMessage = "Missing 'contents' field in response"
          , rpcErrorData = Nothing
          })
        Just v -> case Aeson.fromJSON v of
          Aeson.Error e -> pure (Left RPCError
            { rpcErrorCode = -32602
            , rpcErrorMessage = T.pack e
            , rpcErrorData = Nothing
            })
          Aeson.Success contents -> pure (Right contents)
      _ -> pure (Left RPCError
        { rpcErrorCode = -32602
        , rpcErrorMessage = "resources/read result is not an object"
        , rpcErrorData = Nothing
        })
