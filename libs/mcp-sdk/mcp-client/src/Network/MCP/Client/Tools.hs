-- |
-- Module      : Network.MCP.Client.Tools
-- Stability   : experimental
--
-- Client-side tool discovery and invocation. Sends @tools/list@ and
-- @tools/call@ requests over a connected 'Session'.
module Network.MCP.Client.Tools
  ( -- * Types
    ClientToolDef (..)
  , ToolCallResult (..)

    -- * Discovery
  , listTools
  , listToolsPage

    -- * Invocation
  , callTool
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Session (Session (..))
import Network.MCP.Types (Cursor (..), RPCError (..))
import Network.MCP.Types.Content (ContentBlock)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A tool advertised by an MCP server, as seen from the client side.
data ClientToolDef = ClientToolDef
  { ctdName        :: !Text
  , ctdDescription :: !(Maybe Text)
  , ctdInputSchema :: !Aeson.Value
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON ClientToolDef where
  parseJSON = Aeson.withObject "ClientToolDef" $ \o ->
    ClientToolDef
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .: "inputSchema"

-- | Result of calling a tool on an MCP server.
data ToolCallResult = ToolCallResult
  { tcrContent :: ![ContentBlock]
  , tcrIsError :: !Bool
  }
  deriving stock (Eq, Show)

instance Aeson.FromJSON ToolCallResult where
  parseJSON = Aeson.withObject "ToolCallResult" $ \o ->
    ToolCallResult
      <$> o .: "content"
      <*> (maybe False id <$> o .:? "isError")

------------------------------------------------------------------------
-- Discovery
------------------------------------------------------------------------

-- | List all tools from a connected server, driving pagination
-- automatically.
listTools :: Session -> IO (Either RPCError [ClientToolDef])
listTools session = go Nothing []
  where
    go cursor acc = do
      result <- listToolsPage session cursor
      case result of
        Left err -> pure (Left err)
        Right (tools, Nothing) -> pure (Right (acc ++ tools))
        Right (tools, Just nextCursor) -> go (Just nextCursor) (acc ++ tools)

-- | List a single page of tools from a connected server.
listToolsPage
  :: Session
  -> Maybe Cursor
  -> IO (Either RPCError ([ClientToolDef], Maybe Cursor))
listToolsPage session cursor = do
  let params = case cursor of
        Nothing -> Nothing
        Just (Cursor c) -> Just (Aeson.object ["cursor" .= c])
  result <- session.sessionRequest "tools/list" params Nothing
  case result of
    Left err -> pure (Left err)
    Right val -> case parseToolsListResult val of
      Left msg -> pure (Left RPCError
        { rpcErrorCode = -32602
        , rpcErrorMessage = msg
        , rpcErrorData = Nothing
        })
      Right r -> pure (Right r)

parseToolsListResult :: Aeson.Value -> Either Text ([ClientToolDef], Maybe Cursor)
parseToolsListResult val = case val of
  Aeson.Object o -> do
    tools <- case KM.lookup "tools" o of
      Nothing -> Left "Missing 'tools' field in response"
      Just v -> case Aeson.fromJSON v of
        Aeson.Error e -> Left ("Failed to parse tools: " <> toText e)
        Aeson.Success ts -> Right ts
    let nextCursor = case KM.lookup "nextCursor" o of
          Just (Aeson.String c) -> Just (Cursor c)
          _ -> Nothing
    Right (tools, nextCursor)
  _ -> Left "tools/list result is not an object"

------------------------------------------------------------------------
-- Invocation
------------------------------------------------------------------------

-- | Call a tool on a connected MCP server.
callTool :: Session -> Text -> Aeson.Value -> IO (Either RPCError ToolCallResult)
callTool session toolName args = do
  let params = Just $ Aeson.object
        [ "name" .= toolName
        , "arguments" .= args
        ]
  result <- session.sessionRequest "tools/call" params Nothing
  case result of
    Left err -> pure (Left err)
    Right val -> case Aeson.fromJSON val of
      Aeson.Error e -> pure (Left RPCError
        { rpcErrorCode = -32602
        , rpcErrorMessage = toText e
        , rpcErrorData = Nothing
        })
      Aeson.Success r -> pure (Right r)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

toText :: String -> Text
toText = T.pack
{-# INLINE toText #-}
