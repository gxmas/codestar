module CodeStar.Tools.MCP
  ( connectMcpEndpoints
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Network.MCP.Client.Tools (ClientToolDef (..), ToolCallResult (..))
import Network.MCP.Client.Tools qualified as MCPTools
import Network.MCP.Session (Session)
import Network.MCP.Session.Connect (defaultConnectConfig)
import Network.MCP.Session.Connect qualified as MCPConnect
import Network.MCP.Transport (Transport)
import Network.MCP.Transport.Http qualified as Http
import Network.MCP.Transport.Stdio qualified as Stdio
import Network.MCP.Types (Implementation (..), RPCError (..))
import Network.MCP.Types.Content
  ( ContentBlock (..)
  , ResourceLink (..)
  , TextContent (..)
  )

import Data.JsonSchema.Schema.Internal (emptySchema)
import Data.JsonSchema.Serialization (decode)

import CodeStar.Config (McpEndpoint (..), McpTransport (..))
import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry

-- --------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------

{- | Connect to all configured MCP endpoints and return ToolHandlerDicts
for each discovered tool. Tools are prefixed with the endpoint name
to avoid name collisions (e.g., "filesystem.read_file").
Endpoints that fail to connect are skipped with a warning.
-}
connectMcpEndpoints :: [McpEndpoint] -> IO [ToolHandlerDict]
connectMcpEndpoints [] = pure []
connectMcpEndpoints endpoints = do
  results <- mapM connectOne endpoints
  pure (concat [hs | Just hs <- results])

-- --------------------------------------------------------------------
-- Per-endpoint connection
-- --------------------------------------------------------------------

connectOne :: McpEndpoint -> IO (Maybe [ToolHandlerDict])
connectOne ep = do
  sessionResult <- openSession ep
  case sessionResult of
    Left err -> do
      putStrLn ("[MCP] Failed to connect to " <> Text.unpack ep.endpointName <> ": " <> err)
      pure Nothing
    Right session -> do
      toolsResult <- MCPTools.listTools session
      case toolsResult of
        Left err -> do
          putStrLn
            ( "[MCP] tools/list failed for "
                <> Text.unpack ep.endpointName
                <> ": "
                <> Text.unpack err.rpcErrorMessage
            )
          pure Nothing
        Right defs -> do
          putStrLn
            ( "[MCP] Connected to "
                <> Text.unpack ep.endpointName
                <> " — "
                <> show (length defs)
                <> " tools discovered"
            )
          handlers <- mapM (mkHandler session ep.endpointName) defs
          pure (Just handlers)

openSession :: McpEndpoint -> IO (Either String Session)
openSession ep = do
  let cfg = defaultConnectConfig codestarImpl
  case ep.transport of
    StdioTransport -> do
      result <-
        try (Stdio.new (Text.unpack ep.command) (map Text.unpack ep.args)) ::
          IO (Either IOException Stdio.StdioTransport)
      case result of
        Left ex -> pure (Left (show ex))
        Right t -> connectSession t cfg
    HttpTransport -> do
      result <- try (Http.new ep.command) :: IO (Either IOException Http.HttpTransport)
      case result of
        Left ex -> pure (Left (show ex))
        Right t -> connectSession t cfg

connectSession ::
  (Transport t) =>
  t ->
  MCPConnect.ConnectConfig ->
  IO (Either String Session)
connectSession transport cfg = do
  result <- MCPConnect.connect transport cfg
  case result of
    Left err -> pure (Left (show err))
    Right session -> pure (Right session)

codestarImpl :: Implementation
codestarImpl =
  Implementation
    { implName = "codestar"
    , implVersion = "0.1.0"
    , implTitle = Nothing
    , implDescription = Nothing
    }

-- --------------------------------------------------------------------
-- Tool handler construction
-- --------------------------------------------------------------------

mkHandler :: Session -> Text -> ClientToolDef -> IO ToolHandlerDict
mkHandler session epName def = do
  let prefixedName = ToolName (epName <> "." <> def.ctdName)
      schema = either (const emptySchema) id (decode def.ctdInputSchema)
      description = maybe def.ctdName id def.ctdDescription
      toolDef =
        ToolDefinition
          { name = prefixedName
          , description = description
          , parameters = schema
          , riskTier = SideEffect
          }
  pure
    ToolHandlerDict
      { definition = toolDef
      , invoke = invokeMcpTool session def.ctdName
      }

invokeMcpTool :: Session -> Text -> ToolInput -> IO (Either ToolError ToolOutput)
invokeMcpTool session toolName input = do
  let args = inputToValue input
  result <- MCPTools.callTool session toolName args
  case result of
    Left err ->
      pure (Left (ExecutionFailed err.rpcErrorMessage))
    Right tcr ->
      let content = foldMap renderBlock tcr.tcrContent
       in if tcr.tcrIsError
            then pure (Left (ExecutionFailed content))
            else pure (Right ToolOutput{content = content, truncated = False})

inputToValue :: ToolInput -> Value
inputToValue input =
  Object (KM.fromList [(Key.fromText k, v) | (k, v) <- Map.toList input.arguments])

renderBlock :: ContentBlock -> Text
renderBlock (ContentText tc) = tc.textValue
renderBlock (ContentImage _) = "[image]"
renderBlock (ContentAudio _) = "[audio]"
renderBlock (ContentLink rl) = "[link: " <> rl.linkName <> "]"
renderBlock (ContentEmbedded _) = "[resource]"
