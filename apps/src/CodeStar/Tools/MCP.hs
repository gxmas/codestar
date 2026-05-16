module CodeStar.Tools.MCP
  ( connectMcpEndpoints
  ) where

import Control.Exception (IOException, try)
import GHC.Clock (getMonotonicTimeNSec)
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
import Network.MCP.Types.Content (ContentBlock (..), ResourceLink (..), TextContent (..))

import OTel.Attribute (AttributeValue (..))
import OTel.Log
  ( getGlobalLoggerProvider
  , getLogger
  , defaultLogRecord
  , LogBody (..)
  , SeverityNumber (..)
  , LogRecord (..)
  , emit
  )
import OTel.Attribute qualified as OTelAttr
import OTel.Context (getCurrent)

import Data.JsonSchema.Schema.Internal (emptySchema)
import Data.JsonSchema.Serialization (decode)

import CodeStar.Config (McpEndpoint (..), McpTransport (..))
import CodeStar.LLM.Base (ToolName (..))
import OTel.Attribute (InstrumentationScope (..))
import CodeStar.Telemetry (TelemetryRecorder (..))
import CodeStar.Telemetry qualified as Tel
import CodeStar.Tools.Registry

-- --------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------

{- | Connect to all configured MCP endpoints and return ToolHandlerDicts
for each discovered tool. Tools are prefixed with the endpoint name
to avoid name collisions (e.g., "filesystem.read_file").
Endpoints that fail to connect are skipped with a warning.
-}
connectMcpEndpoints :: TelemetryRecorder -> [McpEndpoint] -> IO [ToolHandlerDict]
connectMcpEndpoints _ [] = pure []
connectMcpEndpoints tel endpoints = do
  results <- mapM (connectOne tel) endpoints
  pure (concat [hs | Just hs <- results])

-- --------------------------------------------------------------------
-- Per-endpoint connection
-- --------------------------------------------------------------------

connectOne :: TelemetryRecorder -> McpEndpoint -> IO (Maybe [ToolHandlerDict])
connectOne tel ep = do
  sessionResult <- openSession ep
  case sessionResult of
    Left err -> do
      logMcp SeverityWarn "mcp.connect.failed"
        [ ("mcp.endpoint",  StringValue ep.endpointName)
        , ("error.message", StringValue (Text.pack err))
        ]
      pure Nothing
    Right session -> do
      toolsResult <- MCPTools.listTools session
      case toolsResult of
        Left err -> do
          logMcp SeverityWarn "mcp.connect.failed"
            [ ("mcp.endpoint",  StringValue ep.endpointName)
            , ("error.message", StringValue err.rpcErrorMessage)
            ]
          pure Nothing
        Right defs -> do
          logMcp SeverityInfo "mcp.connect.success"
            [ ("mcp.endpoint",  StringValue ep.endpointName)
            , ("tool_count",    Int64Value (fromIntegral (length defs)))
            ]
          handlers <- mapM (mkHandler tel session ep.endpointName) defs
          pure (Just handlers)

logMcp :: SeverityNumber -> Text -> [(Text, AttributeValue)] -> IO ()
logMcp severity body attrs = do
  let scope = InstrumentationScope
        { scopeName = "codestar"
        , scopeVersion = Nothing
        , scopeSchemaUrl = Nothing
        , scopeAttributes = Nothing
        }
  loggerProvider <- getGlobalLoggerProvider
  logger         <- getLogger loggerProvider scope
  ctx            <- getCurrent
  emit logger defaultLogRecord
    { logSeverityNumber = Just severity
    , logBody           = Just (LogBodyString body)
    , logAttributes     = OTelAttr.fromList attrs
    , logContext        = Just ctx
    }

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

mkHandler :: TelemetryRecorder -> Session -> Text -> ClientToolDef -> IO ToolHandlerDict
mkHandler tel session epName def = do
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
      , invoke = invokeMcpTool tel session epName def.ctdName
      }

invokeMcpTool :: TelemetryRecorder -> Session -> Text -> Text -> ToolInput -> IO (Either ToolError ToolOutput)
invokeMcpTool tel session epName toolName input = do
  let args = inputToValue input
  mcpSpan <- tel.startSpan "mcp.tool_call"
    [ ("mcp.endpoint",  StringValue epName)
    , ("mcp.tool_name", StringValue toolName)
    ]
  t0 <- getMonotonicTimeNSec
  result <- MCPTools.callTool session toolName args
  t1 <- getMonotonicTimeNSec
  let durMs = fromIntegral ((t1 - t0) `div` 1_000_000) :: Double
  case result of
    Left err -> do
      tel.setSpanError mcpSpan err.rpcErrorMessage
      tel.endSpan mcpSpan
      tel.recordEvent Tel.EvMcpCall
        { Tel.mcEndpoint  = epName
        , Tel.mcToolName  = toolName
        , Tel.mcDurationMs = durMs
        , Tel.mcSuccess   = False
        }
      pure (Left (ExecutionFailed err.rpcErrorMessage))
    Right tcr -> do
      let content = foldMap renderBlock tcr.tcrContent
          isErr = tcr.tcrIsError
      tel.endSpan mcpSpan
      tel.recordEvent Tel.EvMcpCall
        { Tel.mcEndpoint  = epName
        , Tel.mcToolName  = toolName
        , Tel.mcDurationMs = durMs
        , Tel.mcSuccess   = not isErr
        }
      if isErr
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
