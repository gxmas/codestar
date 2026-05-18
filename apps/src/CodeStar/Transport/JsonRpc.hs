{- | JSON-RPC 2.0 transport for the codestar agent protocol.
Uses the json-rpc-sdk for framing and dispatch.
The byte source/sink is abstracted so the same logic works over
stdio (for testing) or WebSocket (for production server use).
-}
module CodeStar.Transport.JsonRpc
  ( -- * Transport construction
    jsonRpcTransport
  , stdioTransport

    -- * Notification encoding
  , encodeNotification

    -- * Method names
  , sessionStartMethod
  , sessionRespondMethod
  , sessionApproveMethod
  , sessionRejectMethod
  , sessionCompactMethod
  , sessionStopMethod
  , sessionSetModelMethod
  ) where

import Data.Aeson (Value (..), encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE

import Network.JsonRpc qualified as RPC

import CodeStar.AgentLoop (AgentEvent (..))
import CodeStar.Transport.Types
  ( AgentEventEnvelope (..)
  , AgentTransportDict (..)
  , Command (..)
  , CommandResult (..)
  )
import CodeStar.Types (SessionId (..))

-- --------------------------------------------------------------------
-- Method name constants
-- --------------------------------------------------------------------

sessionStartMethod
  , sessionRespondMethod
  , sessionApproveMethod
  , sessionRejectMethod
  , sessionCompactMethod
  , sessionStopMethod
  , sessionSetModelMethod ::
    Text
sessionStartMethod = "session.start"
sessionRespondMethod = "session.respond"
sessionApproveMethod = "session.approve"
sessionRejectMethod = "session.reject"
sessionCompactMethod = "session.compact"
sessionStopMethod = "session.stop"
sessionSetModelMethod = "session.setModel"

-- --------------------------------------------------------------------
-- Transport construction
-- --------------------------------------------------------------------

{- | Build an AgentTransportDict backed by custom send/receive functions.
'sendBytes' is called for each outbound JSON-RPC notification.
'recvBytes' blocks until a JSON-RPC message arrives.
-}
jsonRpcTransport ::
  -- | send raw bytes to the peer
  (BL.ByteString -> IO ()) ->
  -- | receive raw bytes; Nothing = EOF
  IO (Maybe BL.ByteString) ->
  IO AgentTransportDict
jsonRpcTransport sendBytes recvBytes = do
  handlerRef <- newIORef (const (pure CmdOk) :: Command -> IO CommandResult)
  let server = buildServer handlerRef

  pure
    AgentTransportDict
      { sendEvent = \env -> sendBytes (encodeNotification env)
      , onCommand = \h -> atomicModifyIORef' handlerRef (\_ -> (h, ()))
      , listen = listenLoop server sendBytes recvBytes
      , shutdown = pure ()
      }

{- | Build an AgentTransportDict using stdin/stdout.
Useful for testing and for IDE extension protocols.
-}
stdioTransport :: IO AgentTransportDict
stdioTransport = jsonRpcTransport sendStdout recvStdin
 where
  sendStdout bs = BL.putStr bs >> putStrLn ""
  recvStdin = do
    line <- getLine
    if null line
      then pure Nothing
      else pure (Just (BL.fromStrict (TE.encodeUtf8 (Text.pack line))))

-- --------------------------------------------------------------------
-- Server dispatch
-- --------------------------------------------------------------------

buildServer :: IORef (Command -> IO CommandResult) -> RPC.Server IO
buildServer handlerRef =
  RPC.mkServer
    [ RPC.method
        sessionStartMethod
        ( (\sid task -> CmdStart (SessionId sid) task)
            <$> RPC.param "sessionId"
            <*> RPC.param "task"
        )
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionRespondMethod
        ( (\sid resp -> CmdRespond (SessionId sid) resp)
            <$> RPC.param "sessionId"
            <*> RPC.param "response"
        )
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionApproveMethod
        (CmdApprove . SessionId <$> RPC.param "sessionId")
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionRejectMethod
        ( (\sid reason -> CmdReject (SessionId sid) reason)
            <$> RPC.param "sessionId"
            <*> RPC.param "reason"
        )
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionCompactMethod
        ( (\sid ins -> CmdCompact (SessionId sid) ins)
            <$> RPC.param "sessionId"
            <*> RPC.optParam "instruction" Nothing
        )
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionStopMethod
        (CmdStop . SessionId <$> RPC.param "sessionId")
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    , RPC.method
        sessionSetModelMethod
        ( (\sid m -> CmdSetModel (SessionId sid) m)
            <$> RPC.param "sessionId"
            <*> RPC.param "model"
        )
        $ \cmd -> do
          h <- readIORef handlerRef
          r <- h cmd
          pure (Right (commandResultText r))
    ]

-- --------------------------------------------------------------------
-- Listen loop
-- --------------------------------------------------------------------

listenLoop ::
  RPC.Server IO ->
  (BL.ByteString -> IO ()) ->
  IO (Maybe BL.ByteString) ->
  IO ()
listenLoop server sendBytes recvBytes = go
 where
  go = do
    mbs <- recvBytes
    case mbs of
      Nothing -> pure ()
      Just bs -> do
        response <- RPC.handleRaw server RPC.defaultServerConfig bs
        mapM_ sendBytes response
        go

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

commandResultText :: CommandResult -> Text
commandResultText CmdOk = "ok"
commandResultText (CmdErr msg) = "error: " <> msg

-- | Encode an AgentEventEnvelope as a JSON-RPC notification.
encodeNotification :: AgentEventEnvelope -> BL.ByteString
encodeNotification env =
  encode
    ( RPC.Notification
        { RPC.notificationMethod = agentEventMethod env.envEvent
        , RPC.notificationParams = eventToParams env
        }
    )

eventToParams :: AgentEventEnvelope -> RPC.Params
eventToParams env = case toJSON env of
  Object km -> RPC.ParamsByName km
  _ -> RPC.NoParams

agentEventMethod :: AgentEvent -> Text
agentEventMethod = \case
  AgentToken{} -> "agent.token"
  AgentToolCall{} -> "agent.tool_call"
  AgentToolResult{} -> "agent.tool_result"
  AgentApprovalRequired{} -> "agent.approval_required"
  AgentCompacting -> "agent.compacting"
  AgentProgress{} -> "agent.progress"
  AgentCostUpdate{} -> "agent.cost_update"
  AgentDone{} -> "agent.done"
  AgentError{} -> "agent.error"
  AgentModelChanged{} -> "agent.modelChanged"
