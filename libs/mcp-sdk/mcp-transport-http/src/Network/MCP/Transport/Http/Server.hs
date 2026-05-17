-- |
-- Module      : Network.MCP.Transport.Http.Server
-- Stability   : experimental
--
-- Server-side WAI 'Application' for the MCP Streamable HTTP transport
-- (spec 2025-03-26). Manages client sessions, handles the initialize
-- handshake, and provides SSE streaming for server-initiated messages.
module Network.MCP.Transport.Http.Server
  ( -- * Configuration
    McpServerConfig (..)
  , defaultMcpServerConfig

    -- * Server
  , McpServer
  , newMcpServer
  , mcpApp
  ) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM
  ( TMVar, TQueue, TVar
  , atomically, newEmptyTMVarIO, newTQueueIO, newTVarIO
  , modifyTVar', putTMVar, readTVar, readTVarIO, readTQueue, takeTMVar
  , writeTQueue, writeTVar
  )
import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, Request, Response)
import qualified Network.Wai as Wai

import Network.MCP.Session
  ( CloseReason (..)
  , NotificationHandler
  , RequestHandler
  , RequestMeta (..)
  , Session (..)
  )
import Network.MCP.Types
  ( Implementation (..)
  , JSONRPCError (..)
  , JSONRPCNotification (..)
  , JSONRPCRequest (..)
  , JSONRPCResult (..)
  , MCPMessage (..)
  , ProtocolVersion (..)
  , RPCError (..)
  , RequestId (..)
  )
import Network.MCP.Types.Capabilities
  ( ClientCapabilities
  , NegotiatedCapabilities (..)
  , ServerCapabilities (..)
  )

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for the server-side Streamable HTTP transport.
data McpServerConfig = McpServerConfig
  { mscServerInfo         :: !Implementation
    -- ^ Server identity returned in initialize responses.
  , mscServerCapabilities :: !ServerCapabilities
    -- ^ Capabilities advertised by this server.
  , mscSupportedVersions  :: ![Text]
    -- ^ Protocol versions this server accepts.
  , mscInstructions       :: !(Maybe Text)
    -- ^ Optional human-readable instructions returned at initialize.
  , mscAllowedOrigins     :: !(Maybe [Text])
    -- ^ Allowed values of the Origin header. Nothing = allow all.
  , mscSupportGetStream   :: !Bool
    -- ^ Whether the server supports GET SSE for server-initiated messages.
  , mscVerifyToken        :: !(Maybe (Text -> IO Bool))
    -- ^ Optional bearer token validator. When set, POST/GET/DELETE
    -- requests (after initialize) must include Authorization: Bearer <token>.
    -- The callback returns True if the token is valid.
  , mscOnSession          :: Session -> IO ()
    -- ^ Called when a new client session is fully initialized.
  }

-- | Default server config. Accepts all origins, supports GET stream,
-- no-op session callback. Caller must set 'mscServerInfo'.
defaultMcpServerConfig :: Implementation -> McpServerConfig
defaultMcpServerConfig info = McpServerConfig
  { mscServerInfo         = info
  , mscServerCapabilities = ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , mscSupportedVersions  = ["2025-03-26", "2024-11-05"]
  , mscInstructions       = Nothing
  , mscAllowedOrigins     = Nothing
  , mscSupportGetStream   = True
  , mscVerifyToken        = Nothing
  , mscOnSession          = \_ -> pure ()
  }

------------------------------------------------------------------------
-- Server handle
------------------------------------------------------------------------

-- | Opaque server handle. Create with 'newMcpServer'.
data McpServer = McpServer
  { msSessions :: !(TVar (HashMap Text ServerSession))
  , msConfig   :: !McpServerConfig
  }

------------------------------------------------------------------------
-- Per-session state (internal)
------------------------------------------------------------------------

data ServerSession = ServerSession
  { ssId               :: !Text
  , ssNotifyQueue      :: !(TQueue (Either () MCPMessage))
    -- ^ Left () = close signal for SSE; Right msg = push to client
  , ssRequestHandlers  :: !(TVar (HashMap Text RequestHandler))
  , ssNotifHandlers    :: !(TVar (HashMap Text NotificationHandler))
  , ssPendingResults   :: !(TVar (HashMap RequestId (TMVar (Either RPCError Aeson.Value))))
    -- ^ Server-initiated requests awaiting client response via POST
  , ssNextId           :: !(IORef Int)
  , ssClosed           :: !(TVar Bool)
  , ssCloseCallbacks   :: !(TVar [CloseReason -> IO ()])
  }

------------------------------------------------------------------------
-- Initialize params
------------------------------------------------------------------------

data InitParams = InitParams
  { ipProtocolVersion :: !ProtocolVersion
  , ipCapabilities    :: !ClientCapabilities
  , ipClientInfo      :: !Implementation
  }

instance Aeson.FromJSON InitParams where
  parseJSON = Aeson.withObject "InitParams" $ \o ->
    InitParams
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "clientInfo"

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Create a new server handle.
newMcpServer :: McpServerConfig -> IO McpServer
newMcpServer cfg = do
  sessions <- newTVarIO HM.empty
  pure McpServer { msSessions = sessions, msConfig = cfg }

newServerSession :: Text -> IO ServerSession
newServerSession sid = do
  q    <- newTQueueIO
  rh   <- newTVarIO HM.empty
  nh   <- newTVarIO HM.empty
  pr   <- newTVarIO HM.empty
  nid  <- newIORef 1
  cl   <- newTVarIO False
  cbs  <- newTVarIO []
  pure ServerSession
    { ssId               = sid
    , ssNotifyQueue      = q
    , ssRequestHandlers  = rh
    , ssNotifHandlers    = nh
    , ssPendingResults   = pr
    , ssNextId           = nid
    , ssClosed           = cl
    , ssCloseCallbacks   = cbs
    }

generateSessionId :: IO Text
generateSessionId = T.pack . show <$> UUID.nextRandom

------------------------------------------------------------------------
-- Session builder
------------------------------------------------------------------------

buildServerSession
  :: ServerSession
  -> ProtocolVersion      -- negotiated version
  -> Implementation       -- client info (peer)
  -> NegotiatedCapabilities
  -> Maybe Text           -- instructions
  -> Session
buildServerSession ss pv peerInfo caps mInstr = Session
  { sessionProtocolVersion = pv
  , sessionPeerInfo        = peerInfo
  , sessionCapabilities    = caps
  , sessionInstructions    = mInstr

  , sessionRequest = \method params _opts -> do
      isClosed <- readTVarIO ss.ssClosed
      if isClosed
        then pure (Left RPCError
          { rpcErrorCode = -32600, rpcErrorMessage = "Session closed", rpcErrorData = Nothing })
        else do
          n <- atomicModifyIORef' ss.ssNextId (\i -> (i + 1, i))
          let reqId  = RequestId (Right n)
          resultVar <- newEmptyTMVarIO
          let req = MCPRequest JSONRPCRequest
                { requestId = reqId, requestMethod = method
                , requestParams = params, requestMeta = Nothing }
          atomically $ do
            modifyTVar' ss.ssPendingResults (HM.insert reqId resultVar)
            writeTQueue ss.ssNotifyQueue (Right req)
          atomically (takeTMVar resultVar)

  , sessionNotify = \method params -> do
      let notif = MCPNotification JSONRPCNotification
            { notificationMethod = method, notificationParams = params, notificationMeta = Nothing }
      atomically (writeTQueue ss.ssNotifyQueue (Right notif))

  , sessionCancel = \reqId reason -> do
      let ps = Aeson.object $
            ["requestId" .= reqId] ++ maybe [] (\r -> ["reason" .= r]) reason
      let notif = MCPNotification JSONRPCNotification
            { notificationMethod = "notifications/cancelled"
            , notificationParams = Just ps, notificationMeta = Nothing }
      atomically (writeTQueue ss.ssNotifyQueue (Right notif))

  , sessionOnRequest = \method handler ->
      atomically $ modifyTVar' ss.ssRequestHandlers (HM.insert method handler)

  , sessionOnNotification = \method handler ->
      atomically $ modifyTVar' ss.ssNotifHandlers (HM.insert method handler)

  , sessionClose = do
      alreadyClosed <- atomically $ do
        c <- readTVar ss.ssClosed
        writeTVar ss.ssClosed True
        pure c
      unless alreadyClosed $ do
        atomically (writeTQueue ss.ssNotifyQueue (Left ()))
        cbs <- readTVarIO ss.ssCloseCallbacks
        mapM_ (\cb -> cb LocalClose `catch` (\(_ :: SomeException) -> pure ())) cbs

  , sessionOnClose = \cb ->
      atomically $ modifyTVar' ss.ssCloseCallbacks (cb :)
  }

------------------------------------------------------------------------
-- Bearer token verification
------------------------------------------------------------------------

-- | Extract and verify a bearer token from the Authorization header.
-- Returns True if no verification is configured, or if the token passes.
-- Returns False if verification is configured but token is missing or invalid.
checkBearer :: McpServerConfig -> Request -> IO Bool
checkBearer cfg req = case cfg.mscVerifyToken of
  Nothing -> pure True
  Just verify ->
    case lookup "Authorization" (Wai.requestHeaders req) of
      Nothing  -> pure False
      Just hdr ->
        case T.stripPrefix "Bearer " (TE.decodeUtf8Lenient hdr) of
          Nothing  -> pure False
          Just tok -> verify tok

------------------------------------------------------------------------
-- WAI Application
------------------------------------------------------------------------

-- | Build a WAI 'Application' for the MCP Streamable HTTP server.
mcpApp :: McpServer -> Application
mcpApp ms req respond
  | not (validateOrigin ms.msConfig req) =
      respond (Wai.responseLBS HTTP.forbidden403 [] "Forbidden")
  | Wai.requestMethod req == "POST"   = handlePost ms req respond
  | Wai.requestMethod req == "GET"    = handleGet  ms req respond
  | Wai.requestMethod req == "DELETE" = handleDelete ms req respond
  | otherwise = respond (Wai.responseLBS HTTP.methodNotAllowed405 [] "")

-- | Validate the Origin header per the MCP spec MUST to prevent DNS rebinding.
-- When an allowlist is configured, requests without an Origin header are also
-- rejected — Origin is required so the server can enforce the allowlist.
-- When no allowlist is configured (Nothing), all origins are accepted (caller
-- has disabled origin validation; appropriate for trusted environments only).
validateOrigin :: McpServerConfig -> Request -> Bool
validateOrigin cfg r = case cfg.mscAllowedOrigins of
  Nothing      -> True  -- origin validation disabled by caller
  Just allowed ->
    case lookup "Origin" (Wai.requestHeaders r) of
      Nothing  -> False  -- allowlist configured: require Origin header
      Just org -> TE.decodeUtf8Lenient org `elem` allowed

sessionIdHdr :: Request -> Maybe Text
sessionIdHdr r =
  TE.decodeUtf8Lenient <$> lookup "Mcp-Session-Id" (Wai.requestHeaders r)

------------------------------------------------------------------------
-- POST handler
------------------------------------------------------------------------

handlePost :: McpServer -> Request -> (Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handlePost ms r respond = do
  body <- Wai.lazyRequestBody r
  case Aeson.eitherDecode' body :: Either String MCPMessage of
    Left err ->
      respond (Wai.responseLBS HTTP.badRequest400 [] (LBS.fromStrict (TE.encodeUtf8 (T.pack err))))
    Right msg -> do
      let mSid = sessionIdHdr r
      case (msg, mSid) of
        (MCPRequest req_, Nothing)
          | req_.requestMethod == "initialize" ->
              handleInitialize ms req_ respond
        _ ->
          case mSid of
            Nothing ->
              respond (Wai.responseLBS HTTP.badRequest400 [] "Missing Mcp-Session-Id")
            Just sid -> do
              authorized <- checkBearer ms.msConfig r
              if not authorized
                then respond (Wai.responseLBS HTTP.unauthorized401 [] "Unauthorized")
                else do
                  mSs <- HM.lookup sid <$> readTVarIO ms.msSessions
                  case mSs of
                    Nothing -> respond (Wai.responseLBS HTTP.notFound404 [] "Session not found")
                    Just ss -> dispatchToSession ss msg respond

------------------------------------------------------------------------
-- Initialize handler
------------------------------------------------------------------------

handleInitialize
  :: McpServer
  -> JSONRPCRequest
  -> (Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
handleInitialize ms initReq respond =
  case Aeson.fromJSON (fromMaybe Aeson.Null initReq.requestParams) of
    Aeson.Error err ->
      respond (Wai.responseLBS HTTP.badRequest400 []
        (LBS.fromStrict (TE.encodeUtf8 ("Invalid initialize params: " <> T.pack err))))
    Aeson.Success (ip :: InitParams) -> do
      let ProtocolVersion cv = ip.ipProtocolVersion
      let supported = ms.msConfig.mscSupportedVersions
      -- Per spec: server MUST respond with a supported version (not an error).
      -- If client requests an unsupported version, respond with the server's
      -- preferred version; the client SHOULD disconnect if it can't support it.
      let negotiatedVersion =
            if cv `elem` supported
              then ip.ipProtocolVersion          -- echo the client's version
              else case supported of
                (v:_) -> ProtocolVersion v       -- use server's preferred
                []    -> ip.ipProtocolVersion    -- no supported versions — echo client
      do
          sid <- generateSessionId
          ss  <- newServerSession sid

          let caps = NegotiatedCapabilities
                { negClient = ip.ipCapabilities
                , negServer = ms.msConfig.mscServerCapabilities
                }
          let session = buildServerSession ss negotiatedVersion
                          ip.ipClientInfo caps ms.msConfig.mscInstructions

          atomically $ modifyTVar' ms.msSessions (HM.insert sid ss)

          -- Let application code register handlers asynchronously
          _ <- async (ms.msConfig.mscOnSession session)

          -- Build and return the initialize result
          let resultVal = Aeson.object $
                [ "protocolVersion" .= negotiatedVersion
                , "capabilities"    .= ms.msConfig.mscServerCapabilities
                , "serverInfo"      .= ms.msConfig.mscServerInfo
                ] ++ maybe [] (\i -> ["instructions" .= i]) ms.msConfig.mscInstructions
          let resultBody = Aeson.encode JSONRPCResult
                { resultId     = initReq.requestId
                , resultResult = resultVal
                , resultMeta   = Nothing
                }
          respond $ Wai.responseLBS HTTP.ok200
            [ ("Content-Type", "application/json")
            , ("Mcp-Session-Id", TE.encodeUtf8 sid)
            ]
            resultBody

------------------------------------------------------------------------
-- Dispatch handler
------------------------------------------------------------------------

dispatchToSession
  :: ServerSession
  -> MCPMessage
  -> (Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
dispatchToSession ss msg respond = case msg of

  MCPRequest req -> do
    handlers <- readTVarIO ss.ssRequestHandlers
    let meta = RequestMeta
          { requestMetaId            = req.requestId
          , requestMetaProgressToken = Nothing
          , requestMetaRelatedTask   = Nothing
          }
    result <- case HM.lookup req.requestMethod handlers of
      Nothing -> pure (Left RPCError
        { rpcErrorCode    = -32601
        , rpcErrorMessage = "Method not found: " <> req.requestMethod
        , rpcErrorData    = Nothing
        })
      Just handler ->
        handler (fromMaybe Aeson.Null req.requestParams) meta
          `catch` (\(e :: SomeException) -> pure (Left RPCError
            { rpcErrorCode = -32603, rpcErrorMessage = T.pack (show e), rpcErrorData = Nothing }))
    let body = case result of
          Left rpcErr -> Aeson.encode JSONRPCError
            { errorId    = Just req.requestId
            , errorError = rpcErr
            }
          Right val -> Aeson.encode JSONRPCResult
            { resultId     = req.requestId
            , resultResult = val
            , resultMeta   = Nothing
            }
    respond (Wai.responseLBS HTTP.ok200 [("Content-Type", "application/json")] body)

  MCPNotification notif -> do
    handlers <- readTVarIO ss.ssNotifHandlers
    case HM.lookup notif.notificationMethod handlers of
      Nothing      -> pure ()
      Just handler ->
        handler (fromMaybe Aeson.Null notif.notificationParams)
          `catch` (\(_ :: SomeException) -> pure ())
    respond (Wai.responseLBS HTTP.accepted202 [] "")

  MCPResult res -> do
    mVar <- atomically $ do
      pending <- readTVar ss.ssPendingResults
      case HM.lookup res.resultId pending of
        Nothing -> pure Nothing
        Just v  -> do
          modifyTVar' ss.ssPendingResults (HM.delete res.resultId)
          pure (Just v)
    case mVar of
      Nothing -> pure ()
      Just v  -> atomically (putTMVar v (Right res.resultResult))
    respond (Wai.responseLBS HTTP.accepted202 [] "")

  MCPError err -> do
    case err.errorId of
      Nothing    -> pure ()
      Just reqId -> do
        mVar <- atomically $ do
          pending <- readTVar ss.ssPendingResults
          case HM.lookup reqId pending of
            Nothing -> pure Nothing
            Just v  -> do
              modifyTVar' ss.ssPendingResults (HM.delete reqId)
              pure (Just v)
        case mVar of
          Nothing -> pure ()
          Just v  -> atomically (putTMVar v (Left err.errorError))
    respond (Wai.responseLBS HTTP.accepted202 [] "")

------------------------------------------------------------------------
-- GET handler (SSE stream)
------------------------------------------------------------------------

handleGet :: McpServer -> Request -> (Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleGet ms r respond
  | not ms.msConfig.mscSupportGetStream =
      respond (Wai.responseLBS HTTP.methodNotAllowed405 [] "GET SSE not supported")
  | otherwise = do
      authorized <- checkBearer ms.msConfig r
      if not authorized
        then respond (Wai.responseLBS HTTP.unauthorized401 [] "Unauthorized")
        else case sessionIdHdr r of
        Nothing  -> respond (Wai.responseLBS HTTP.badRequest400 [] "Missing Mcp-Session-Id")
        Just sid -> do
          mSs <- HM.lookup sid <$> readTVarIO ms.msSessions
          case mSs of
            Nothing -> respond (Wai.responseLBS HTTP.notFound404 [] "Session not found")
            Just ss -> respond $ Wai.responseStream HTTP.ok200
              [ ("Content-Type",  "text/event-stream")
              , ("Cache-Control", "no-cache")
              , ("Connection",    "keep-alive")
              ]
              (sseStream ss)

sseStream :: ServerSession -> (Builder.Builder -> IO ()) -> IO () -> IO ()
sseStream ss write flush = go
  where
    go = do
      item <- atomically (readTQueue ss.ssNotifyQueue)
      case item of
        Left  ()  -> pure ()   -- session closed; end the stream
        Right msg -> do
          let encoded = LBS.toStrict (Aeson.encode msg)
          write
            $  Builder.byteString "event: message\ndata: "
            <> Builder.byteString encoded
            <> Builder.byteString "\n\n"
          flush
          go

------------------------------------------------------------------------
-- DELETE handler
------------------------------------------------------------------------

handleDelete :: McpServer -> Request -> (Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleDelete ms r respond = do
  authorized <- checkBearer ms.msConfig r
  if not authorized
    then respond (Wai.responseLBS HTTP.unauthorized401 [] "Unauthorized")
    else case sessionIdHdr r of
    Nothing  -> respond (Wai.responseLBS HTTP.badRequest400 [] "Missing Mcp-Session-Id")
    Just sid -> do
      mSs <- atomically $ do
        sessions <- readTVar ms.msSessions
        case HM.lookup sid sessions of
          Nothing -> pure Nothing
          Just s  -> do
            writeTVar ms.msSessions (HM.delete sid sessions)
            pure (Just s)
      case mSs of
        Nothing -> respond (Wai.responseLBS HTTP.notFound404 [] "Session not found")
        Just ss -> do
          atomically $ do
            writeTVar  ss.ssClosed True
            writeTQueue ss.ssNotifyQueue (Left ())
          cbs <- readTVarIO ss.ssCloseCallbacks
          mapM_ (\cb -> cb RemoteClose `catch` (\(_ :: SomeException) -> pure ())) cbs
          respond (Wai.responseLBS HTTP.ok200 [] "")
