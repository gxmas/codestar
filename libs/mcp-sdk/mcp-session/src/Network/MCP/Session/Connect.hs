-- |
-- Module      : Network.MCP.Session.Connect
-- Stability   : experimental
--
-- Client-side session connection factory. Performs the MCP initialize
-- handshake over a 'Transport' and returns a live 'Session'.
module Network.MCP.Session.Connect
  ( -- * Configuration
    ConnectConfig (..)
  , defaultConnectConfig

    -- * Connection
  , connect
  , disconnect

    -- * Utilities
  , sessionPing
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TMVar, TVar
  , atomically, newEmptyTMVarIO, newTVarIO
  , putTMVar, readTVar, readTVarIO, takeTMVar, writeTVar, modifyTVar'
  )
import Control.Exception (SomeException, catch)
import System.Timeout (timeout)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Streaming.Prelude as SP

import Network.MCP.Session
  ( CloseReason (..)
  , Session (..)
  , SessionError (..)
  , SessionErrorKind (..)
  , RequestHandler
  , NotificationHandler
  , RequestMeta (..)
  , RequestOptions (..)
  )
import Network.MCP.Transport (Transport (..))
import qualified Network.MCP.Transport as Tr
import Network.MCP.Types
  ( Implementation (..)
  , JSONRPCError (..)
  , JSONRPCNotification (..)
  , JSONRPCRequest (..)
  , JSONRPCResult (..)
  , MCPMessage (..)
  , RPCError (..)
  , ProtocolVersion (..)
  , RequestId (..)
  , ProgressToken
  )
import Network.MCP.Types.Capabilities
  ( ClientCapabilities (..)
  , NegotiatedCapabilities (..)
  , ServerCapabilities (..)
  )

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for establishing an MCP client session.
data ConnectConfig = ConnectConfig
  { connectClientInfo :: !Implementation
  , connectClientCapabilities :: !ClientCapabilities
  , connectProtocolVersion :: !ProtocolVersion
  , connectTimeoutMs :: !(Maybe Word)
  , connectSupportedVersions :: ![Text]
  }

-- | Default connect configuration with empty capabilities and the
-- latest protocol version.
defaultConnectConfig :: Implementation -> ConnectConfig
defaultConnectConfig info = ConnectConfig
  { connectClientInfo = info
  , connectClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing Nothing
  , connectProtocolVersion = ProtocolVersion "2025-03-26"
  , connectTimeoutMs = Just 30000
  , connectSupportedVersions = ["2025-03-26", "2024-11-05"]
  }

------------------------------------------------------------------------
-- Internal state
------------------------------------------------------------------------

-- | Pending request: waiting for a response.
data PendingEntry = PendingEntry
  { pendingResult :: !(TMVar (Either RPCError Aeson.Value))
  , pendingOpts   :: !(Maybe RequestOptions)
  }

-- | Mutable session state, shared between the reader thread and
-- session operations.
data SessionState = SessionState
  { ssNextId :: !(IORef Int)
  , ssPending :: !(TVar (HashMap RequestId PendingEntry))
  , ssProgressMap :: !(TVar (Map.Map ProgressToken RequestId))
  , ssRequestHandlers :: !(TVar (HashMap Text RequestHandler))
  , ssNotificationHandlers :: !(TVar (HashMap Text NotificationHandler))
  , ssCloseCallbacks :: !(TVar [CloseReason -> IO ()])
  , ssClosed :: !(TVar Bool)
  }

newSessionState :: IO SessionState
newSessionState = do
  nextId <- newIORef 1
  pending <- newTVarIO HM.empty
  progressMap <- newTVarIO Map.empty
  reqH <- newTVarIO HM.empty
  notifH <- newTVarIO HM.empty
  closeCbs <- newTVarIO []
  closed <- newTVarIO False
  pure SessionState
    { ssNextId = nextId
    , ssPending = pending
    , ssProgressMap = progressMap
    , ssRequestHandlers = reqH
    , ssNotificationHandlers = notifH
    , ssCloseCallbacks = closeCbs
    , ssClosed = closed
    }

nextRequestId :: SessionState -> IO RequestId
nextRequestId ss = do
  n <- atomicModifyIORef' ss.ssNextId (\n -> (n + 1, n))
  pure (RequestId (Right n))

------------------------------------------------------------------------
-- Progress notification parsing
------------------------------------------------------------------------

-- | Internal type for deserializing progress notification params.
data ProgressNotif = ProgressNotif
  { pnToken    :: ProgressToken
  , pnProgress :: Double
  , pnTotal    :: Maybe Double
  , pnMessage  :: Maybe Text
  }
  deriving stock (Generic)

instance Aeson.FromJSON ProgressNotif where
  parseJSON = Aeson.withObject "ProgressNotif" $ \o ->
    ProgressNotif
      <$> o Aeson..: "progressToken"
      <*> o Aeson..: "progress"
      <*> o Aeson..:? "total"
      <*> o Aeson..:? "message"

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

-- | Establish a client-side MCP session over the given transport.
-- Performs the @initialize@ handshake and returns a live 'Session'.
connect
  :: Transport t
  => t
  -> ConnectConfig
  -> IO (Either SessionError Session)
connect transport cfg = do
  ss <- newSessionState

  -- Start background reader thread
  reader <- async (readerThread transport ss)

  -- Send initialize request
  reqId <- nextRequestId ss
  let initParams = Aeson.object
        [ "protocolVersion" Aeson..= cfg.connectProtocolVersion
        , "capabilities" Aeson..= cfg.connectClientCapabilities
        , "clientInfo" Aeson..= cfg.connectClientInfo
        ]
  let initReq = MCPRequest JSONRPCRequest
        { requestId = reqId
        , requestMethod = "initialize"
        , requestParams = Just initParams
        , requestMeta = Nothing
        }

  -- Register pending entry
  entry <- PendingEntry <$> newEmptyTMVarIO <*> pure Nothing
  atomically $ modifyTVar' ss.ssPending (HM.insert reqId entry)

  -- Send the request
  sendResult <- send transport initReq
  case sendResult of
    Left err -> do
      cancel reader
      pure (Left (SessionError SessionTransportError
        (T.pack ("Failed to send initialize: " <> show err))))
    Right () -> do
      -- Wait for response (with timeout from config)
      mResult <- case cfg.connectTimeoutMs of
        Nothing -> Just <$> atomically (takeTMVar entry.pendingResult)
        Just ms -> do
          let microseconds = fromIntegral ms * 1000
          timeout microseconds (atomically (takeTMVar entry.pendingResult))
      case mResult of
        Nothing -> do
          -- Timeout: clean up pending entry and fail
          atomically $ modifyTVar' ss.ssPending (HM.delete reqId)
          cancel reader
          let ms = maybe 0 id cfg.connectTimeoutMs
          pure (Left (SessionError NotConnected
            ("Initialize timed out after " <> T.pack (show ms) <> "ms")))
        Just result -> case result of
          Left rpcErr -> do
            cancel reader
            pure (Left (SessionError NotConnected
              ("Initialize failed: " <> rpcErr.rpcErrorMessage)))
          Right val -> do
            -- Parse InitializeResult
            case parseInitializeResult val of
              Left parseErr -> do
                cancel reader
                pure (Left (SessionError NotConnected parseErr))
              Right (serverVersion, serverCaps, serverInfo, instructions) -> do
                -- Check protocol version compatibility (multi-version negotiation)
                let ProtocolVersion sv = serverVersion
                if sv `notElem` cfg.connectSupportedVersions
                  then do
                    cancel reader
                    pure (Left (SessionError VersionMismatch
                      ("Server offered unsupported version: " <> sv)))
                  else do
                    -- Send initialized notification
                    let initializedNotif = MCPNotification JSONRPCNotification
                          { notificationMethod = "notifications/initialized"
                          , notificationParams = Nothing
                          , notificationMeta = Nothing
                          }
                    _ <- send transport initializedNotif

                    let caps = NegotiatedCapabilities
                          { negClient = cfg.connectClientCapabilities
                          , negServer = serverCaps
                          }

                    -- Build the Session record
                    let session = buildSession transport ss reader serverVersion serverInfo caps instructions cfg.connectTimeoutMs
                    pure (Right session)

-- | Close a session.
disconnect :: Session -> IO ()
disconnect s = s.sessionClose

-- | Send a @ping@ request and await the pong. Returns @Right ()@ on
-- success or @Left err@ on failure or timeout.
sessionPing :: Session -> IO (Either RPCError ())
sessionPing s = fmap (fmap (const ())) (s.sessionRequest "ping" Nothing Nothing)

------------------------------------------------------------------------
-- Reader thread
------------------------------------------------------------------------

-- | Background thread that reads from the transport and dispatches
-- incoming messages.
readerThread :: Transport t => t -> SessionState -> IO ()
readerThread transport ss = do
  stream <- messages transport
  SP.mapM_ (handleInbound ss transport) stream
    `catch` (\(_ :: SomeException) -> pure ())
  -- When stream ends, fire close callbacks
  fireClose ss LocalClose

handleInbound
  :: Transport t
  => SessionState
  -> t
  -> Either Tr.TransportError MCPMessage
  -> IO ()
handleInbound _ss _transport (Left _err) = do
  -- Transport error — could fire close, but let the stream termination handle it
  pure ()
handleInbound ss transport (Right msg) = case msg of
  MCPResult res -> do
    -- Route to pending request
    mEntry <- atomically $ do
      pending <- readTVar ss.ssPending
      case HM.lookup res.resultId pending of
        Nothing -> pure Nothing
        Just entry -> do
          writeTVar ss.ssPending (HM.delete res.resultId pending)
          -- Clean up progress map
          case entry.pendingOpts >>= (.requestProgressToken) of
            Just pt -> modifyTVar' ss.ssProgressMap (Map.delete pt)
            Nothing -> pure ()
          pure (Just entry)
    case mEntry of
      Nothing -> pure ()  -- Unmatched response, ignore
      Just entry ->
        atomically (putTMVar entry.pendingResult (Right res.resultResult))

  MCPError err -> do
    -- Route error to pending request
    case err.errorId of
      Nothing -> pure ()  -- Error without id, ignore
      Just reqId -> do
        mEntry <- atomically $ do
          pending <- readTVar ss.ssPending
          case HM.lookup reqId pending of
            Nothing -> pure Nothing
            Just entry -> do
              writeTVar ss.ssPending (HM.delete reqId pending)
              -- Clean up progress map
              case entry.pendingOpts >>= (.requestProgressToken) of
                Just pt -> modifyTVar' ss.ssProgressMap (Map.delete pt)
                Nothing -> pure ()
              pure (Just entry)
        case mEntry of
          Nothing -> pure ()
          Just entry ->
            atomically (putTMVar entry.pendingResult (Left err.errorError))

  MCPRequest req -> do
    -- Dispatch to registered request handler
    handlers <- readTVarIO ss.ssRequestHandlers
    case HM.lookup req.requestMethod handlers of
      Nothing -> do
        -- Send method not found error
        let errResp = MCPError JSONRPCError
              { errorId = Just req.requestId
              , errorError = RPCError
                  { rpcErrorCode = -32601
                  , rpcErrorMessage = "Method not found: " <> req.requestMethod
                  , rpcErrorData = Nothing
                  }
              }
        _ <- send transport errResp
        pure ()
      Just handler -> do
        let meta = RequestMeta
              { requestMetaId = req.requestId
              , requestMetaProgressToken = extractProgressToken req.requestMeta
              , requestMetaRelatedTask = Nothing
              }
        result <- handler (maybe Aeson.Null id req.requestParams) meta
          `catch` (\(e :: SomeException) -> pure (Left RPCError
            { rpcErrorCode = -32603
            , rpcErrorMessage = T.pack ("Internal error: " <> show e)
            , rpcErrorData = Nothing
            }))
        case result of
          Left rpcErr -> do
            let errResp = MCPError JSONRPCError
                  { errorId = Just req.requestId
                  , errorError = rpcErr
                  }
            _ <- send transport errResp
            pure ()
          Right val -> do
            let resp = MCPResult JSONRPCResult
                  { resultId = req.requestId
                  , resultResult = val
                  , resultMeta = Nothing
                  }
            _ <- send transport resp
            pure ()

  MCPNotification notif
    | notif.notificationMethod == "notifications/progress" -> do
        -- Dispatch progress to the requesting callback
        let params = maybe Aeson.Null id notif.notificationParams
        case Aeson.fromJSON @ProgressNotif params of
          Aeson.Error _ -> pure ()  -- malformed, ignore
          Aeson.Success pn -> do
            mEntry <- atomically $ do
              pm <- readTVar ss.ssProgressMap
              case Map.lookup pn.pnToken pm of
                Nothing -> pure Nothing
                Just rid -> do
                  pending <- readTVar ss.ssPending
                  pure (HM.lookup rid pending)
            case mEntry of
              Nothing -> pure ()
              Just entry ->
                case entry.pendingOpts >>= (.requestOnProgress) of
                  Nothing -> pure ()
                  Just cb  -> cb pn.pnProgress pn.pnTotal pn.pnMessage
                    `catch` (\(_ :: SomeException) -> pure ())

    | otherwise -> do
        -- Dispatch to registered notification handler
        handlers <- readTVarIO ss.ssNotificationHandlers
        case HM.lookup notif.notificationMethod handlers of
          Nothing -> pure ()  -- Unknown notifications are silently ignored
          Just handler ->
            handler (maybe Aeson.Null id notif.notificationParams)
              `catch` (\(_ :: SomeException) -> pure ())

------------------------------------------------------------------------
-- Session builder
------------------------------------------------------------------------

buildSession
  :: Transport t
  => t
  -> SessionState
  -> Async ()
  -> ProtocolVersion
  -> Implementation
  -> NegotiatedCapabilities
  -> Maybe Text
  -> Maybe Word
  -> Session
buildSession transport ss reader version peerInfo caps mInstructions defaultTimeoutMs = Session
  { sessionProtocolVersion = version
  , sessionPeerInfo = peerInfo
  , sessionCapabilities = caps
  , sessionInstructions = mInstructions

  , sessionRequest = \method params opts -> do
      isClosed <- readTVarIO ss.ssClosed
      if isClosed
        then pure (Left RPCError
          { rpcErrorCode = -32600
          , rpcErrorMessage = "Session is closed"
          , rpcErrorData = Nothing
          })
        else do
          reqId <- nextRequestId ss
          entry <- PendingEntry <$> newEmptyTMVarIO <*> pure opts
          atomically $ do
            modifyTVar' ss.ssPending (HM.insert reqId entry)
            case opts >>= (.requestProgressToken) of
              Just pt -> modifyTVar' ss.ssProgressMap (Map.insert pt reqId)
              Nothing -> pure ()

          let reqMeta = case opts of
                Nothing -> Nothing
                Just o -> case o.requestProgressToken of
                  Nothing -> Nothing
                  Just pt -> Just (HM.singleton "progressToken" (Aeson.toJSON pt))
          let req = MCPRequest JSONRPCRequest
                { requestId = reqId
                , requestMethod = method
                , requestParams = params
                , requestMeta = reqMeta
                }
          sendRes <- send transport req
          case sendRes of
            Left err ->
              pure (Left RPCError
                { rpcErrorCode = -32603
                , rpcErrorMessage = T.pack ("Transport error: " <> show err)
                , rpcErrorData = Nothing
                })
            Right () -> do
              -- Determine timeout: per-request overrides session default
              let timeoutMs = case opts of
                    Just o | Just ms <- o.requestTimeoutMs -> Just ms
                    _ -> defaultTimeoutMs
              case timeoutMs of
                Nothing ->
                  -- No timeout configured, block indefinitely
                  atomically (takeTMVar entry.pendingResult)
                Just ms -> do
                  let microseconds = fromIntegral ms * 1000
                  mResult <- timeout microseconds (atomically (takeTMVar entry.pendingResult))
                  case mResult of
                    Just result -> pure result
                    Nothing -> do
                      -- Timeout fired: clean up pending entry and progress map
                      atomically $ do
                        modifyTVar' ss.ssPending (HM.delete reqId)
                        case opts >>= (.requestProgressToken) of
                          Just pt -> modifyTVar' ss.ssProgressMap (Map.delete pt)
                          Nothing -> pure ()
                      -- Send cancellation notification
                      let cancelParams = Aeson.object
                            [ "requestId" Aeson..= reqId
                            , "reason"    Aeson..= ("Request timed out" :: Text)
                            ]
                      let cancelNotif = MCPNotification JSONRPCNotification
                            { notificationMethod = "notifications/cancelled"
                            , notificationParams = Just cancelParams
                            , notificationMeta = Nothing
                            }
                      _ <- send transport cancelNotif
                      pure (Left RPCError
                        { rpcErrorCode = -32603
                        , rpcErrorMessage = "Request timed out after " <> T.pack (show ms) <> "ms"
                        , rpcErrorData = Nothing
                        })

  , sessionNotify = \method params -> do
      let notif = MCPNotification JSONRPCNotification
            { notificationMethod = method
            , notificationParams = params
            , notificationMeta = Nothing
            }
      _ <- send transport notif
      pure ()

  , sessionCancel = \reqId reason -> do
      let params = Aeson.object $
            [ "requestId" Aeson..= reqId ]
            ++ maybe [] (\r -> ["reason" Aeson..= r]) reason
      let notif = MCPNotification JSONRPCNotification
            { notificationMethod = "notifications/cancelled"
            , notificationParams = Just params
            , notificationMeta = Nothing
            }
      _ <- send transport notif
      pure ()

  , sessionOnRequest = \method handler ->
      atomically $ modifyTVar' ss.ssRequestHandlers (HM.insert method handler)

  , sessionOnNotification = \method handler ->
      atomically $ modifyTVar' ss.ssNotificationHandlers (HM.insert method handler)

  , sessionClose = do
      alreadyClosed <- atomically $ do
        c <- readTVar ss.ssClosed
        writeTVar ss.ssClosed True
        pure c
      if alreadyClosed
        then pure ()
        else do
          cancel reader
          close transport
          fireClose ss LocalClose

  , sessionOnClose = \callback ->
      atomically $ modifyTVar' ss.ssCloseCallbacks (callback :)
  }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

fireClose :: SessionState -> CloseReason -> IO ()
fireClose ss reason = do
  callbacks <- readTVarIO ss.ssCloseCallbacks
  mapM_ (\cb -> cb reason `catch` (\(_ :: SomeException) -> pure ())) callbacks

parseInitializeResult
  :: Aeson.Value
  -> Either Text (ProtocolVersion, ServerCapabilities, Implementation, Maybe Text)
parseInitializeResult val = case val of
  Aeson.Object o -> do
    version <- case KM.lookup "protocolVersion" o of
      Nothing -> Left "Missing protocolVersion in initialize result"
      Just v -> case Aeson.fromJSON v of
        Aeson.Error e -> Left (T.pack e)
        Aeson.Success pv -> Right pv
    serverCaps <- case KM.lookup "capabilities" o of
      Nothing -> Left "Missing capabilities in initialize result"
      Just v -> case Aeson.fromJSON v of
        Aeson.Error e -> Left (T.pack ("Invalid capabilities: " <> e))
        Aeson.Success sc -> Right sc
    serverInfo <- case KM.lookup "serverInfo" o of
      Nothing -> Left "Missing serverInfo in initialize result"
      Just v -> case Aeson.fromJSON v of
        Aeson.Error e -> Left (T.pack ("Invalid serverInfo: " <> e))
        Aeson.Success si -> Right si
    let instructions = case KM.lookup "instructions" o of
          Just (Aeson.String t) -> Just t
          _ -> Nothing
    Right (version, serverCaps, serverInfo, instructions)
  _ -> Left "Initialize result is not an object"

extractProgressToken :: Maybe (HashMap Text Aeson.Value) -> Maybe ProgressToken
extractProgressToken Nothing = Nothing
extractProgressToken (Just meta) =
  case HM.lookup "progressToken" meta of
    Nothing -> Nothing
    Just v -> case Aeson.fromJSON v of
      Aeson.Error _ -> Nothing
      Aeson.Success pt -> Just pt
