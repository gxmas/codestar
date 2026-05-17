-- |
-- Module      : Network.MCP.Transport.Http.Streamable
-- Stability   : experimental
--
-- Streamable HTTP transport for MCP (client side, spec 2025-03-26).
-- The client POSTs all messages to a single endpoint URL. The server
-- responds with either @application/json@ (single response) or
-- @text/event-stream@ (SSE stream of responses). Unlike the older
-- HTTP+SSE transport there is no persistent SSE setup phase.
module Network.MCP.Transport.Http.Streamable
  ( StreamableHttpTransport
  , new
  , AuthContext (..)
  , newWithAuth
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TQueue, TVar
  , atomically, newTQueueIO, newTVarIO
  , readTQueue, readTVar, readTVarIO, writeTQueue, writeTVar
  )
import Control.Exception (SomeException, catch, finally, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Status as Status
import qualified Streaming as S
import qualified Streaming.Prelude as SP

import Network.MCP.Codec (McpCodec (..), Codec (..))
import Network.MCP.Transport
  ( Transport (..), TransportError (..), TransportErrorKind (..)
  )
import Network.MCP.Transport.Http (SseEvent (..), parseSseLines)
import Network.MCP.Types (MCPMessage)

------------------------------------------------------------------------
-- StreamableHttpTransport
------------------------------------------------------------------------

-- | Client-side Streamable HTTP transport (MCP spec 2025-03-26).
-- All messages are POSTed to a single endpoint URL. Responses arrive
-- either as a single JSON body or as an SSE stream.
data StreamableHttpTransport = StreamableHttpTransport
  { shtManager     :: !HTTP.Manager
  , shtBaseUrl     :: !Text
  , shtSessionId   :: !(TVar (Maybe Text))  -- Mcp-Session-Id from server
  , shtInbox       :: !(TQueue (Either TransportError MCPMessage))
  , shtIsClosed    :: !(TVar Bool)
  , shtGetThread   :: !(TVar (Maybe (Async ())))  -- background GET SSE thread
  , shtLastEventId :: !(TVar (Maybe Text))         -- for Last-Event-ID on reconnect
  , shtAuth        :: !(Maybe AuthContext)
  }

-- | Authentication context for bearer token auth. The callback is called
-- when a 401 is received and must return a fresh access token (or Left on
-- failure). The TVar holds the current access token.
data AuthContext = AuthContext
  { acCurrentToken  :: !(TVar (Maybe Text))
    -- ^ The current bearer token (Nothing if not yet obtained)
  , acRefreshToken  :: !(IO (Either Text Text))
    -- ^ IO action to obtain a fresh access token; called on 401.
  }

-- | Create a new Streamable HTTP transport. Initialises a TLS manager
-- and empty state. No background thread is started — messages arrive
-- only in response to 'send'.
new :: Text -> IO StreamableHttpTransport
new baseUrl = do
  manager     <- TLS.newTlsManager
  sessionId   <- newTVarIO Nothing
  inbox       <- newTQueueIO
  closed      <- newTVarIO False
  getThread   <- newTVarIO Nothing
  lastEventId <- newTVarIO Nothing
  pure StreamableHttpTransport
    { shtManager     = manager
    , shtBaseUrl     = baseUrl
    , shtSessionId   = sessionId
    , shtInbox       = inbox
    , shtIsClosed    = closed
    , shtGetThread   = getThread
    , shtLastEventId = lastEventId
    , shtAuth        = Nothing
    }

-- | Create a Streamable HTTP transport with bearer token authentication.
-- The @refreshToken@ action is called on 401 to obtain a fresh token and
-- the request is retried once.
newWithAuth
  :: Text                  -- ^ base URL
  -> IO (Either Text Text) -- ^ action to obtain/refresh access token
  -> IO StreamableHttpTransport
newWithAuth baseUrl refreshToken = do
  manager     <- TLS.newTlsManager
  sessionId   <- newTVarIO Nothing
  inbox       <- newTQueueIO
  closed      <- newTVarIO False
  getThread   <- newTVarIO Nothing
  lastEventId <- newTVarIO Nothing
  tokenVar    <- newTVarIO Nothing
  let auth = AuthContext
        { acCurrentToken = tokenVar
        , acRefreshToken = refreshToken
        }
  pure StreamableHttpTransport
    { shtManager     = manager
    , shtBaseUrl     = baseUrl
    , shtSessionId   = sessionId
    , shtInbox       = inbox
    , shtIsClosed    = closed
    , shtGetThread   = getThread
    , shtLastEventId = lastEventId
    , shtAuth        = Just auth
    }

------------------------------------------------------------------------
-- Transport instance
------------------------------------------------------------------------

instance Transport StreamableHttpTransport where
  send t msg = do
    isClosed <- readTVarIO t.shtIsClosed
    if isClosed
      then pure (Left (TransportError TransportClosed "transport is closed"))
      else case encode McpCodec msg of
        Left _codecErr ->
          pure (Left (TransportError TransportProtocolError "failed to encode message"))
        Right bs -> do
          result <- try (doStreamablePost t bs) :: IO (Either SomeException ())
          case result of
            Left ex -> do
              atomically $ writeTQueue t.shtInbox
                (Left (TransportError TransportIoError (T.pack (show ex))))
              pure (Right ())
            Right () -> pure (Right ())

  messages t = pure loop
    where
      loop = do
        item <- S.lift (atomically (readTQueue t.shtInbox))
        SP.yield item
        case item of
          Left err | err.transportErrorKind == TransportClosed -> pure ()
          _ -> loop

  close t = do
    alreadyClosed <- atomically $ do
      c <- readTVar t.shtIsClosed
      writeTVar t.shtIsClosed True
      pure c
    if alreadyClosed
      then pure ()
      else do
        -- Cancel the GET stream thread
        mThread <- readTVarIO t.shtGetThread
        case mThread of
          Nothing     -> pure ()
          Just thread -> cancel thread `catch` (\(_ :: SomeException) -> pure ())
        -- Fire-and-forget DELETE to terminate server session
        mSid <- readTVarIO t.shtSessionId
        case mSid of
          Nothing  -> pure ()
          Just sid -> sendSessionDelete t sid
                        `catch` (\(_ :: SomeException) -> pure ())
        atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportClosed "closed"))

------------------------------------------------------------------------
-- HTTP POST with response dispatch
------------------------------------------------------------------------

sendSessionDelete :: StreamableHttpTransport -> Text -> IO ()
sendSessionDelete t sid = do
  req0 <- HTTP.parseRequest (T.unpack t.shtBaseUrl)
  mToken <- case t.shtAuth of
    Nothing   -> pure Nothing
    Just auth -> readTVarIO auth.acCurrentToken
  let authHdr = maybe [] (\tok -> [("Authorization", "Bearer " <> TE.encodeUtf8 tok)]) mToken
  let req = req0
        { HTTP.method         = "DELETE"
        , HTTP.requestHeaders = ("Mcp-Session-Id", TE.encodeUtf8 sid) : authHdr
        }
  _ <- HTTP.httpNoBody req t.shtManager
  pure ()

doStreamablePost :: StreamableHttpTransport -> LBS.ByteString -> IO ()
doStreamablePost t body = do
  req0 <- HTTP.parseRequest (T.unpack t.shtBaseUrl)
  mSessionId <- readTVarIO t.shtSessionId
  let sessionHdr = case mSessionId of
        Nothing  -> []
        Just sid -> [("Mcp-Session-Id", TE.encodeUtf8 sid)]
  mToken <- case t.shtAuth of
    Nothing   -> pure Nothing
    Just auth -> readTVarIO auth.acCurrentToken
  let authHdr = case mToken of
        Nothing  -> []
        Just tok -> [("Authorization", "Bearer " <> TE.encodeUtf8 tok)]
  let req = req0
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        , HTTP.requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Accept", "application/json, text/event-stream")
            ] ++ sessionHdr ++ authHdr
        }
  HTTP.withResponse req t.shtManager $ \resp -> do
    -- Extract Mcp-Session-Id from response headers
    let hdrs = HTTP.responseHeaders resp
    case lookup "Mcp-Session-Id" hdrs of
      Just sid -> do
        atomically (writeTVar t.shtSessionId (Just (TE.decodeUtf8Lenient sid)))
        ensureGetStream t
      Nothing  -> pure ()

    let status = HTTP.responseStatus resp
    if Status.statusCode status == 404
      then do
        atomically (writeTVar t.shtSessionId Nothing)
        atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportSessionExpired "session expired (HTTP 404)"))
      else if Status.statusCode status == 400
        then
          -- 400 means the request was missing a required session ID.
          -- This is a protocol error, not a session expiration; do not clear the session ID.
          atomically $ writeTQueue t.shtInbox
            (Left (TransportError TransportProtocolError "session ID required (HTTP 400)"))
      else if Status.statusCode status == 401
        then case t.shtAuth of
          Nothing ->
            atomically $ writeTQueue t.shtInbox
              (Left (TransportError TransportIoError "HTTP 401 Unauthorized (no auth configured)"))
          Just auth -> do
            -- Try to refresh the token
            refreshResult <- auth.acRefreshToken
            case refreshResult of
              Left err ->
                atomically $ writeTQueue t.shtInbox
                  (Left (TransportError TransportIoError ("401: token refresh failed: " <> err)))
              Right newToken -> do
                atomically (writeTVar auth.acCurrentToken (Just newToken))
                -- Retry the request once with the new token
                doStreamablePostWithToken t body (Just newToken)
      else if not (Status.statusIsSuccessful status)
        then atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportIoError
            ("HTTP POST failed with status " <> T.pack (show (Status.statusCode status)))))
      else if Status.statusCode status == 202
        then pure ()   -- notification/response acknowledged, no body
        else do
          let ct = maybe "" id (lookup "Content-Type" hdrs)
          if "text/event-stream" `BS.isInfixOf` ct
            then readSseResponse (HTTP.responseBody resp) t.shtInbox
            else if "application/json" `BS.isInfixOf` ct
              then readJsonResponse (HTTP.responseBody resp) t.shtInbox
              else atomically $ writeTQueue t.shtInbox
                (Left (TransportError TransportProtocolError
                  ("Unexpected Content-Type: " <> TE.decodeUtf8Lenient ct)))

-- | Internal: identical to doStreamablePost but uses the supplied token
-- instead of reading from shtAuth, and does not retry on 401.
doStreamablePostWithToken
  :: StreamableHttpTransport -> LBS.ByteString -> Maybe Text -> IO ()
doStreamablePostWithToken t body mOverrideToken = do
  req0 <- HTTP.parseRequest (T.unpack t.shtBaseUrl)
  mSessionId <- readTVarIO t.shtSessionId
  let sessionHdr = case mSessionId of
        Nothing  -> []
        Just sid -> [("Mcp-Session-Id", TE.encodeUtf8 sid)]
      authHdr = case mOverrideToken of
        Nothing  -> []
        Just tok -> [("Authorization", "Bearer " <> TE.encodeUtf8 tok)]
      req = req0
            { HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS body
            , HTTP.requestHeaders =
                [ ("Content-Type", "application/json")
                , ("Accept", "application/json, text/event-stream")
                ] ++ sessionHdr ++ authHdr
            }
  HTTP.withResponse req t.shtManager $ \resp -> do
    -- Extract Mcp-Session-Id from response headers
    let hdrs = HTTP.responseHeaders resp
    case lookup "Mcp-Session-Id" hdrs of
      Just sid -> do
        atomically (writeTVar t.shtSessionId (Just (TE.decodeUtf8Lenient sid)))
        ensureGetStream t
      Nothing  -> pure ()
    let status = HTTP.responseStatus resp
    -- Same dispatch as doStreamablePost but without 401 retry
    if Status.statusCode status == 404
      then do
        atomically (writeTVar t.shtSessionId Nothing)
        atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportSessionExpired "session expired (HTTP 404)"))
      else if Status.statusCode status == 400
        then atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportProtocolError "session ID required (HTTP 400)"))
      else if not (Status.statusIsSuccessful status)
        then atomically $ writeTQueue t.shtInbox
          (Left (TransportError TransportIoError
            ("HTTP POST failed with status " <> T.pack (show (Status.statusCode status)))))
      else if Status.statusCode status == 202
        then pure ()
        else do
          let ct = maybe "" id (lookup "Content-Type" hdrs)
          if "text/event-stream" `BS.isInfixOf` ct
            then readSseResponse (HTTP.responseBody resp) t.shtInbox
            else if "application/json" `BS.isInfixOf` ct
              then readJsonResponse (HTTP.responseBody resp) t.shtInbox
              else atomically $ writeTQueue t.shtInbox
                (Left (TransportError TransportProtocolError
                  ("Unexpected Content-Type: " <> TE.decodeUtf8Lenient ct)))

------------------------------------------------------------------------
-- Response readers
------------------------------------------------------------------------

readJsonResponse
  :: IO BS.ByteString
  -> TQueue (Either TransportError MCPMessage)
  -> IO ()
readJsonResponse readChunk inbox = do
  chunks <- readAll readChunk
  let bs = LBS.fromChunks chunks
  -- Try array (batch) first, then single object
  case Aeson.eitherDecode' bs :: Either String [MCPMessage] of
    Right msgs -> mapM_ (\m -> atomically (writeTQueue inbox (Right m))) msgs
    Left _ ->
      case decode McpCodec bs of
        Left _codecErr ->
          atomically $ writeTQueue inbox
            (Left (TransportError TransportProtocolError
              ("Failed to decode JSON response: " <> T.take 200 (TE.decodeUtf8Lenient (LBS.toStrict bs)))))
        Right msg ->
          atomically (writeTQueue inbox (Right msg))

readAll :: IO BS.ByteString -> IO [BS.ByteString]
readAll readChunk = go []
  where
    go acc = do
      chunk <- readChunk
      if BS.null chunk
        then pure (reverse acc)
        else go (chunk : acc)

readSseResponse
  :: IO BS.ByteString
  -> TQueue (Either TransportError MCPMessage)
  -> IO ()
readSseResponse readChunk inbox = go BS.empty []
  where
    go buf eventAcc = do
      chunk <- readChunk
      if BS.null chunk
        then do
          -- EOF: flush any remaining event
          case eventAcc of
            [] -> pure ()
            _  -> do
              let events = parseSseLines (reverse eventAcc ++ [BS.empty])
              mapM_ (dispatchEvent inbox) events
        else do
          let allBytes = buf <> chunk
          let (newLines, remaining) = splitLines allBytes
          eventAcc' <- feedLines newLines eventAcc
          go remaining eventAcc'

    feedLines :: [BS.ByteString] -> [BS.ByteString] -> IO [BS.ByteString]
    feedLines [] accum = pure accum
    feedLines (line : rest) accum
      | BS.null line = do
          let events = parseSseLines (reverse accum ++ [BS.empty])
          mapM_ (dispatchEvent inbox) events
          feedLines rest []
      | otherwise =
          feedLines rest (line : accum)

dispatchEvent :: TQueue (Either TransportError MCPMessage) -> SseEvent -> IO ()
dispatchEvent inbox event =
  -- Only dispatch events with type "message" or no event type
  case event.sseEvent of
    Nothing        -> decodeAndPush
    Just "message" -> decodeAndPush
    _              -> pure ()  -- Ignore other event types (e.g. heartbeat)
  where
    decodeAndPush = do
      let bs = LBS.fromStrict (TE.encodeUtf8 event.sseData)
      case decode McpCodec bs of
        Left _codecErr ->
          atomically $ writeTQueue inbox
            (Left (TransportError TransportProtocolError
              ("Failed to decode SSE data: " <> T.take 200 event.sseData)))
        Right msg ->
          atomically (writeTQueue inbox (Right msg))

------------------------------------------------------------------------
-- GET SSE stream for server-initiated messages
------------------------------------------------------------------------

-- | Start the background GET SSE thread if not already running.
-- Called once after the first Mcp-Session-Id is received.
ensureGetStream :: StreamableHttpTransport -> IO ()
ensureGetStream t = do
  mThread <- readTVarIO t.shtGetThread
  case mThread of
    Just _  -> pure ()
    Nothing -> do
      thread <- async (getStreamLoop t)
      atomically (writeTVar t.shtGetThread (Just thread))

-- | Reconnecting loop for the GET SSE stream. Runs until the transport
-- is closed or the server responds with 405 Method Not Allowed (meaning
-- it does not support server-initiated messages via GET).
getStreamLoop :: StreamableHttpTransport -> IO ()
getStreamLoop t = go `finally` atomically (writeTVar t.shtGetThread Nothing)
  where
    go = do
      isClosed <- readTVarIO t.shtIsClosed
      if isClosed
        then pure ()
        else do
          -- Returns True = reconnect, False = 405 (stop)
          result <- try (connectAndReadGet t) :: IO (Either SomeException Bool)
          case result of
            Left _      -> checkAndRetry
            Right True  -> checkAndRetry
            Right False -> pure ()   -- 405: server does not support GET stream
    checkAndRetry = do
      isClosed <- readTVarIO t.shtIsClosed
      if isClosed then pure () else go

-- | Perform a single GET request and read the SSE stream until EOF or error.
-- Returns False if the server responds 405 (do not retry), True otherwise.
connectAndReadGet :: StreamableHttpTransport -> IO Bool
connectAndReadGet t = do
  req0    <- HTTP.parseRequest (T.unpack t.shtBaseUrl)
  mSid    <- readTVarIO t.shtSessionId
  mLastId <- readTVarIO t.shtLastEventId
  mToken  <- case t.shtAuth of
    Nothing   -> pure Nothing
    Just auth -> readTVarIO auth.acCurrentToken
  let sessionHdr = maybe [] (\s -> [("Mcp-Session-Id", TE.encodeUtf8 s)]) mSid
      lastIdHdr  = maybe [] (\l -> [("Last-Event-ID",  TE.encodeUtf8 l)]) mLastId
      authHdr    = maybe [] (\tok -> [("Authorization", "Bearer " <> TE.encodeUtf8 tok)]) mToken
      req = req0
              { HTTP.method = "GET"
              , HTTP.requestHeaders =
                  [("Accept", "text/event-stream")]
                  ++ sessionHdr ++ lastIdHdr ++ authHdr
              }
  HTTP.withResponse req t.shtManager $ \resp -> do
    let status = HTTP.responseStatus resp
    if Status.statusCode status == 405
      then pure False    -- server does not allow client-initiated GET; stop
      else if Status.statusCode status == 401
        then pure False  -- token rejected; stop GET loop (POST path handles re-auth)
      else if not (Status.statusIsSuccessful status)
        then pure True   -- transient error; retry
        else do
          readSseGetStream (HTTP.responseBody resp) t.shtInbox
                           t.shtLastEventId t.shtIsClosed
          pure True      -- EOF on stream; reconnect

-- | Consume an SSE stream from a GET response, pushing decoded messages
-- into the inbox and tracking Last-Event-ID for reconnection.
readSseGetStream
  :: IO BS.ByteString
  -> TQueue (Either TransportError MCPMessage)
  -> TVar (Maybe Text)
  -> TVar Bool
  -> IO ()
readSseGetStream readChunk inbox lastEventId isClosed = go BS.empty []
  where
    go buf eventAcc = do
      closed <- readTVarIO isClosed
      if closed
        then pure ()
        else do
          chunk <- readChunk
          if BS.null chunk
            then pure ()   -- EOF; outer loop reconnects
            else do
              let allBytes = buf <> chunk
                  (newLines, remaining) = splitLines allBytes
              eventAcc' <- feedLines newLines eventAcc
              go remaining eventAcc'

    feedLines [] acc = pure acc
    feedLines (line : rest) acc
      | BS.null line = do
          let events = parseSseLines (reverse acc ++ [BS.empty])
          mapM_ (dispatchGetEvent inbox lastEventId) events
          feedLines rest []
      | otherwise = feedLines rest (line : acc)

-- | Handle a single SSE event from the GET stream: update Last-Event-ID
-- and decode/enqueue the message.
dispatchGetEvent
  :: TQueue (Either TransportError MCPMessage)
  -> TVar (Maybe Text)
  -> SseEvent
  -> IO ()
dispatchGetEvent inbox lastEventId event = do
  case event.sseId of
    Just eid -> atomically (writeTVar lastEventId (Just eid))
    Nothing  -> pure ()
  case event.sseEvent of
    Nothing        -> decodeAndPush
    Just "message" -> decodeAndPush
    _              -> pure ()
  where
    decodeAndPush = do
      let bs = LBS.fromStrict (TE.encodeUtf8 event.sseData)
      case decode McpCodec bs of
        Left _ ->
          atomically $ writeTQueue inbox
            (Left (TransportError TransportProtocolError
              ("Failed to decode GET SSE data: " <> T.take 200 event.sseData)))
        Right msg ->
          atomically (writeTQueue inbox (Right msg))

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs = go bs []
  where
    go remaining acc =
      case BS8.elemIndex '\n' remaining of
        Nothing -> (reverse acc, remaining)
        Just idx ->
          let (line, rest) = BS.splitAt (idx + 1) remaining
          in go rest (BS8.dropWhileEnd (\c -> c == '\r' || c == '\n') line : acc)
