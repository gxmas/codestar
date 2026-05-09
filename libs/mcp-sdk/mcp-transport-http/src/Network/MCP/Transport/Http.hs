-- |
-- Module      : Network.MCP.Transport.Http
-- Stability   : experimental
--
-- HTTP+SSE transport for MCP (client side). Uses Server-Sent Events
-- for server→client messages and HTTP POST for client→server messages.
module Network.MCP.Transport.Http
  ( HttpTransport
  , new
    -- * SSE parsing (exported for testing)
  , SseEvent (..)
  , parseSseLines
  , renderSseEvent
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TQueue, TVar
  , atomically, newTQueueIO, newTVarIO
  , readTQueue, readTVar, readTVarIO, writeTQueue, writeTVar
  )
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive ()
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
import Network.MCP.Types (MCPMessage)

------------------------------------------------------------------------
-- SSE types
------------------------------------------------------------------------

-- | A single Server-Sent Event.
data SseEvent = SseEvent
  { sseId    :: !(Maybe Text)
  , sseEvent :: !(Maybe Text)
  , sseData  :: !Text
  }
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- HttpTransport
------------------------------------------------------------------------

-- | Client-side HTTP+SSE transport. Connects to an SSE endpoint for
-- receiving messages and uses HTTP POST for sending messages.
data HttpTransport = HttpTransport
  { htManager     :: !HTTP.Manager
  , htPostUrl     :: !(TVar (Maybe Text))
  , htInbox       :: !(TQueue (Either TransportError MCPMessage))
  , htIsClosed    :: !(TVar Bool)
  , htSseThread   :: !(Async ())
  , htLastEventId :: !(TVar (Maybe Text))
  }

-- | Create a new HTTP+SSE transport. Connects to the SSE endpoint and
-- starts reading events in a background thread.
new :: Text -> IO HttpTransport
new sseUrl = do
  manager <- TLS.newTlsManager
  postUrl <- newTVarIO Nothing
  inbox <- newTQueueIO
  closed <- newTVarIO False
  lastEventId <- newTVarIO Nothing
  reader <- async (sseReaderLoop manager sseUrl inbox closed postUrl lastEventId)
  pure HttpTransport
    { htManager     = manager
    , htPostUrl     = postUrl
    , htInbox       = inbox
    , htIsClosed    = closed
    , htSseThread   = reader
    , htLastEventId = lastEventId
    }

------------------------------------------------------------------------
-- SSE reader
------------------------------------------------------------------------

sseReaderLoop
  :: HTTP.Manager
  -> Text
  -> TQueue (Either TransportError MCPMessage)
  -> TVar Bool
  -> TVar (Maybe Text)
  -> TVar (Maybe Text)
  -> IO ()
sseReaderLoop manager sseUrl inbox closed postUrl lastEventId = go
  where
    go = do
      isClosed <- readTVarIO closed
      if isClosed then pure ()
      else do
        _ <- try (connectAndRead manager sseUrl inbox closed postUrl lastEventId)
          :: IO (Either SomeException ())
        -- Reconnect on both exception and clean server close, unless we
        -- have been explicitly closed.
        isClosed' <- readTVarIO closed
        if isClosed'
          then pure ()
          else go

connectAndRead
  :: HTTP.Manager
  -> Text
  -> TQueue (Either TransportError MCPMessage)
  -> TVar Bool
  -> TVar (Maybe Text)
  -> TVar (Maybe Text)
  -> IO ()
connectAndRead manager sseUrl inbox closed postUrl lastEventId = do
  req0 <- HTTP.parseRequest (T.unpack sseUrl)
  mLastId <- readTVarIO lastEventId
  let headers = [("Accept", "text/event-stream")]
        ++ maybe [] (\lid -> [("Last-Event-ID", TE.encodeUtf8 lid)]) mLastId
  let req = req0 { HTTP.requestHeaders = headers }
  HTTP.withResponse req manager $ \resp -> do
    let body = HTTP.responseBody resp
    readSseStream body inbox closed postUrl lastEventId

readSseStream
  :: IO BS.ByteString
  -> TQueue (Either TransportError MCPMessage)
  -> TVar Bool
  -> TVar (Maybe Text)
  -> TVar (Maybe Text)
  -> IO ()
-- | buf: unprocessed bytes after the last newline (partial line).
-- eventAcc: complete field lines accumulated for the current SSE event,
-- stored in reverse order. Maintaining this across calls means a single
-- event whose id:/data: lines arrive in different TCP chunks is
-- reconstructed correctly before being dispatched.
readSseStream readChunk inbox closed postUrl lastEventId = go BS.empty []
  where
    go buf eventAcc = do
      isClosed <- readTVarIO closed
      if isClosed then pure ()
      else do
        chunk <- readChunk
        if BS.null chunk
          then
            -- EOF: discard any incomplete event, signal transport closed.
            atomically $ writeTQueue inbox
              (Left (TransportError TransportClosed "SSE stream ended"))
          else do
            let allBytes = buf <> chunk
            let (newLines, remaining) = splitLines allBytes
            eventAcc' <- feedLines newLines eventAcc
            go remaining eventAcc'

    -- Feed completed lines into the event accumulator one at a time.
    -- An empty line is the SSE event boundary: emit the event and reset.
    feedLines :: [BS.ByteString] -> [BS.ByteString] -> IO [BS.ByteString]
    feedLines [] accum = pure accum
    feedLines (line : rest) accum
      | BS.null line = do
          let events = parseSseLines (reverse accum ++ [BS.empty])
          mapM_ (handleSseEvent inbox postUrl lastEventId) events
          feedLines rest []
      | otherwise =
          feedLines rest (line : accum)

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs = go bs []
  where
    go remaining acc =
      case BS8.elemIndex '\n' remaining of
        Nothing -> (reverse acc, remaining)
        Just idx ->
          let (line, rest) = BS.splitAt (idx + 1) remaining
          in go rest (BS8.dropWhileEnd (\c -> c == '\r' || c == '\n') line : acc)

handleSseEvent
  :: TQueue (Either TransportError MCPMessage)
  -> TVar (Maybe Text)
  -> TVar (Maybe Text)
  -> SseEvent
  -> IO ()
handleSseEvent inbox postUrl lastEventId event = do
  -- Update last event ID
  case event.sseId of
    Just eid -> atomically (writeTVar lastEventId (Just eid))
    Nothing -> pure ()

  -- Handle event type
  case event.sseEvent of
    Just "endpoint" -> do
      -- Server is telling us the POST endpoint URL
      atomically (writeTVar postUrl (Just event.sseData))

    _ -> do
      -- Treat as a message event: decode the data as MCPMessage
      let bs = LBS.fromStrict (TE.encodeUtf8 event.sseData)
      case decode McpCodec bs of
        Left _codecErr ->
          atomically $ writeTQueue inbox
            (Left (TransportError TransportProtocolError
              ("Failed to decode SSE data: " <> T.take 200 event.sseData)))
        Right msg ->
          atomically (writeTQueue inbox (Right msg))

------------------------------------------------------------------------
-- SSE parsing
------------------------------------------------------------------------

-- | Parse a list of raw lines into SSE events. An event boundary is
-- an empty line. Lines prefixed with @data:@, @id:@, or @event:@ set
-- the corresponding field. Multiple @data:@ lines within one event
-- are joined with newlines.
parseSseLines :: [BS.ByteString] -> [SseEvent]
parseSseLines = go Nothing Nothing [] []
  where
    go eid etype dlines acc [] =
      let acc' = finishEvent eid etype dlines acc
      in reverse acc'
    go eid etype dlines acc (line : rest)
      | BS.null line =
          -- Empty line = event boundary
          go Nothing Nothing [] (finishEvent eid etype dlines acc) rest
      | Just val <- BS8.stripPrefix "data:" line =
          go eid etype (T.strip (TE.decodeUtf8Lenient val) : dlines) acc rest
      | Just val <- BS8.stripPrefix "id:" line =
          go (Just (T.strip (TE.decodeUtf8Lenient val))) etype dlines acc rest
      | Just val <- BS8.stripPrefix "event:" line =
          go eid (Just (T.strip (TE.decodeUtf8Lenient val))) dlines acc rest
      | BS8.isPrefixOf ":" line =
          -- Comment line, skip
          go eid etype dlines acc rest
      | otherwise =
          -- Unknown field, skip
          go eid etype dlines acc rest

    finishEvent _ _ [] acc = acc  -- no data lines = no event
    finishEvent eid etype dlines acc =
      SseEvent
        { sseId = eid
        , sseEvent = etype
        , sseData = T.intercalate "\n" (reverse dlines)
        } : acc

-- | Render an 'SseEvent' back to the line-based wire format that
-- 'parseSseLines' consumes. Each element in the result is one line
-- (no newline character); the last element is an empty ByteString
-- marking the event boundary. Useful for roundtrip property tests.
renderSseEvent :: SseEvent -> [BS.ByteString]
renderSseEvent e = concat
  [ maybe [] (\i -> ["id: " <> TE.encodeUtf8 i]) e.sseId
  , maybe [] (\t -> ["event: " <> TE.encodeUtf8 t]) e.sseEvent
  , map (\l -> "data: " <> TE.encodeUtf8 l) (T.splitOn "\n" e.sseData)
  , [BS.empty]
  ]

------------------------------------------------------------------------
-- Transport instance
------------------------------------------------------------------------

instance Transport HttpTransport where
  send t msg = do
    isClosed <- readTVarIO t.htIsClosed
    if isClosed
      then pure (Left (TransportError TransportClosed "transport is closed"))
      else do
        mUrl <- readTVarIO t.htPostUrl
        case mUrl of
          Nothing ->
            pure (Left (TransportError TransportProtocolError
              "POST endpoint URL not yet received from server"))
          Just url ->
            case encode McpCodec msg of
              Left _codecErr ->
                pure (Left (TransportError TransportProtocolError
                  "failed to encode message"))
              Right bs -> do
                result <- try (doPost t.htManager url bs)
                  :: IO (Either SomeException ())
                case result of
                  Left ex ->
                    pure (Left (TransportError TransportIoError
                      (T.pack ("POST failed: " <> show ex))))
                  Right () -> pure (Right ())

  messages t = pure loop
    where
      loop = do
        item <- S.lift (atomically (readTQueue t.htInbox))
        SP.yield item
        case item of
          Left err | err.transportErrorKind == TransportClosed -> pure ()
          _ -> loop

  close t = do
    alreadyClosed <- atomically $ do
      c <- readTVar t.htIsClosed
      writeTVar t.htIsClosed True
      pure c
    if alreadyClosed
      then pure ()
      else cancel t.htSseThread

doPost :: HTTP.Manager -> Text -> LBS.ByteString -> IO ()
doPost manager url body = do
  req0 <- HTTP.parseRequest (T.unpack url)
  let req = req0
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- HTTP.httpLbs req manager
  let status = HTTP.responseStatus resp
  if Status.statusIsSuccessful status
    then pure ()
    else ioError $ userError $
      "HTTP POST failed with status " <> show (Status.statusCode status)
