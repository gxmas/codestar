{-# LANGUAGE CPP #-}
-- |
-- Module      : Network.MCP.Transport.Stdio
-- Stability   : experimental
--
-- Stdio transport for MCP. Spawns a subprocess and communicates via
-- newline-delimited JSON over its stdin/stdout.
module Network.MCP.Transport.Stdio
  ( StdioTransport
  , new
  , newWithHandles
  , stdioStderr
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TQueue, TVar
  , atomically, isEmptyTQueue, newTQueueIO, newTVarIO
  , readTQueue, readTVar, writeTQueue, writeTVar
  )
import Control.Exception (SomeException, catch, try)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Streaming as S
import qualified Streaming.Prelude as SP
import System.IO
  ( Handle, BufferMode (..), hClose, hFlush, hIsEOF, hSetBinaryMode
  , hSetBuffering
  )
import System.Process
  ( CreateProcess (..), ProcessHandle, StdStream (..)
  , createProcess, getPid, proc, terminateProcess, waitForProcess
  )
import System.Timeout (timeout)
#ifndef mingw32_HOST_OS
import System.Posix.Signals (sigKILL, signalProcess)
#endif

import Network.MCP.Codec (McpCodec (..), Codec (..))
import Network.MCP.Transport
  ( Transport (..), TransportError (..), TransportErrorKind (..)
  )
import Network.MCP.Types (MCPMessage)

-- | A stdio transport that communicates with a subprocess via
-- newline-delimited JSON on stdin/stdout.
data StdioTransport = StdioTransport
  { stStdin        :: !Handle          -- ^ subprocess stdin (we write)
  , stStdout       :: !Handle          -- ^ subprocess stdout (we read)
  , stStderr       :: !(Maybe Handle)  -- ^ subprocess stderr handle (Nothing for newWithHandles)
  , stProcess      :: !(Maybe ProcessHandle)
  , stInbox        :: !(TQueue (Either TransportError MCPMessage))
  , stIsClosed     :: !(TVar Bool)
  , stReader       :: !(Async ())
  , stStderrQueue  :: !(TQueue Text)        -- ^ lines drained from stderr
  , stStderrReader :: !(Async ())           -- ^ background stderr drain thread
  }

-- | Spawn a subprocess and create a stdio transport to communicate with it.
new :: FilePath -> [String] -> IO StdioTransport
new cmd args = do
  let cp = (proc cmd args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (Just hin, Just hout, Just herr, ph) <- createProcess cp
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  hSetBuffering hin LineBuffering
  hSetBuffering hout LineBuffering
  inbox <- newTQueueIO
  closed <- newTVarIO False
  reader <- async (readerLoop hout inbox closed)
  stderrQ <- newTQueueIO
  stderrReader <- async (stderrDrainLoop herr stderrQ)
  pure StdioTransport
    { stStdin        = hin
    , stStdout       = hout
    , stStderr       = Just herr
    , stProcess      = Just ph
    , stInbox        = inbox
    , stIsClosed     = closed
    , stReader       = reader
    , stStderrQueue  = stderrQ
    , stStderrReader = stderrReader
    }

-- | Create a stdio transport from pre-existing handles. Useful for
-- testing with pipes. No subprocess is managed; 'close' will close
-- the handles but not terminate any process.
newWithHandles :: Handle -> Handle -> IO StdioTransport
newWithHandles writeHandle readHandle = do
  hSetBinaryMode writeHandle True
  hSetBinaryMode readHandle True
  hSetBuffering writeHandle LineBuffering
  hSetBuffering readHandle LineBuffering
  inbox <- newTQueueIO
  closed <- newTVarIO False
  reader <- async (readerLoop readHandle inbox closed)
  stderrQ <- newTQueueIO
  stderrReader <- async (pure ())
  pure StdioTransport
    { stStdin        = writeHandle
    , stStdout       = readHandle
    , stStderr       = Nothing
    , stProcess      = Nothing
    , stInbox        = inbox
    , stIsClosed     = closed
    , stReader       = reader
    , stStderrQueue  = stderrQ
    , stStderrReader = stderrReader
    }

-- | Background reader loop: reads newline-delimited JSON from stdout,
-- decodes via McpCodec, enqueues into inbox. Terminates on EOF,
-- exception, or transport close.
readerLoop :: Handle -> TQueue (Either TransportError MCPMessage) -> TVar Bool -> IO ()
readerLoop hout inbox closed = go
  where
    go = do
      isClosed <- atomically (readTVar closed)
      if isClosed
        then pure ()
        else do
          eof <- hIsEOF hout
          if eof
            then enqueueClose "subprocess stdout closed (EOF)"
            else do
              result <- try (BS8.hGetLine hout) :: IO (Either SomeException BS8.ByteString)
              case result of
                Left ex ->
                  enqueueClose (T.pack ("I/O error reading from subprocess: " <> show ex))
                Right sline
                  | BS8.null sline -> go  -- skip empty lines
                  | otherwise ->
                      let line = LBS.fromStrict sline
                       in case decode McpCodec line of
                        Left _codecErr ->
                          enqueueError TransportProtocolError
                            ("Failed to decode message: " <> LBS8.unpack (LBS.take 200 line))
                        Right msg -> do
                          atomically (writeTQueue inbox (Right msg))
                          go

    enqueueClose detail = atomically $ do
      writeTQueue inbox (Left (TransportError TransportClosed detail))

    enqueueError kind detail = do
      atomically $ writeTQueue inbox
        (Left (TransportError kind (T.pack detail)))
      go

instance Transport StdioTransport where
  send t msg = do
    isClosed <- atomically (readTVar t.stIsClosed)
    if isClosed
      then pure (Left (TransportError TransportClosed "transport is closed"))
      else case encode McpCodec msg of
        Left _codecErr ->
          pure (Left (TransportError TransportProtocolError "failed to encode message"))
        Right bs -> do
          result <- try (do
            LBS.hPut t.stStdin (bs <> "\n")
            hFlush t.stStdin
            ) :: IO (Either SomeException ())
          case result of
            Left ex ->
              pure (Left (TransportError TransportIoError
                (T.pack ("send failed: " <> show ex))))
            Right () -> pure (Right ())

  messages t = pure $ loop
    where
      loop = do
        item <- S.lift (atomically (readTQueue t.stInbox))
        SP.yield item
        case item of
          Left err | err.transportErrorKind == TransportClosed -> pure ()
          _ -> loop

  close t = do
    alreadyClosed <- atomically $ do
      c <- readTVar t.stIsClosed
      writeTVar t.stIsClosed True
      pure c
    if alreadyClosed
      then pure ()
      else do
        cancel t.stReader
        cancel t.stStderrReader
        case t.stProcess of
          Nothing -> do
            -- Handle-only transport: just close handles
            hClose t.stStdin  `catch` ignoreAll
            hClose t.stStdout `catch` ignoreAll
            case t.stStderr of
              Just herr -> hClose herr `catch` ignoreAll
              Nothing   -> pure ()
          Just ph -> do
            -- Step 1: Close stdin to signal EOF to subprocess
            hClose t.stStdin `catch` ignoreAll
            -- Step 2: Wait up to 5 s for graceful exit
            mExit <- timeout 5_000_000 (waitForProcess ph)
            case mExit of
              Just _  -> pure ()  -- Subprocess exited cleanly
              Nothing -> do
                -- Step 3: SIGTERM (Unix) / TerminateProcess (Windows)
                terminateProcess ph `catch` ignoreAll
                -- Step 4: Wait up to 2 s for SIGTERM to take effect
                mExit2 <- timeout 2_000_000 (waitForProcess ph)
                case mExit2 of
                  Just _ -> pure ()
                  Nothing ->
#ifndef mingw32_HOST_OS
                    -- Step 5 (Unix only): escalate to SIGKILL
                    do mPid <- getPid ph
                       case mPid of
                         Just pid -> signalProcess sigKILL pid `catch` ignoreAll
                         Nothing  -> pure ()
#else
                    pure ()
#endif
            hClose t.stStdout `catch` ignoreAll
            case t.stStderr of
              Just herr -> hClose herr `catch` ignoreAll
              Nothing   -> pure ()

-- | Read one line from the subprocess stderr buffer without blocking.
-- Returns 'Nothing' when the buffer is empty.
stdioStderr :: StdioTransport -> IO (Maybe Text)
stdioStderr t = atomically $ do
  empty <- isEmptyTQueue t.stStderrQueue
  if empty then pure Nothing else Just <$> readTQueue t.stStderrQueue

stderrDrainLoop :: Handle -> TQueue Text -> IO ()
stderrDrainLoop herr q = go
  where
    go = do
      eof <- hIsEOF herr `catch` (\(_ :: SomeException) -> pure True)
      if eof
        then pure ()
        else do
          line <- try (BS8.hGetLine herr)
                    :: IO (Either SomeException BS8.ByteString)
          case line of
            Left _  -> pure ()
            Right bs -> do
              atomically (writeTQueue q (TE.decodeUtf8Lenient bs))
              go

ignoreAll :: SomeException -> IO ()
ignoreAll _ = pure ()
