{- |
= CodeStar.RepoMap.Worker — incremental background repo-map indexer

The 'RepoMapWorker' runs a background thread that continuously processes
files from a queue, extracting Tree-sitter tags and rebuilding the rendered
repo map whenever enough files have been processed.

== Design

@
  ┌──────────────────────────────────────────────────────┐
  │  background thread (workerLoop)                      │
  │                                                      │
  │  queue ──► processFile ──► WorkerState.tags          │
  │                                     │                │
  │                                     ▼                │
  │              checkShouldRebuild ──► rebuildGraph      │
  │                                     │                │
  │                                     ▼                │
  │                              WorkerState.renderedMap │
  │                                     │                │
  │                                     ▼                │
  │                              rwUpdates (broadcast)   │
  └──────────────────────────────────────────────────────┘
@

== Key choices

  * __Incremental__: the worker processes one file at a time so it can
    interleave with other threads.  The repo map is rebuilt at most every
    2 seconds to avoid thrashing during large edits.
  * __Cache-aware__: 'processCodeFile' checks the tag cache before parsing;
    only files with changed modification times are re-extracted.
  * __Broadcast channel__: 'subscribeToUpdates' returns a 'TChan' that
    receives the rendered map every time it is rebuilt, allowing multiple
    consumers (future IDE/web clients) without extra coupling.
-}
module CodeStar.RepoMap.Worker
  ( -- * Worker handle
    RepoMapWorker (..)
  , newRepoMapWorker
  , stopWorker

    -- * State access
  , WorkerStatus (..)
  , getCurrentMap
  , getWorkerStatus
  , getIndexedFiles
  , subscribeToUpdates

    -- * Manual operations
  , enqueueFile
  , enqueueAll
  , forceRebuild
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay, yield)
import Control.Concurrent.STM
  ( TChan
  , TQueue
  , TVar
  , atomically
  , dupTChan
  , isEmptyTQueue
  , modifyTVar'
  , newBroadcastTChanIO
  , newTQueueIO
  , newTVarIO
  , readTVar
  , tryReadTQueue
  , writeTChan
  , writeTQueue
  )
import Control.Exception (SomeException, try)
import Control.Monad (forM, forever, when)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import CodeStar.RepoMap.Cache (RepoMapCache (..))
import CodeStar.RepoMap.Graph (Tag, ExtractionSkip (..), TagExtraction (..), buildSymbolGraph, defaultWeights, extractTagsDetailed, pageRank)
import CodeStar.RepoMap.Render (RenderConfig (..), defaultRenderConfig, renderRepoMap)
import CodeStar.TreeSitter (GrammarRegistry)

-- --------------------------------------------------------------------
-- Worker State
-- --------------------------------------------------------------------

data WorkerState = WorkerState
  { tags :: !(Map FilePath [Tag])
  -- ^ Accumulated tags per file
  , renderedMap :: !Text
  -- ^ Current rendered map
  , lastRebuild :: !UTCTime
  -- ^ When we last rebuilt the graph
  , pendingFiles :: !(Set FilePath)
  -- ^ Files changed since last rebuild
  }

emptyWorkerState :: IO WorkerState
emptyWorkerState = do
  now <- getCurrentTime
  pure
    WorkerState
      { tags = Map.empty
      , renderedMap = Text.empty
      , lastRebuild = now
      , pendingFiles = Set.empty
      }

-- --------------------------------------------------------------------
-- Worker Handle
-- --------------------------------------------------------------------

data RepoMapWorker = RepoMapWorker
  { rwState :: !(TVar WorkerState)
  , rwQueue :: !(TQueue FilePath)
  -- ^ Files to process
  , rwUpdates :: !(TChan Text)
  -- ^ Broadcast channel for map updates
  , rwThreadId :: !ThreadId
  , rwGrammarReg :: !GrammarRegistry
  , rwCache :: !RepoMapCache
  , rwWorkspace :: !FilePath
  , rwConfig :: !RenderConfig
  }

-- --------------------------------------------------------------------
-- Construction
-- --------------------------------------------------------------------

data WorkerConfig = WorkerConfig
  { wcRebuildIntervalMs :: !Int
  -- ^ Min ms between graph rebuilds
  , wcMaxTokens :: !Int
  -- ^ Token budget for rendered map
  , wcBatchSize :: !Int
  -- ^ Max files to process before checking rebuild
  }

defaultWorkerConfig :: WorkerConfig
defaultWorkerConfig =
  WorkerConfig
    { wcRebuildIntervalMs = 2000 -- Rebuild at most every 2 seconds
    , wcMaxTokens = 4096
    , wcBatchSize = 10
    }

-- | Construct and start a 'RepoMapWorker' for the given workspace.
-- The background indexing thread starts immediately; all workspace files
-- are enqueued for processing in a second background thread so the caller
-- is not blocked.
newRepoMapWorker ::
  GrammarRegistry ->
  RepoMapCache ->
  -- | Workspace root
  FilePath ->
  IO RepoMapWorker
newRepoMapWorker grammarReg cache workspace = do
  state <- emptyWorkerState >>= newTVarIO
  queue <- newTQueueIO
  updates <- newBroadcastTChanIO

  let renderCfg = defaultRenderConfig{maxTokens = defaultWorkerConfig.wcMaxTokens}

  -- Start background worker FIRST (returns immediately)
  tid <- forkIO $ workerLoop state queue updates grammarReg cache renderCfg defaultWorkerConfig

  -- Queue all workspace files for processing (also in background)
  _ <- forkIO $ do
    result <- try @SomeException $ do
      files <- listWorkspaceFiles workspace
      atomically $ mapM_ (writeTQueue queue) files
      pure ()
    case result of
      Left _ -> pure ()
      Right _ -> pure ()

  pure
    RepoMapWorker
      { rwState = state
      , rwQueue = queue
      , rwUpdates = updates
      , rwThreadId = tid
      , rwGrammarReg = grammarReg
      , rwCache = cache
      , rwWorkspace = workspace
      , rwConfig = renderCfg
      }

-- | Kill the background indexing thread.  Safe to call multiple times.
stopWorker :: RepoMapWorker -> IO ()
stopWorker worker = killThread worker.rwThreadId

-- --------------------------------------------------------------------
-- State Access
-- --------------------------------------------------------------------

-- | A snapshot of the worker's indexing progress, returned by 'getWorkerStatus'.
data WorkerStatus = WorkerStatus
  { indexedFileCount :: !Int
  -- ^ Number of files that have been processed (tags extracted or empty-tagged).
  , totalTagCount    :: !Int
  -- ^ Total number of symbol tags across all indexed files.
  , pendingFileCount :: !Int
  -- ^ Files changed since the last graph rebuild; non-zero means the
  --   rendered map may be stale.
  , queueIsEmpty     :: !Bool
  -- ^ 'False' while files are waiting to be processed.
  }

-- | Read the most recently rendered repo map.  Returns 'Text.empty' if
-- no rebuild has completed yet (the worker has just started).
getCurrentMap :: RepoMapWorker -> IO Text
getCurrentMap worker = atomically $ do
  state <- readTVar worker.rwState
  pure state.renderedMap

-- | Sample the worker's current indexing state without blocking.
getWorkerStatus :: RepoMapWorker -> IO WorkerStatus
getWorkerStatus worker = atomically $ do
  state <- readTVar worker.rwState
  empty <- isEmptyTQueue worker.rwQueue
  let totalTags = sum (map length (Map.elems state.tags))
  pure WorkerStatus
    { indexedFileCount = Map.size state.tags
    , totalTagCount    = totalTags
    , pendingFileCount = Set.size state.pendingFiles
    , queueIsEmpty     = empty
    }

-- | Return the list of files that have been processed (whether or not
-- tags were successfully extracted).
getIndexedFiles :: RepoMapWorker -> IO [FilePath]
getIndexedFiles worker = do
  state <- atomically $ readTVar worker.rwState
  pure (Map.keys state.tags)

-- | Subscribe to the rendered-map broadcast channel.  Each rebuild writes
-- the new rendered text to this channel.  Use 'atomically readTChan' to
-- wait for the next update.
subscribeToUpdates :: RepoMapWorker -> IO (TChan Text)
subscribeToUpdates worker = atomically $ dupTChan worker.rwUpdates

-- --------------------------------------------------------------------
-- Manual Operations
-- --------------------------------------------------------------------

-- | Add a single file to the processing queue.  The file will be
-- (re-)indexed on the next batch cycle.  Call this after editing a file
-- to keep the repo map up to date.
enqueueFile :: RepoMapWorker -> FilePath -> IO ()
enqueueFile worker path = atomically $ writeTQueue worker.rwQueue path

-- | Enqueue every file in the workspace for re-indexing.
-- Use this after a large refactor or when the repo map looks stale.
enqueueAll :: RepoMapWorker -> IO ()
enqueueAll worker = do
  files <- listWorkspaceFiles worker.rwWorkspace
  atomically $ mapM_ (writeTQueue worker.rwQueue) files

-- | Rebuild the rendered repo map immediately from the current tag index,
-- bypassing the debounce interval.  Used in tests and the @\/rescan@
-- REPL command to get a fresh map on demand.
forceRebuild :: RepoMapWorker -> IO ()
forceRebuild worker = do
  state <- atomically $ readTVar worker.rwState
  let allTags = concat (Map.elems state.tags)
      graph = buildSymbolGraph allTags
      scores = pageRank graph [] [] defaultWeights
      rendered = renderRepoMap allTags scores graph worker.rwConfig
  now <- getCurrentTime
  atomically $ do
    modifyTVar' worker.rwState $ \s ->
      s
        { renderedMap = rendered
        , lastRebuild = now
        , pendingFiles = Set.empty
        }
    writeTChan worker.rwUpdates rendered

-- --------------------------------------------------------------------
-- Background Worker Loop
-- --------------------------------------------------------------------

workerLoop ::
  TVar WorkerState ->
  TQueue FilePath ->
  TChan Text ->
  GrammarRegistry ->
  RepoMapCache ->
  RenderConfig ->
  WorkerConfig ->
  IO ()
workerLoop stateVar queue updates grammarReg cache renderCfg cfg = forever $ do
  -- Yield to other threads regularly
  yield

  -- Process a batch of files
  processedCount <- processBatch cfg.wcBatchSize

  -- Check if we should rebuild (either we processed files, or we have pending changes)
  when (processedCount > 0) $ do
    shouldRebuild <- checkShouldRebuild cfg.wcRebuildIntervalMs
    when shouldRebuild rebuildGraph
    yield -- Yield after rebuild

  -- If queue is empty, check for any pending rebuilds then sleep
  isEmpty <- atomically $ isEmptyTQueue queue
  when isEmpty $ do
    hasPending <- atomically $ do
      s <- readTVar stateVar
      pure (not (Set.null s.pendingFiles))
    when hasPending rebuildGraph
    threadDelay 100000 -- 100ms idle sleep
 where
  processBatch :: Int -> IO Int
  processBatch 0 = pure 0
  processBatch remaining = do
    mPath <- atomically $ tryReadTQueue queue
    case mPath of
      Nothing -> pure 0
      Just path -> do
        processFile path
        (1 +) <$> processBatch (remaining - 1)

  processFile :: FilePath -> IO ()
  processFile path
    | takeExtension path `notElem` codeExtensions = recordUnsupported path
    | otherwise = do
        -- 5-second timeout keeps the worker responsive on slow or large files.
        result <- timeout 5000000 $ try @SomeException (processCodeFile path)
        case result of
          Nothing        -> markFailed path  -- timed out
          Just (Left _)  -> markFailed path  -- exception
          Just (Right _) -> pure ()

  -- | Process a file whose extension is in 'codeExtensions'.
  processCodeFile :: FilePath -> IO ()
  processCodeFile path = do
    exists <- doesFileExist path
    if not exists
      then removeFile' path
      else do
        mtimeResult <- try @SomeException (getModificationTime path)
        case mtimeResult of
          Right mtime -> do
            cached <- cache.getTags path mtime
            case cached of
              Just tags -> storeTags' path tags (Just mtime)
              Nothing   -> extractAndStore path (Just mtime)
          Left _ -> extractAndStore path Nothing

  -- | Extract tags from @path@, cache them under @mMtime@, and store in state.
  extractAndStore :: FilePath -> Maybe UTCTime -> IO ()
  extractAndStore path mMtime = do
    src <- BS.readFile path
    outcome <- extractTagsDetailed grammarReg path src
    case outcome of
      Extracted tags -> do
        case mMtime of
          Just mtime -> cache.putTags path mtime tags
          Nothing    -> pure ()
        storeTags' path tags mMtime
      Skipped _ ->
        -- Expected: no extractor or grammar for this file type.
        markFailed path
      ExtractFailed reason -> do
        hPutStrLn stderr ("[codestar] extract failed for " <> path <> ": " <> Text.unpack reason)
        markFailed path

  -- | Record a non-code file with empty tags so it appears as indexed.
  recordUnsupported :: FilePath -> IO ()
  recordUnsupported path = atomically $ modifyTVar' stateVar $ \s ->
    s { tags = Map.insert path [] s.tags
      , pendingFiles = Set.insert path s.pendingFiles
      }

  -- | Remove a deleted file from the tag index.
  removeFile' :: FilePath -> IO ()
  removeFile' path = atomically $ modifyTVar' stateVar $ \s ->
    s { tags = Map.delete path s.tags
      , pendingFiles = Set.insert path s.pendingFiles
      }

  -- | Write successfully extracted tags into the state.
  -- @mMtime@ is unused here but kept for symmetry with 'extractAndStore'.
  storeTags' :: FilePath -> [Tag] -> Maybe UTCTime -> IO ()
  storeTags' path tags _mMtime = atomically $ modifyTVar' stateVar $ \s ->
    s { tags = Map.insert path tags s.tags
      , pendingFiles = Set.insert path s.pendingFiles
      }

  -- | Mark a file as failed: preserve any previously extracted tags so the
  -- file still counts as indexed; insert it into pending for the next rebuild.
  markFailed :: FilePath -> IO ()
  markFailed path = atomically $ modifyTVar' stateVar $ \s ->
    s { tags = Map.insertWith (\_ old -> old) path [] s.tags
      , pendingFiles = Set.insert path s.pendingFiles
      }

  codeExtensions = [".hs", ".py", ".js", ".ts", ".tsx", ".rs", ".go", ".c", ".cpp", ".java", ".rb", ".swift", ".kt", ".scala", ".ex", ".exs", ".lua", ".pl", ".r", ".R"]

  takeExtension p = case break (== '.') (reverse p) of
    (ext, _ : _) -> '.' : reverse ext
    _ -> ""

  checkShouldRebuild :: Int -> IO Bool
  checkShouldRebuild intervalMs = do
    now <- getCurrentTime
    state <- atomically $ readTVar stateVar
    let elapsed = diffTimeMs now state.lastRebuild
    pure (elapsed >= intervalMs && not (Set.null state.pendingFiles))

  rebuildGraph :: IO ()
  rebuildGraph = do
    state <- atomically $ readTVar stateVar
    let allTags = concat (Map.elems state.tags)
        graph = buildSymbolGraph allTags
        scores = pageRank graph [] [] defaultWeights
        rendered = renderRepoMap allTags scores graph renderCfg
    now <- getCurrentTime
    atomically $ do
      modifyTVar' stateVar $ \s ->
        s
          { renderedMap = rendered
          , lastRebuild = now
          , pendingFiles = Set.empty
          }
      writeTChan updates rendered

diffTimeMs :: UTCTime -> UTCTime -> Int
diffTimeMs t1 t2 =
  let diff = realToFrac (diffUTCTime t1 t2) :: Double
   in round (diff * 1000)

-- --------------------------------------------------------------------
-- File Listing
-- --------------------------------------------------------------------

listWorkspaceFiles :: FilePath -> IO [FilePath]
listWorkspaceFiles root = go root
 where
  go dir = do
    entries <- listDirectory dir
    let visible = filter (not . shouldSkip) entries
    fmap concat $ forM visible $ \name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir
        then go path
        else do
          isFile <- doesFileExist path
          if isFile then pure [path] else pure []

  shouldSkip name =
    ("." `isPrefixOf` name)
      || name `elem` excludedDirs -- dotfiles/dirs
  excludedDirs =
    [ "dist-newstyle" -- Cabal build
    , "dist" -- Old cabal
    , ".stack-work" -- Stack build
    , "node_modules" -- JS deps
    , "__pycache__" -- Python cache
    , ".git" -- Git
    , "target" -- Rust/Java build
    , "build" -- Generic build
    , ".cache" -- Cache dirs
    , "vendor" -- Vendored deps
    ]
