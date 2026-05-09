module CodeStar.RepoMap.Worker
  ( -- * Worker handle
    RepoMapWorker (..)
  , newRepoMapWorker
  , stopWorker

    -- * State access
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
import System.Timeout (timeout)

import CodeStar.RepoMap.Cache (RepoMapCache (..))
import CodeStar.RepoMap.Graph (Tag, TagExtraction (..), buildSymbolGraph, defaultWeights, extractTagsDetailed, pageRank)
import CodeStar.RepoMap.Render (RenderConfig (..), defaultRenderConfig, renderRepoMap)
import CodeStar.TreeSitter (GrammarRegistry)

-- --------------------------------------------------------------------
-- Worker State
-- --------------------------------------------------------------------

data WorkerState = WorkerState
  { wsTags :: !(Map FilePath [Tag])
  -- ^ Accumulated tags per file
  , wsRendered :: !Text
  -- ^ Current rendered map
  , wsLastRebuild :: !UTCTime
  -- ^ When we last rebuilt the graph
  , wsPendingFiles :: !(Set FilePath)
  -- ^ Files changed since last rebuild
  }

emptyWorkerState :: IO WorkerState
emptyWorkerState = do
  now <- getCurrentTime
  pure
    WorkerState
      { wsTags = Map.empty
      , wsRendered = Text.empty
      , wsLastRebuild = now
      , wsPendingFiles = Set.empty
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

stopWorker :: RepoMapWorker -> IO ()
stopWorker worker = killThread worker.rwThreadId

-- --------------------------------------------------------------------
-- State Access
-- --------------------------------------------------------------------

getCurrentMap :: RepoMapWorker -> IO Text
getCurrentMap worker = atomically $ do
  state <- readTVar worker.rwState
  pure state.wsRendered

getWorkerStatus :: RepoMapWorker -> IO (Int, Int, Int, Int)
getWorkerStatus worker = do
  state <- atomically $ readTVar worker.rwState
  queueLen <- atomically $ queueLength worker.rwQueue
  let totalTags = sum (map length (Map.elems state.wsTags))
  pure (Map.size state.wsTags, totalTags, Set.size state.wsPendingFiles, queueLen)
 where
  queueLength q = do
    empty <- isEmptyTQueue q
    if empty then pure 0 else pure (-1) -- Can't easily get length, just empty/non-empty

getIndexedFiles :: RepoMapWorker -> IO [FilePath]
getIndexedFiles worker = do
  state <- atomically $ readTVar worker.rwState
  pure (Map.keys state.wsTags)

subscribeToUpdates :: RepoMapWorker -> IO (TChan Text)
subscribeToUpdates worker = atomically $ dupTChan worker.rwUpdates

-- --------------------------------------------------------------------
-- Manual Operations
-- --------------------------------------------------------------------

enqueueFile :: RepoMapWorker -> FilePath -> IO ()
enqueueFile worker path = atomically $ writeTQueue worker.rwQueue path

enqueueAll :: RepoMapWorker -> IO ()
enqueueAll worker = do
  files <- listWorkspaceFiles worker.rwWorkspace
  atomically $ mapM_ (writeTQueue worker.rwQueue) files

forceRebuild :: RepoMapWorker -> IO ()
forceRebuild worker = do
  state <- atomically $ readTVar worker.rwState
  let allTags = concat (Map.elems state.wsTags)
      graph = buildSymbolGraph allTags
      scores = pageRank graph [] [] defaultWeights
      rendered = renderRepoMap allTags scores graph worker.rwConfig
  now <- getCurrentTime
  atomically $ do
    modifyTVar' worker.rwState $ \s ->
      s
        { wsRendered = rendered
        , wsLastRebuild = now
        , wsPendingFiles = Set.empty
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
      pure (not (Set.null s.wsPendingFiles))
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
  processFile path = do
    let markFailed = atomically $ modifyTVar' stateVar $ \s ->
          s
            { wsTags =
                -- Keep previously extracted tags when available; only fall back to empty tags
                -- for first-time indexing failures so the file still counts as indexed.
                Map.insertWith (\_ old -> old) path [] s.wsTags
            , wsPendingFiles = Set.insert path s.wsPendingFiles
            }
        extractAndStore mMtime = do
          src <- BS.readFile path
          outcome <- extractTagsDetailed grammarReg path src
          case outcome of
            Extracted tags -> do
              case mMtime of
                Just mtime -> cache.putTags path mtime tags
                Nothing -> pure ()
              atomically $ modifyTVar' stateVar $ \s ->
                s
                  { wsTags = Map.insert path tags s.wsTags
                  , wsPendingFiles = Set.insert path s.wsPendingFiles
                  }
            _ ->
              markFailed
    -- Skip non-code files early
    let ext = takeExtension path
    if ext `notElem` codeExtensions
      then do
        -- Still record the file but with empty tags
        atomically $ modifyTVar' stateVar $ \s ->
          s
            { wsTags = Map.insert path [] s.wsTags
            , wsPendingFiles = Set.insert path s.wsPendingFiles
            }
      else do
        -- Timeout after 5 seconds per file to keep indexing responsive.
        result <- timeout 5000000 $ try @SomeException $ do
          exists <- doesFileExist path
          if not exists
            then do
              atomically $ modifyTVar' stateVar $ \s ->
                s
                  { wsTags = Map.delete path s.wsTags
                  , wsPendingFiles = Set.insert path s.wsPendingFiles
                  }
            else do
              mtimeResult <- try @SomeException (getModificationTime path)
              case mtimeResult of
                Right mtime -> do
                  cached <- cache.getTags path mtime
                  case cached of
                    Just tags ->
                      atomically $ modifyTVar' stateVar $ \s ->
                        s
                          { wsTags = Map.insert path tags s.wsTags
                          , wsPendingFiles = Set.insert path s.wsPendingFiles
                          }
                    Nothing -> extractAndStore (Just mtime)
                Left _ -> extractAndStore Nothing
        case result of
          Nothing -> do
            markFailed
          Just (Left _) -> do
            markFailed
          Just (Right _) -> pure ()

  codeExtensions = [".hs", ".py", ".js", ".ts", ".tsx", ".rs", ".go", ".c", ".cpp", ".java", ".rb", ".swift", ".kt", ".scala", ".ex", ".exs", ".lua", ".pl", ".r", ".R"]

  takeExtension p = case break (== '.') (reverse p) of
    (ext, _ : _) -> '.' : reverse ext
    _ -> ""

  checkShouldRebuild :: Int -> IO Bool
  checkShouldRebuild intervalMs = do
    now <- getCurrentTime
    state <- atomically $ readTVar stateVar
    let elapsed = diffTimeMs now state.wsLastRebuild
    pure (elapsed >= intervalMs && not (Set.null state.wsPendingFiles))

  rebuildGraph :: IO ()
  rebuildGraph = do
    state <- atomically $ readTVar stateVar
    let allTags = concat (Map.elems state.wsTags)
        graph = buildSymbolGraph allTags
        scores = pageRank graph [] [] defaultWeights
        rendered = renderRepoMap allTags scores graph renderCfg
    now <- getCurrentTime
    atomically $ do
      modifyTVar' stateVar $ \s ->
        s
          { wsRendered = rendered
          , wsLastRebuild = now
          , wsPendingFiles = Set.empty
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
