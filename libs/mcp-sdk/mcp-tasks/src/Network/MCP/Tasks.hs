-- |
-- Module      : Network.MCP.Tasks
-- Stability   : stable
--
-- Task store and session integration for the Model Context Protocol.
module Network.MCP.Tasks
  ( -- * Task record
    Task (..)
    -- * Task store
  , TaskStore
  , TaskStoreConfig (..)
  , defaultTaskStoreConfig
  , newTaskStore
  , closeTaskStore
    -- * Operations
  , createTask
  , getTask
  , listTasks
  , cancelTask
  , completeTask
  , failTask
    -- * Session integration
  , TaskFeature
  , newTaskFeature
  , attach
  , detach
    -- * Exported for testing
  , offsetToCursor
  , cursorToOffset
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TVar, TMVar
  , atomically
  , newTVarIO, readTVar, writeTVar, readTVarIO
  , newEmptyTMVarIO, tryPutTMVar
  )
import Control.Monad (forever, forM_, unless)
import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Network.MCP.Types (RPCError (..), Cursor (..))
import Network.MCP.Types.Capabilities
  ( TaskStatus (..)
  , TaskError (..)
  , TaskErrorKind (..)
  , AuthContext
  , TaskHandle (isActive, update, complete, fail_)
  , TaskFactory
  , SomeTaskHandle (..)
  )
-- Qualified import so we can implement TaskHandle.taskId and TaskFactory.createTask
-- in instance declarations without clashing with same-named exports of this module.
import qualified Network.MCP.Types.Capabilities as Cap
import Network.MCP.Session (Session (..))

------------------------------------------------------------------------
-- Task (external-facing record)
------------------------------------------------------------------------

data Task = Task
  { taskId      :: !Text
  , taskStatus  :: !TaskStatus
  , taskMessage :: !(Maybe Text)
  , taskCreatedAt :: !Text
  , taskUpdatedAt :: !Text
  , taskTtl     :: !Word
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Task where
  toJSON t =
    Aeson.object $
      [ "id"        .= t.taskId
      , "status"    .= t.taskStatus
      , "createdAt" .= t.taskCreatedAt
      , "updatedAt" .= t.taskUpdatedAt
      , "ttl"       .= t.taskTtl
      ]
      ++ maybe [] (\m -> ["message" .= m]) t.taskMessage
  toEncoding t =
    E.pairs $
      "id"        .= t.taskId
      <> "status"    .= t.taskStatus
      <> "createdAt" .= t.taskCreatedAt
      <> "updatedAt" .= t.taskUpdatedAt
      <> "ttl"       .= t.taskTtl
      <> foldMap ("message" .=) t.taskMessage

instance Aeson.FromJSON Task where
  parseJSON = Aeson.withObject "Task" $ \o ->
    Task
      <$> o .:  "id"
      <*> o .:  "status"
      <*> o .:? "message"
      <*> o .:  "createdAt"
      <*> o .:  "updatedAt"
      <*> o .:  "ttl"

------------------------------------------------------------------------
-- TaskStoreConfig
------------------------------------------------------------------------

data TaskStoreConfig = TaskStoreConfig
  { taskDefaultTtl :: !Word   -- milliseconds
  , taskMaxTasks   :: !Word   -- max concurrent tasks
  }
  deriving stock (Eq, Show, Generic)

defaultTaskStoreConfig :: TaskStoreConfig
defaultTaskStoreConfig = TaskStoreConfig
  { taskDefaultTtl = 3600000
  , taskMaxTasks   = 10000
  }

------------------------------------------------------------------------
-- ConcreteTask (internal)
------------------------------------------------------------------------

data ConcreteTask = ConcreteTask
  { ctId        :: !Text
  , ctStatus    :: !(TVar TaskStatus)
  , ctMessage   :: !(TVar (Maybe Text))
  , ctResult    :: !(TMVar (Either RPCError Aeson.Value))
  , ctCreatedAt :: !UTCTime
  , ctUpdatedAt :: !(TVar UTCTime)
  , ctTtl       :: !Word
  , ctAuthCtx   :: !(Maybe AuthContext)
  }

------------------------------------------------------------------------
-- TaskStore
------------------------------------------------------------------------

data TaskStore = TaskStore
  { tsTasks     :: !(TVar (HashMap Text ConcreteTask))
  , tsConfig    :: !TaskStoreConfig
  , tsTtlThread :: !(Async ())
  }

newTaskStore :: TaskStoreConfig -> IO TaskStore
newTaskStore cfg = do
  tasksVar <- newTVarIO HM.empty
  thread   <- async (ttlThread tasksVar)
  pure TaskStore
    { tsTasks     = tasksVar
    , tsConfig    = cfg
    , tsTtlThread = thread
    }

closeTaskStore :: TaskStore -> IO ()
closeTaskStore store = cancel store.tsTtlThread

------------------------------------------------------------------------
-- TTL background thread
------------------------------------------------------------------------

ttlThread :: TVar (HashMap Text ConcreteTask) -> IO ()
ttlThread tasksVar = forever $ do
  threadDelay 60_000_000  -- check every 60 seconds
  now <- getCurrentTime
  atomically $ do
    tasks <- readTVar tasksVar
    let expired = HM.filter (isExpired now) tasks
    forM_ expired $ \t -> do
      status <- readTVar t.ctStatus
      unless (isTerminal status) $ do
        writeTVar t.ctStatus TaskFailed
        writeTVar t.ctMessage (Just "Task expired (TTL)")
        _ <- tryPutTMVar t.ctResult (Left (RPCError (-32603) "Task expired" Nothing))
        pure ()
  where
    isExpired now task =
      let ttlSeconds = fromIntegral task.ctTtl / 1000.0 :: Double
          deadline   = addUTCTime (realToFrac ttlSeconds) task.ctCreatedAt
      in now > deadline

------------------------------------------------------------------------
-- Status helpers
------------------------------------------------------------------------

isTerminal :: TaskStatus -> Bool
isTerminal TaskCompleted  = True
isTerminal TaskFailed     = True
isTerminal TaskCancelled  = True
isTerminal _              = False

------------------------------------------------------------------------
-- Auth check
------------------------------------------------------------------------

checkAuth :: Maybe AuthContext -> ConcreteTask -> Either TaskError ()
checkAuth requestAuth task = case task.ctAuthCtx of
  Nothing      -> Right ()
  Just taskAuth -> case requestAuth of
    Nothing     -> Left (TaskError TaskAccessDenied "Authentication required")
    Just reqAuth ->
      if reqAuth == taskAuth
        then Right ()
        else Left (TaskError TaskAccessDenied "Access denied")

------------------------------------------------------------------------
-- Helper: toTask
------------------------------------------------------------------------

toTask :: ConcreteTask -> IO Task
toTask ct = do
  status    <- readTVarIO ct.ctStatus
  msg       <- readTVarIO ct.ctMessage
  updatedAt <- readTVarIO ct.ctUpdatedAt
  pure (Task
    { taskId        = ct.ctId
    , taskStatus    = status
    , taskMessage   = msg
    , taskCreatedAt = formatUTC ct.ctCreatedAt
    , taskUpdatedAt = formatUTC updatedAt
    , taskTtl       = ct.ctTtl
    } :: Task)

formatUTC :: UTCTime -> Text
formatUTC = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

------------------------------------------------------------------------
-- Pagination helpers
------------------------------------------------------------------------

pageSize :: Int
pageSize = 50

-- Encode an Int offset as a Cursor (base-10 text).
offsetToCursor :: Int -> Cursor
offsetToCursor = Cursor . T.pack . show

cursorToOffset :: Cursor -> Maybe Int
cursorToOffset (Cursor t) = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing

------------------------------------------------------------------------
-- Operations
------------------------------------------------------------------------

createTask :: TaskStore -> Maybe AuthContext -> IO (Either TaskError Task)
createTask store authCtx = do
  now <- getCurrentTime
  tasks <- readTVarIO store.tsTasks
  let count = HM.size tasks
  if fromIntegral count >= store.tsConfig.taskMaxTasks
    then pure $ Left (TaskError TaskAlreadyTerminal "Maximum task limit reached")
    else do
      uuid      <- nextRandom
      let tid   = UUID.toText uuid
      statusVar <- newTVarIO TaskWorking
      msgVar    <- newTVarIO Nothing
      resultVar <- newEmptyTMVarIO
      updVar    <- newTVarIO now
      let ct = ConcreteTask
                 { ctId        = tid
                 , ctStatus    = statusVar
                 , ctMessage   = msgVar
                 , ctResult    = resultVar
                 , ctCreatedAt = now
                 , ctUpdatedAt = updVar
                 , ctTtl       = store.tsConfig.taskDefaultTtl
                 , ctAuthCtx   = authCtx
                 }
      atomically $ do
        m <- readTVar store.tsTasks
        writeTVar store.tsTasks (HM.insert tid ct m)
      Right <$> toTask ct

getTask :: TaskStore -> Text -> Maybe AuthContext -> IO (Either TaskError Task)
getTask store tid reqAuth = do
  tasks <- readTVarIO store.tsTasks
  case HM.lookup tid tasks of
    Nothing -> pure $ Left (TaskError TaskNotFound "Task not found")
    Just ct ->
      case checkAuth reqAuth ct of
        Left e  -> pure (Left e)
        Right _ -> Right <$> toTask ct

listTasks :: TaskStore -> Maybe AuthContext -> Maybe Cursor -> IO (Either TaskError ([Task], Maybe Cursor))
listTasks store reqAuth mCursor = do
  tasks <- readTVarIO store.tsTasks
  let visible = filter (isVisible reqAuth) (HM.elems tasks)
      sorted  = sortBy (comparing (Down . (.ctCreatedAt))) visible
      offset  = maybe 0 (\c -> maybe 0 id (cursorToOffset c)) mCursor
      page    = take pageSize (drop offset sorted)
      nextCur = if length page < pageSize
                  then Nothing
                  else Just (offsetToCursor (offset + pageSize))
  taskList <- mapM toTask page
  pure $ Right (taskList, nextCur)
  where
    isVisible Nothing  _  = True
    isVisible (Just _) ct = case ct.ctAuthCtx of
      Nothing      -> True
      Just taskAuth -> case reqAuth of
        Nothing     -> False
        Just rAuth  -> rAuth == taskAuth

cancelTask :: TaskStore -> Text -> Maybe AuthContext -> IO (Either TaskError Task)
cancelTask store tid reqAuth = do
  now <- getCurrentTime
  tasks <- readTVarIO store.tsTasks
  case HM.lookup tid tasks of
    Nothing -> pure $ Left (TaskError TaskNotFound "Task not found")
    Just ct ->
      case checkAuth reqAuth ct of
        Left e  -> pure (Left e)
        Right _ -> do
          result <- atomically $ do
            status <- readTVar ct.ctStatus
            if isTerminal status
              then pure $ Left (TaskError TaskAlreadyTerminal "Task already in terminal state")
              else do
                writeTVar ct.ctStatus TaskCancelled
                writeTVar ct.ctUpdatedAt now
                pure (Right ())
          case result of
            Left e  -> pure (Left e)
            Right _ -> Right <$> toTask ct

completeTask :: TaskStore -> Text -> Aeson.Value -> Maybe AuthContext -> IO (Either TaskError Task)
completeTask store tid val reqAuth = do
  now <- getCurrentTime
  tasks <- readTVarIO store.tsTasks
  case HM.lookup tid tasks of
    Nothing -> pure $ Left (TaskError TaskNotFound "Task not found")
    Just ct ->
      case checkAuth reqAuth ct of
        Left e  -> pure (Left e)
        Right _ -> do
          result <- atomically $ do
            status <- readTVar ct.ctStatus
            if isTerminal status
              then pure $ Left (TaskError TaskAlreadyTerminal "Task already in terminal state")
              else do
                writeTVar ct.ctStatus TaskCompleted
                writeTVar ct.ctUpdatedAt now
                _ <- tryPutTMVar ct.ctResult (Right val)
                pure (Right ())
          case result of
            Left e  -> pure (Left e)
            Right _ -> Right <$> toTask ct

failTask :: TaskStore -> Text -> RPCError -> Maybe AuthContext -> IO (Either TaskError Task)
failTask store tid rpcErr reqAuth = do
  now <- getCurrentTime
  tasks <- readTVarIO store.tsTasks
  case HM.lookup tid tasks of
    Nothing -> pure $ Left (TaskError TaskNotFound "Task not found")
    Just ct ->
      case checkAuth reqAuth ct of
        Left e  -> pure (Left e)
        Right _ -> do
          result <- atomically $ do
            status <- readTVar ct.ctStatus
            if isTerminal status
              then pure $ Left (TaskError TaskAlreadyTerminal "Task already in terminal state")
              else do
                writeTVar ct.ctStatus TaskFailed
                writeTVar ct.ctUpdatedAt now
                _ <- tryPutTMVar ct.ctResult (Left rpcErr)
                pure (Right ())
          case result of
            Left e  -> pure (Left e)
            Right _ -> Right <$> toTask ct

------------------------------------------------------------------------
-- ConcreteTaskHandle (TaskHandle instance)
------------------------------------------------------------------------

newtype ConcreteTaskHandle = ConcreteTaskHandle ConcreteTask

instance TaskHandle ConcreteTaskHandle where
  taskId (ConcreteTaskHandle ct) = ct.ctId

  isActive (ConcreteTaskHandle ct) = unsafePerformIO $ do
    status <- readTVarIO ct.ctStatus
    pure (not (isTerminal status))

  update (ConcreteTaskHandle ct) newStatus mMsg = do
    now <- getCurrentTime
    atomically $ do
      writeTVar ct.ctStatus newStatus
      case mMsg of
        Nothing  -> pure ()
        Just msg -> writeTVar ct.ctMessage (Just msg)
      writeTVar ct.ctUpdatedAt now

  complete (ConcreteTaskHandle ct) val = do
    now <- getCurrentTime
    atomically $ do
      writeTVar ct.ctStatus TaskCompleted
      writeTVar ct.ctUpdatedAt now
      _ <- tryPutTMVar ct.ctResult (Right val)
      pure ()

  fail_ (ConcreteTaskHandle ct) rpcErr = do
    now <- getCurrentTime
    atomically $ do
      writeTVar ct.ctStatus TaskFailed
      writeTVar ct.ctUpdatedAt now
      _ <- tryPutTMVar ct.ctResult (Left rpcErr)
      pure ()

------------------------------------------------------------------------
-- TaskFactory instance for TaskStore
------------------------------------------------------------------------

instance TaskFactory TaskStore where
  createTask store mTtl authCtx = do
    now <- getCurrentTime
    tasks <- readTVarIO store.tsTasks
    let count = HM.size tasks
    if fromIntegral count >= store.tsConfig.taskMaxTasks
      then pure $ Left (TaskError TaskAlreadyTerminal "Maximum task limit reached")
      else do
        uuid      <- nextRandom
        let tid   = UUID.toText uuid
            ttl   = maybe store.tsConfig.taskDefaultTtl id mTtl
        statusVar <- newTVarIO TaskWorking
        msgVar    <- newTVarIO Nothing
        resultVar <- newEmptyTMVarIO
        updVar    <- newTVarIO now
        let ct = ConcreteTask
                   { ctId        = tid
                   , ctStatus    = statusVar
                   , ctMessage   = msgVar
                   , ctResult    = resultVar
                   , ctCreatedAt = now
                   , ctUpdatedAt = updVar
                   , ctTtl       = ttl
                   , ctAuthCtx   = authCtx
                   }
        atomically $ do
          m <- readTVar store.tsTasks
          writeTVar store.tsTasks (HM.insert tid ct m)
        pure $ Right (SomeTaskHandle (ConcreteTaskHandle ct))

------------------------------------------------------------------------
-- TaskFeature
------------------------------------------------------------------------

data TaskFeature = TaskFeature
  { tfStore   :: !TaskStore
  , tfSession :: !(TVar (Maybe Session))
  }

newTaskFeature :: TaskStore -> IO TaskFeature
newTaskFeature store = do
  sessionVar <- newTVarIO Nothing
  pure TaskFeature { tfStore = store, tfSession = sessionVar }

attach :: TaskFeature -> Session -> IO ()
attach feat session = do
  atomically $ writeTVar feat.tfSession (Just session)
  session.sessionOnRequest "tasks/get"    (handleGet feat)
  session.sessionOnRequest "tasks/list"   (handleList feat)
  session.sessionOnRequest "tasks/cancel" (handleCancel feat)

detach :: TaskFeature -> IO ()
detach feat = atomically $ writeTVar feat.tfSession Nothing

------------------------------------------------------------------------
-- Request handlers
------------------------------------------------------------------------

handleGet :: TaskFeature -> Aeson.Value -> req -> IO (Either RPCError Aeson.Value)
handleGet feat params _ =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure $ Left (RPCError (-32602) (T.pack ("Invalid params: " ++ err)) Nothing)
    Aeson.Success (tid :: Text) -> do
      result <- getTask feat.tfStore tid Nothing
      pure $ case result of
        Left te -> Left (taskErrorToRPC te)
        Right t -> Right (Aeson.toJSON t)

handleList :: TaskFeature -> Aeson.Value -> req -> IO (Either RPCError Aeson.Value)
handleList feat params _ = do
  let mCursor = case Aeson.fromJSON params of
        Aeson.Success (Aeson.Object o) ->
          case HM.lookup "cursor" (toTextMap o) of
            Just (Aeson.String c) -> Just (Cursor c)
            _                     -> Nothing
        _ -> Nothing
  result <- listTasks feat.tfStore Nothing mCursor
  pure $ case result of
    Left te             -> Left (taskErrorToRPC te)
    Right (ts, nextCur) ->
      Right $ Aeson.object
        [ "tasks"      .= ts
        , "nextCursor" .= fmap (\(Cursor c) -> c) nextCur
        ]

handleCancel :: TaskFeature -> Aeson.Value -> req -> IO (Either RPCError Aeson.Value)
handleCancel feat params _ =
  case Aeson.fromJSON params of
    Aeson.Error err ->
      pure $ Left (RPCError (-32602) (T.pack ("Invalid params: " ++ err)) Nothing)
    Aeson.Success (tid :: Text) -> do
      result <- cancelTask feat.tfStore tid Nothing
      pure $ case result of
        Left te -> Left (taskErrorToRPC te)
        Right t -> Right (Aeson.toJSON t)

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

taskErrorToRPC :: TaskError -> RPCError
taskErrorToRPC te = RPCError
  { rpcErrorCode    = kindToCode te.taskErrorKind
  , rpcErrorMessage = te.taskErrorDetail
  , rpcErrorData    = Nothing
  }
  where
    kindToCode TaskNotFound       = -32001
    kindToCode TaskAlreadyTerminal = -32002
    kindToCode TaskAccessDenied   = -32003
    kindToCode TaskExpired        = -32004

-- Convert aeson KeyMap to HashMap Text for cursor lookup
toTextMap :: Aeson.Object -> HashMap Text Aeson.Value
toTextMap = HM.fromList . map (\(k, v) -> (AKey.toText k, v)) . KM.toList
