module CodeStar.Platform.SessionManager
  ( -- * Session
    Session (..)
  , SessionStatus (..)

    -- * Manager
  , SessionManager (..)
  , SessionConfig (..)
  , defaultSessionConfig
  , newSessionManager

    -- * Operations
  , createSession
  , getSession
  , destroySession
  , listSessions
  , respondToSession
  , approveSession
  , rejectSession
  , destroyAll
  ) where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryPutMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import CodeStar.AgentLoop (ApprovalDecision (..))
import CodeStar.Transport.Types (AgentEventEnvelope (..), CommandResult (..))
import CodeStar.Types (ControlSignal (..), SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Session
-- --------------------------------------------------------------------

data SessionStatus
  = SActive
  | SWaitingForInput
  | SWaitingForApproval
  | SCompleted ControlSignal
  | STerminated
  deriving stock (Eq, Show)

data Session = Session
  { sessionId :: !SessionId
  , userId :: !UserId
  , status :: !(TVar SessionStatus)
  , createdAt :: !UTCTime
  , lastActiveAt :: !(TVar UTCTime)
  , workerThread :: !(TVar (Maybe (Async ())))
  , inputMVar :: !(MVar Text)
  , approvalMVar :: !(MVar ApprovalDecision)
  , eventSink :: !(AgentEventEnvelope -> IO ())
  }

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data SessionConfig = SessionConfig
  { maxSessionsPerUser :: !Int
  , inactivityTimeout :: !Int
  }
  deriving stock (Eq, Show)

defaultSessionConfig :: SessionConfig
defaultSessionConfig =
  SessionConfig
    { maxSessionsPerUser = 3
    , inactivityTimeout = 1800
    }

-- --------------------------------------------------------------------
-- Manager
-- --------------------------------------------------------------------

type SessionMap = Map SessionId Session

data SessionManager = SessionManager
  { create :: UserId -> (AgentEventEnvelope -> IO ()) -> IO (Either Text Session)
  , get :: SessionId -> IO (Maybe Session)
  , destroy :: SessionId -> IO ()
  , list :: UserId -> IO [Session]
  , reap :: IO ()
  , shutdownAll :: IO ()
  }

newSessionManager :: SessionConfig -> IO SessionManager
newSessionManager cfg = do
  ref <- newIORef (Map.empty :: SessionMap)
  pure
    SessionManager
      { create = doCreate cfg ref
      , get = doGet ref
      , destroy = doDestroy ref
      , list = doList ref
      , reap = doReap cfg ref
      , shutdownAll = doDestroyAll ref
      }

-- --------------------------------------------------------------------
-- Convenience wrappers
-- --------------------------------------------------------------------

createSession :: SessionManager -> UserId -> (AgentEventEnvelope -> IO ()) -> IO (Either Text Session)
createSession mgr = mgr.create

getSession :: SessionManager -> SessionId -> IO (Maybe Session)
getSession mgr = mgr.get

destroySession :: SessionManager -> SessionId -> IO ()
destroySession mgr = mgr.destroy

listSessions :: SessionManager -> UserId -> IO [Session]
listSessions mgr = mgr.list

-- --------------------------------------------------------------------
-- Interaction functions
-- --------------------------------------------------------------------

respondToSession :: SessionManager -> SessionId -> Text -> IO CommandResult
respondToSession mgr sid text = do
  mSession <- mgr.get sid
  case mSession of
    Nothing -> pure (CmdErr "Session not found")
    Just session -> do
      st <- readTVarIO session.status
      case st of
        SWaitingForInput -> do
          putMVar session.inputMVar text
          atomically $ writeTVar session.status SActive
          pure CmdOk
        _ -> pure (CmdErr "Session not waiting for input")

approveSession :: SessionManager -> SessionId -> IO CommandResult
approveSession mgr sid = do
  mSession <- mgr.get sid
  case mSession of
    Nothing -> pure (CmdErr "Session not found")
    Just session -> do
      st <- readTVarIO session.status
      case st of
        SWaitingForApproval -> do
          putMVar session.approvalMVar Approved
          atomically $ writeTVar session.status SActive
          pure CmdOk
        _ -> pure (CmdErr "Session not waiting for approval")

rejectSession :: SessionManager -> SessionId -> Text -> IO CommandResult
rejectSession mgr sid reason = do
  mSession <- mgr.get sid
  case mSession of
    Nothing -> pure (CmdErr "Session not found")
    Just session -> do
      st <- readTVarIO session.status
      case st of
        SWaitingForApproval -> do
          putMVar session.approvalMVar (Rejected reason)
          atomically $ writeTVar session.status SActive
          pure CmdOk
        _ -> pure (CmdErr "Session not waiting for approval")

destroyAll :: SessionManager -> IO ()
destroyAll mgr = mgr.shutdownAll

doDestroyAll :: IORef SessionMap -> IO ()
doDestroyAll ref = do
  sessions <- readIORef ref
  mapM_ (doDestroy ref) (Map.keys sessions)

-- --------------------------------------------------------------------
-- Implementation
-- --------------------------------------------------------------------

doCreate :: SessionConfig -> IORef SessionMap -> UserId -> (AgentEventEnvelope -> IO ()) -> IO (Either Text Session)
doCreate cfg ref uid sink = do
  now <- getCurrentTime
  sessions <- readIORef ref
  let userSessions = length [s | s <- Map.elems sessions, s.userId == uid]
  if userSessions >= cfg.maxSessionsPerUser
    then
      pure $
        Left
          ( "Concurrent session limit ("
              <> Text.pack (show cfg.maxSessionsPerUser)
              <> ") reached for user"
          )
    else do
      statusVar <- newTVarIO SActive
      lastVar <- newTVarIO now
      threadVar <- newTVarIO Nothing
      inputVar <- newEmptyMVar
      approvalVar <- newEmptyMVar
      let sid = SessionId ("session-" <> Text.pack (show (Map.size sessions)))
          session =
            Session
              { sessionId = sid
              , userId = uid
              , status = statusVar
              , createdAt = now
              , lastActiveAt = lastVar
              , workerThread = threadVar
              , inputMVar = inputVar
              , approvalMVar = approvalVar
              , eventSink = sink
              }
      atomicModifyIORef' ref (\m -> (Map.insert sid session m, ()))
      pure (Right session)

doGet :: IORef SessionMap -> SessionId -> IO (Maybe Session)
doGet ref sid = Map.lookup sid <$> readIORef ref

doDestroy :: IORef SessionMap -> SessionId -> IO ()
doDestroy ref sid = do
  sessions <- readIORef ref
  case Map.lookup sid sessions of
    Nothing -> pure ()
    Just s -> do
      _ <- tryPutMVar s.inputMVar ""
      _ <- tryPutMVar s.approvalMVar (Rejected "session cancelled")
      mThread <- readTVarIO s.workerThread
      mapM_ cancel mThread
      atomically $ writeTVar s.status STerminated
      atomicModifyIORef' ref (\m -> (Map.delete sid m, ()))

doList :: IORef SessionMap -> UserId -> IO [Session]
doList ref uid = do
  sessions <- readIORef ref
  filterM isActive [s | s <- Map.elems sessions, s.userId == uid]
 where
  isActive s = do
    st <- readTVarIO s.status
    pure (st /= STerminated)

doReap :: SessionConfig -> IORef SessionMap -> IO ()
doReap cfg ref = do
  now <- getCurrentTime
  sessions <- readIORef ref
  timedOut <- filterM (isTimedOut now) (Map.toList sessions)
  mapM_ (doDestroy ref . fst) timedOut
 where
  isTimedOut now (_, s) = do
    st <- readTVarIO s.status
    lastActive <- readTVarIO s.lastActiveAt
    pure (st /= STerminated && diffUTCTime now lastActive > fromIntegral cfg.inactivityTimeout)

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x : xs) = do
  b <- p x
  xs' <- filterM p xs
  pure (if b then x : xs' else xs')
