{- |
= Platform.SessionManager — agent session lifecycle

The 'SessionManager' creates, tracks, and destroys agent sessions on the
server.  Each session corresponds to one active agent thread, identified
by a 'SessionId'.

== Session state machine

@
  SActive
     │
     ├─► SWaitingForInput   ◄─── CmdRespond ──► SActive
     │
     └─► SWaitingForApproval ◄── CmdApprove/CmdReject ──► SActive
                                                     │
                                                     └─► STerminated
  SActive ──► SCompleted ControlSignal
  SActive ──► STerminated (on error or destroy)
@

== Concurrency model

  * The session list is stored in an 'IORef' updated atomically.
  * Each session's @status@, @lastActiveAt@, @workerThread@, and
    @pendingModel@ are 'TVar's updated in STM transactions.
  * @inputMVar@ and @approvalMVar@ are @MVar@s that the agent thread
    blocks on when waiting for user input or tool approval.
    'respondToSession' and 'approveSession' unblock the agent by
    writing to the appropriate @MVar@.

== Inactivity reaping

'SessionManager.reap' scans for sessions whose @lastActiveAt@ is older
than @inactivityTimeout@ seconds and destroys them.  Callers are expected
to call this periodically (e.g. every 60 seconds).
-}
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
import CodeStar.LLM.Base (LlmClientDict)
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

-- | A live agent session.  All mutable fields use STM or MVar so they
-- can be read and written safely across threads.
data Session = Session
  { sessionId     :: !SessionId
  -- ^ Unique session identifier, scoped to this server instance.
  , userId        :: !UserId
  -- ^ The user who owns this session.
  , status        :: !(TVar SessionStatus)
  -- ^ Current status; written by the agent thread, read by the server.
  , createdAt     :: !UTCTime
  -- ^ Wall-clock creation time (immutable).
  , lastActiveAt  :: !(TVar UTCTime)
  -- ^ Updated on every command; used for inactivity reaping.
  , workerThread  :: !(TVar (Maybe (Async ())))
  -- ^ The agent's background thread; cancelled on session destroy.
  , inputMVar     :: !(MVar Text)
  -- ^ Unblocked by 'respondToSession' when the agent is waiting for input.
  , approvalMVar  :: !(MVar ApprovalDecision)
  -- ^ Unblocked by 'approveSession'\/'rejectSession' when the agent
  --   is waiting for tool-call approval.
  , eventSink     :: !(AgentEventEnvelope -> IO ())
  -- ^ Called by the agent to send events back to the client.
  , pendingModel  :: !(TVar (Maybe (Text, LlmClientDict)))
  -- ^ Non-Nothing when 'CmdSetModel' was received; consumed by the agent
  --   at the next turn boundary to hot-swap the LLM client.
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

-- | The session manager interface as a record of functions.
-- Construct with 'newSessionManager'; the backing store is an in-memory map.
data SessionManager = SessionManager
  { create      :: UserId -> (AgentEventEnvelope -> IO ()) -> IO (Either Text Session)
  -- ^ Allocate a new session for @userId@.  Returns @Left err@ if the
  --   per-user concurrent session limit is reached.
  , get         :: SessionId -> IO (Maybe Session)
  -- ^ Look up a session by ID.  Returns 'Nothing' if not found or already destroyed.
  , destroy     :: SessionId -> IO ()
  -- ^ Cancel the agent thread and remove the session from the map.
  , list        :: UserId -> IO [Session]
  -- ^ List active (non-terminated) sessions for a user.
  , reap        :: IO ()
  -- ^ Destroy sessions that have been inactive longer than 'inactivityTimeout'.
  , shutdownAll :: IO ()
  -- ^ Destroy every session in the manager (called on server shutdown).
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
      pendingModelVar <- newTVarIO Nothing
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
              , pendingModel = pendingModelVar
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
