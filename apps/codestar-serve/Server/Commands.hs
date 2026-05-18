{- |
= Server.Commands — command introspection and model switching

Small helpers for working with the 'Command' ADT that do not belong in
the transport layer or the main dispatch function.

  * 'commandType' and 'commandSessionId' are used in "Server" to attach
    metadata to telemetry spans __before__ the command is dispatched, so
    every command is labelled even if it fails immediately.

  * 'setSessionModel' implements hot model switching: the client can send
    @CmdSetModel@ mid-session to change which LLM is used for the next
    turn without tearing down the session or losing conversation history.
    The new client is stored in a 'TVar' that the agent loop reads at the
    start of each turn.
-}
module Server.Commands
  ( commandType
  , commandSessionId
  , setSessionModel
  ) where

import Control.Concurrent.STM (atomically, writeTVar)
import Data.Text (Text)

import CodeStar.AgentSetup (buildClientForEntry)
import CodeStar.Config (Config (..), AgentConfig)
import CodeStar.Config.Types (ModelEntry (..))
import CodeStar.Platform.SessionManager (Session (..), SessionManager (..))
import CodeStar.Transport.Types (Command (..), CommandResult (..))
import CodeStar.Types (SessionId (..))

-- | Return a stable string label for a 'Command' variant.
-- Used as a telemetry attribute so span data can be filtered by command type.
commandType :: Command -> Text
commandType CmdSetModel{} = "setModel"
commandType CmdStart{}    = "start"
commandType CmdRespond{}  = "respond"
commandType CmdApprove{}  = "approve"
commandType CmdReject{}   = "reject"
commandType CmdCompact{}  = "compact"
commandType CmdStop{}     = "stop"

-- | Extract the session ID from any 'Command' as a plain 'Text'.
-- Every command carries a @sessionId@ field; this unwraps the newtype
-- so callers do not need to pattern-match on 'SessionId'.
commandSessionId :: Command -> Text
commandSessionId cmd = let SessionId s = cmd.sessionId in s

-- | Hot-swap the LLM client for a live session.
--
-- Looks up the named model in @config.models@, builds a fresh HTTP client
-- for it, and stores it in @session.pendingModel@.  The agent loop reads
-- this @TVar@ at the start of each turn and swaps the client atomically,
-- so the change takes effect at the next message boundary without any
-- conversation history being lost.
setSessionModel :: AgentConfig -> SessionManager -> SessionId -> Text -> IO CommandResult
setSessionModel config sessionMgr sid name = do
  mSession <- sessionMgr.get sid
  case mSession of
    Nothing -> pure (CmdErr "Session not found")
    Just session -> do
      case filter (\m -> m.meName == name) config.models of
        [] -> pure (CmdErr ("Unknown model: " <> name))
        (entry:_) -> do
          newClient <- buildClientForEntry config entry
          atomically $ writeTVar session.pendingModel (Just (name, newClient))
          pure CmdOk
