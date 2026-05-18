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

commandType :: Command -> Text
commandType CmdSetModel{} = "setModel"
commandType CmdStart{}    = "start"
commandType CmdRespond{}  = "respond"
commandType CmdApprove{}  = "approve"
commandType CmdReject{}   = "reject"
commandType CmdCompact{}  = "compact"
commandType CmdStop{}     = "stop"

commandSessionId :: Command -> Text
commandSessionId cmd = let SessionId s = cmd.sessionId in s

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
