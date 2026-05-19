{-# OPTIONS_GHC -Wno-orphans #-}

{- |
= CodeStar.Transport.Types — wire protocol types

This module defines the __bidirectional protocol__ between the agent
server and its clients (IDE extensions, web frontends, CLI client).

== Direction

@
  Client ──── Command ──────────────────────────► Server
  Client ◄─── AgentEventEnvelope (notification) ─ Server
@

  * __Commands__ (@CmdStart@, @CmdRespond@, @CmdApprove@, …) are
    client-initiated requests that drive the session state machine.
    Each command carries a @sessionId@ so the server can route it to
    the correct agent thread.
  * __Events__ ('AgentEventEnvelope') are server-initiated notifications
    pushed to the client as the agent works.  They are tagged with a
    @sessionId@ to support multiplexing multiple sessions over one
    WebSocket connection.

== Orphan instances

'ToJSON'\/'FromJSON' instances for 'AgentEvent' and 'ApprovalDecision'
are defined here (orphans relative to "AgentLoop") because they depend on
transport-level decisions about the wire format that the core library
should not know about.
-}
module CodeStar.Transport.Types
  ( -- * Transport handle
    AgentTransportDict (..)

    -- * Commands (client → server)
  , Command (..)
  , CommandResult (..)

    -- * Events (server → client)
  , AgentEventEnvelope (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

import CodeStar.AgentLoop (AgentEvent (..), ApprovalDecision (..))
import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Types (CheckResult (..), ControlSignal (..), Evidence (..), SessionId (..))
import Data.Aeson.Types (Parser)

-- --------------------------------------------------------------------
-- Commands (client → server)
-- --------------------------------------------------------------------

-- | Commands sent from the client to the server to drive a session.
-- Every command carries a @sessionId@ for routing.
data Command
  = CmdStart
      { sessionId :: !SessionId
      -- ^ The session to create (must be unique per client connection).
      , task :: !Text
      -- ^ The initial task description; starts the agent turn.
      }
  | CmdRespond
      { sessionId :: !SessionId
      , response :: !Text
      }
  | CmdApprove
      { sessionId :: !SessionId
      }
  | CmdReject
      { sessionId :: !SessionId
      , reason :: !Text
      }
  | CmdCompact
      { sessionId :: !SessionId
      , instruction :: !(Maybe Text)
      }
  | CmdStop
      { sessionId :: !SessionId
      }
  | CmdSetModel
      { sessionId :: !SessionId
      , modelName :: !Text
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CommandResult
  = CmdOk
  | CmdErr !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- --------------------------------------------------------------------
-- Events (server → client)
-- --------------------------------------------------------------------

-- | Wraps an AgentEvent with the originating session ID for multiplexing.
data AgentEventEnvelope = AgentEventEnvelope
  { envSessionId :: !SessionId
  , envEvent :: !AgentEvent
  }
  deriving stock (Show, Generic)

instance ToJSON AgentEvent where
  toJSON (AgentToken t) =
    object ["type" .= ("token" :: Text), "token" .= t]
  toJSON (AgentToolCall (ToolName n) a) =
    object ["type" .= ("toolCall" :: Text), "tool" .= n, "arguments" .= a]
  toJSON (AgentToolResult (ToolName n) r) =
    object ["type" .= ("toolResult" :: Text), "tool" .= n, "output" .= r]
  toJSON (AgentApprovalRequired (ToolName n) r) =
    object ["type" .= ("needsApproval" :: Text), "tool" .= n, "reason" .= r]
  toJSON AgentCompacting =
    object ["type" .= ("compacting" :: Text)]
  toJSON (AgentProgress msg) =
    object ["type" .= ("progress" :: Text), "message" .= msg]
  toJSON (AgentCostUpdate i o) =
    object ["type" .= ("cost" :: Text), "inputTokens" .= i, "outputTokens" .= o]
  toJSON (AgentDone sig) =
    object ["type" .= ("done" :: Text), "signal" .= show sig]
  toJSON (AgentError msg) =
    object ["type" .= ("error" :: Text), "message" .= msg]
  toJSON (AgentModelChanged from to) =
    object ["type" .= ("modelChanged" :: Text), "from" .= from, "to" .= to]

instance FromJSON AgentEvent where
  parseJSON = withObject "AgentEvent" $ \o -> do
    tag <- o .: "type" :: Parser Text
    case tag of
      "token" -> AgentToken <$> o .: "token"
      "toolCall" -> AgentToolCall <$> (ToolName <$> o .: "tool") <*> o .: "arguments"
      "toolResult" -> AgentToolResult <$> (ToolName <$> o .: "tool") <*> o .: "output"
      "needsApproval" -> AgentApprovalRequired <$> (ToolName <$> o .: "tool") <*> o .: "reason"
      "compacting" -> pure AgentCompacting
      "progress" -> AgentProgress <$> o .: "message"
      "cost" -> AgentCostUpdate <$> o .: "inputTokens" <*> o .: "outputTokens"
      "done" -> pure (AgentDone (Done emptyEvidence))
      "error" -> AgentError <$> o .: "message"
      "modelChanged" -> AgentModelChanged <$> o .: "from" <*> o .: "to"
      _ -> fail ("Unknown AgentEvent type: " <> show tag)

emptyEvidence :: Evidence
emptyEvidence =
  Evidence
    { testsPass = NotChecked
    , buildSucceeds = NotChecked
    , filesVerified = []
    , regressions = []
    }

instance ToJSON AgentEventEnvelope where
  toJSON env = object ["sessionId" .= env.envSessionId, "event" .= env.envEvent]

instance FromJSON AgentEventEnvelope where
  parseJSON = withObject "AgentEventEnvelope" $ \o ->
    AgentEventEnvelope <$> o .: "sessionId" <*> o .: "event"

instance ToJSON ApprovalDecision where
  toJSON Approved = object ["decision" .= ("approved" :: Text)]
  toJSON (Rejected msg) = object ["decision" .= ("rejected" :: Text), "reason" .= msg]

instance FromJSON ApprovalDecision where
  parseJSON = withObject "ApprovalDecision" $ \o -> do
    d <- o .: "decision" :: Parser Text
    case d of
      "approved" -> pure Approved
      "rejected" -> do
        reason <- Text.strip <$> o .: "reason"
        if Text.null reason
          then fail "Rejected decision requires non-empty reason"
          else pure (Rejected reason)
      _ -> fail ("Unknown decision: " <> show d)

-- --------------------------------------------------------------------
-- Transport handle
-- --------------------------------------------------------------------

{- | Record-of-functions for sending events and receiving commands.
Implementations: in-process (for tests), WebSocket/JSON-RPC (for server).
-}
data AgentTransportDict = AgentTransportDict
  { sendEvent :: AgentEventEnvelope -> IO ()
  -- ^ Push an event to the connected client.
  , onCommand :: (Command -> IO CommandResult) -> IO ()
  -- ^ Register a handler that is called for each inbound command.
  , listen :: IO ()
  -- ^ Block and process incoming commands until the connection closes.
  , shutdown :: IO ()
  -- ^ Gracefully close the transport connection.
  }
