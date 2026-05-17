-- |
-- Module      : Network.MCP.Types.Capabilities
-- Stability   : stable
--
-- Capability maps, negotiated capabilities, and task interfaces for MCP.
module Network.MCP.Types.Capabilities
  ( -- * Capability maps
    ClientCapabilities (..)
  , ServerCapabilities (..)
  , NegotiatedCapabilities (..)

    -- * Capability sub-records
  , RootsCapability (..)
  , SamplingCapability (..)
  , ElicitationCapability (..)
  , PromptsCapability (..)
  , ResourcesCapability (..)
  , ToolsCapability (..)
  , TasksClientCapability (..)
  , TasksServerCapability (..)

    -- * Task types
  , TaskStatus (..)
  , TaskError (..)
  , TaskErrorKind (..)
  , AuthContext

    -- * Task interfaces
  , TaskHandle (..)
  , TaskFactory (..)
  , SomeTaskHandle (..)
  ) where

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics (Generic)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T

import Network.MCP.Types (RPCError)

------------------------------------------------------------------------
-- Capability sub-records
------------------------------------------------------------------------

-- | Client roots capability.
data RootsCapability = RootsCapability
  { rootsListChanged :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON RootsCapability where
  toJSON rc =
    Aeson.object $ maybe [] (\b -> ["listChanged" .= b]) rc.rootsListChanged
  toEncoding rc =
    E.pairs $ foldMap ("listChanged" .=) rc.rootsListChanged

instance Aeson.FromJSON RootsCapability where
  parseJSON = Aeson.withObject "RootsCapability" $ \o ->
    RootsCapability <$> o .:? "listChanged"

-- | Client sampling capability. Per the MCP spec this is an empty
-- object whose presence signals that the client supports sampling.
data SamplingCapability = SamplingCapability
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SamplingCapability where
  toJSON _ = Aeson.object []
  toEncoding _ = E.pairs mempty

instance Aeson.FromJSON SamplingCapability where
  parseJSON = Aeson.withObject "SamplingCapability" $ \_ ->
    pure SamplingCapability

-- | Client elicitation capability.
data ElicitationCapability = ElicitationCapability
  { elicitForm :: !Bool
  , elicitUrl :: !Bool
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ElicitationCapability where
  toJSON ec =
    Aeson.object $
      (if ec.elicitForm then ["form" .= Aeson.object []] else [])
        ++ (if ec.elicitUrl then ["url" .= Aeson.object []] else [])
  toEncoding ec =
    E.pairs $
      (if ec.elicitForm then "form" .= Aeson.object [] else mempty)
        <> (if ec.elicitUrl then "url" .= Aeson.object [] else mempty)

instance Aeson.FromJSON ElicitationCapability where
  parseJSON = Aeson.withObject "ElicitationCapability" $ \o ->
    ElicitationCapability
      <$> pure (KM.member "form" o)
      <*> pure (KM.member "url" o)

-- | Server prompts capability.
data PromptsCapability = PromptsCapability
  { promptsListChanged :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON PromptsCapability where
  toJSON pc =
    Aeson.object $ maybe [] (\b -> ["listChanged" .= b]) pc.promptsListChanged
  toEncoding pc =
    E.pairs $ foldMap ("listChanged" .=) pc.promptsListChanged

instance Aeson.FromJSON PromptsCapability where
  parseJSON = Aeson.withObject "PromptsCapability" $ \o ->
    PromptsCapability <$> o .:? "listChanged"

-- | Server resources capability.
data ResourcesCapability = ResourcesCapability
  { resourcesSubscribe :: !(Maybe Bool)
  , resourcesListChanged :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ResourcesCapability where
  toJSON rc =
    Aeson.object $
      maybe [] (\b -> ["subscribe" .= b]) rc.resourcesSubscribe
        ++ maybe [] (\b -> ["listChanged" .= b]) rc.resourcesListChanged
  toEncoding rc =
    E.pairs $
      foldMap ("subscribe" .=) rc.resourcesSubscribe
        <> foldMap ("listChanged" .=) rc.resourcesListChanged

instance Aeson.FromJSON ResourcesCapability where
  parseJSON = Aeson.withObject "ResourcesCapability" $ \o ->
    ResourcesCapability <$> o .:? "subscribe" <*> o .:? "listChanged"

-- | Server tools capability.
data ToolsCapability = ToolsCapability
  { toolsListChanged :: !(Maybe Bool)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ToolsCapability where
  toJSON tc =
    Aeson.object $ maybe [] (\b -> ["listChanged" .= b]) tc.toolsListChanged
  toEncoding tc =
    E.pairs $ foldMap ("listChanged" .=) tc.toolsListChanged

instance Aeson.FromJSON ToolsCapability where
  parseJSON = Aeson.withObject "ToolsCapability" $ \o ->
    ToolsCapability <$> o .:? "listChanged"

-- | Client tasks capability (fields TBD per protocol spec).
data TasksClientCapability = TasksClientCapability
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON TasksClientCapability where
  toJSON _ = Aeson.object []

instance Aeson.FromJSON TasksClientCapability where
  parseJSON = Aeson.withObject "TasksClientCapability" $ \_ ->
    pure TasksClientCapability

-- | Server tasks capability (fields TBD per protocol spec).
data TasksServerCapability = TasksServerCapability
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON TasksServerCapability where
  toJSON _ = Aeson.object []

instance Aeson.FromJSON TasksServerCapability where
  parseJSON = Aeson.withObject "TasksServerCapability" $ \_ ->
    pure TasksServerCapability

------------------------------------------------------------------------
-- Capability maps
------------------------------------------------------------------------

-- | Capabilities declared by the client during initialization.
data ClientCapabilities = ClientCapabilities
  { clientRoots :: !(Maybe RootsCapability)
  , clientSampling :: !(Maybe SamplingCapability)
  , clientElicitation :: !(Maybe ElicitationCapability)
  , clientTasks :: !(Maybe TasksClientCapability)
  , clientExperimental :: !(Maybe (HashMap Text Aeson.Value))
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ClientCapabilities where
  toJSON cc =
    Aeson.object $
      maybe [] (\r -> ["roots" .= r]) cc.clientRoots
        ++ maybe [] (\s -> ["sampling" .= s]) cc.clientSampling
        ++ maybe [] (\e -> ["elicitation" .= e]) cc.clientElicitation
        ++ maybe [] (\t -> ["tasks" .= t]) cc.clientTasks
        ++ maybe [] (\x -> ["experimental" .= x]) cc.clientExperimental
  toEncoding cc =
    E.pairs $
      foldMap ("roots" .=) cc.clientRoots
        <> foldMap ("sampling" .=) cc.clientSampling
        <> foldMap ("elicitation" .=) cc.clientElicitation
        <> foldMap ("tasks" .=) cc.clientTasks
        <> foldMap ("experimental" .=) cc.clientExperimental

instance Aeson.FromJSON ClientCapabilities where
  parseJSON = Aeson.withObject "ClientCapabilities" $ \o ->
    ClientCapabilities
      <$> o .:? "roots"
      <*> o .:? "sampling"
      <*> o .:? "elicitation"
      <*> o .:? "tasks"
      <*> o .:? "experimental"

-- | Capabilities declared by the server during initialization.
data ServerCapabilities = ServerCapabilities
  { serverPrompts :: !(Maybe PromptsCapability)
  , serverResources :: !(Maybe ResourcesCapability)
  , serverTools :: !(Maybe ToolsCapability)
  , serverLogging :: !(Maybe ())
  , serverCompletions :: !(Maybe ())
  , serverTasks :: !(Maybe TasksServerCapability)
  , serverExperimental :: !(Maybe (HashMap Text Aeson.Value))
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON ServerCapabilities where
  toJSON sc =
    Aeson.object $
      maybe [] (\p -> ["prompts" .= p]) sc.serverPrompts
        ++ maybe [] (\r -> ["resources" .= r]) sc.serverResources
        ++ maybe [] (\t -> ["tools" .= t]) sc.serverTools
        ++ maybe [] (\_ -> ["logging" .= Aeson.object []]) sc.serverLogging
        ++ maybe [] (\_ -> ["completions" .= Aeson.object []]) sc.serverCompletions
        ++ maybe [] (\t -> ["tasks" .= t]) sc.serverTasks
        ++ maybe [] (\x -> ["experimental" .= x]) sc.serverExperimental
  toEncoding sc =
    E.pairs $
      foldMap ("prompts" .=) sc.serverPrompts
        <> foldMap ("resources" .=) sc.serverResources
        <> foldMap ("tools" .=) sc.serverTools
        <> foldMap (\_ -> "logging" .= Aeson.object []) sc.serverLogging
        <> foldMap (\_ -> "completions" .= Aeson.object []) sc.serverCompletions
        <> foldMap ("tasks" .=) sc.serverTasks
        <> foldMap ("experimental" .=) sc.serverExperimental

instance Aeson.FromJSON ServerCapabilities where
  parseJSON = Aeson.withObject "ServerCapabilities" $ \o ->
    ServerCapabilities
      <$> o .:? "prompts"
      <*> o .:? "resources"
      <*> o .:? "tools"
      <*> parseUnit o "logging"
      <*> parseUnit o "completions"
      <*> o .:? "tasks"
      <*> o .:? "experimental"

-- | Parse an optional unit-capability: key present (any value) → 'Just ()',
-- key absent → 'Nothing'.
parseUnit :: Aeson.Object -> Aeson.Key -> Parser (Maybe ())
parseUnit o k = case KM.lookup k o of
  Nothing -> pure Nothing
  Just _ -> pure (Just ())

-- | The intersection of client and server capabilities.
data NegotiatedCapabilities = NegotiatedCapabilities
  { negClient :: !ClientCapabilities
  , negServer :: !ServerCapabilities
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON NegotiatedCapabilities where
  toJSON nc =
    Aeson.object
      [ "client" .= nc.negClient
      , "server" .= nc.negServer
      ]

instance Aeson.FromJSON NegotiatedCapabilities where
  parseJSON = Aeson.withObject "NegotiatedCapabilities" $ \o ->
    NegotiatedCapabilities
      <$> o .: "client"
      <*> o .: "server"

------------------------------------------------------------------------
-- Task types
------------------------------------------------------------------------

-- | Task execution status.
data TaskStatus
  = TaskWorking
  | TaskInputRequired
  | TaskCompleted
  | TaskFailed
  | TaskCancelled
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance Aeson.ToJSON TaskStatus where
  toJSON = \case
    TaskWorking -> "working"
    TaskInputRequired -> "input_required"
    TaskCompleted -> "completed"
    TaskFailed -> "failed"
    TaskCancelled -> "cancelled"
  toEncoding = \case
    TaskWorking -> E.text "working"
    TaskInputRequired -> E.text "input_required"
    TaskCompleted -> E.text "completed"
    TaskFailed -> E.text "failed"
    TaskCancelled -> E.text "cancelled"

instance Aeson.FromJSON TaskStatus where
  parseJSON = Aeson.withText "TaskStatus" $ \case
    "working" -> pure TaskWorking
    "input_required" -> pure TaskInputRequired
    "completed" -> pure TaskCompleted
    "failed" -> pure TaskFailed
    "cancelled" -> pure TaskCancelled
    other -> fail $ "Unknown TaskStatus: " ++ T.unpack other

-- | Kinds of task errors.
data TaskErrorKind
  = TaskNotFound
  | TaskAlreadyTerminal
  | TaskAccessDenied
  | TaskExpired
  deriving stock (Eq, Show, Bounded, Enum, Generic)

instance Aeson.ToJSON TaskErrorKind where
  toJSON = \case
    TaskNotFound -> "not_found"
    TaskAlreadyTerminal -> "already_terminal"
    TaskAccessDenied -> "access_denied"
    TaskExpired -> "expired"
  toEncoding = \case
    TaskNotFound -> E.text "not_found"
    TaskAlreadyTerminal -> E.text "already_terminal"
    TaskAccessDenied -> E.text "access_denied"
    TaskExpired -> E.text "expired"

instance Aeson.FromJSON TaskErrorKind where
  parseJSON = Aeson.withText "TaskErrorKind" $ \case
    "not_found" -> pure TaskNotFound
    "already_terminal" -> pure TaskAlreadyTerminal
    "access_denied" -> pure TaskAccessDenied
    "expired" -> pure TaskExpired
    other -> fail $ "Unknown TaskErrorKind: " ++ T.unpack other

-- | A task error with kind and detail message.
data TaskError = TaskError
  { taskErrorKind :: !TaskErrorKind
  , taskErrorDetail :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON TaskError where
  toJSON te =
    Aeson.object
      [ "kind" .= te.taskErrorKind
      , "detail" .= te.taskErrorDetail
      ]

instance Aeson.FromJSON TaskError where
  parseJSON = Aeson.withObject "TaskError" $ \o ->
    TaskError <$> o .: "kind" <*> o .: "detail"

-- | Opaque authorization context, derived from OAuth token claims.
type AuthContext = Aeson.Value

------------------------------------------------------------------------
-- Task interfaces
------------------------------------------------------------------------

-- | Interface for a running task handle.
class TaskHandle h where
  taskId :: h -> Text
  isActive :: h -> Bool
  update :: h -> TaskStatus -> Maybe Text -> IO ()
  complete :: h -> Aeson.Value -> IO ()
  fail_ :: h -> RPCError -> IO ()

-- | Interface for creating new tasks.
class TaskFactory f where
  createTask :: f -> Maybe Word -> Maybe AuthContext -> IO (Either TaskError SomeTaskHandle)

-- | Existential wrapper for any 'TaskHandle'.
data SomeTaskHandle = forall h. TaskHandle h => SomeTaskHandle h
