{-# LANGUAGE DeriveAnyClass #-}

module CodeStar.Config.Types
  ( -- * API Key
    ApiKey (..)

    -- * Config errors
  , ConfigError (..)

    -- * Top-level config
  , Config (..)
  , PartialConfig (..)

    -- * Model specification
  , ModelSpec (..)

    -- * Sections
  , ServerSection (..)
  , TelemetrySection (..)
  , ContextSection (..)
  , CompactionSection (..)
  , ShellSection (..)
  , SessionSection (..)
  , GuardrailsSection (..)
  , BudgetSection (..)
  , RepoMapSection (..)
  , MemorySection (..)

    -- * Partial sections
  , PartialServerSection (..)
  , PartialTelemetrySection (..)
  , PartialContextSection (..)
  , PartialCompactionSection (..)
  , PartialShellSection (..)
  , PartialSessionSection (..)
  , PartialGuardrailsSection (..)
  , PartialBudgetSection (..)
  , PartialRepoMapSection (..)
  , PartialMemorySection (..)

    -- * Enums
  , TelemetryMode (..)
  , IndexStrategy (..)
  , SandboxMode (..)
  , AuthMode (..)

    -- * MCP
  , McpAuthConfig (..)
  , McpEndpoint (..)
  , McpTransport (..)

    -- * CLI
  , CliArgs (..)
  , CliCommand (..)
  , CacheGcArgs (..)
  , RunArgs (..)
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=), withText)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import Data.Monoid (Last (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

import CodeStar.Types (ModelRole (..), PlanningMode (..))

-- --------------------------------------------------------------------
-- API Key
-- --------------------------------------------------------------------

newtype ApiKey = ApiKey { unApiKey :: Text }
  deriving stock (Eq, Generic)

instance Show ApiKey where
  show _ = "ApiKey \"<redacted>\""

-- --------------------------------------------------------------------
-- Config Errors
-- --------------------------------------------------------------------

data ConfigError
  = ConfigFileError !FilePath !Text
  | MissingRequired !Text
  | InvalidRange !Text !Text
  | InvalidFraction !Text !Double
  | PortConflict !Text !Text !Int
  | InvalidEnumValue !Text !Text
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Model Specification
-- --------------------------------------------------------------------

data ModelSpec = ModelSpec
  { modelName   :: !Text
  , temperature :: !(Maybe Double)
  , topP        :: !(Maybe Double)
  , maxTokens   :: !(Maybe Int)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

-- --------------------------------------------------------------------
-- Enums
-- --------------------------------------------------------------------

data TelemetryMode = TelemetryOtlp | TelemetryStderr | TelemetryOff
  deriving stock (Eq, Show, Bounded, Enum)

data IndexStrategy = NoIndex | RepoMapIndex | SemanticIndex
  deriving stock (Eq, Show, Generic)

instance FromJSON IndexStrategy where
  parseJSON = withText "IndexStrategy" $ \case
    "none"     -> pure NoIndex
    "repomap"  -> pure RepoMapIndex
    "semantic" -> pure SemanticIndex
    other      -> fail $ "Unknown IndexStrategy: " <> show other

data SandboxMode = NoSandbox
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data AuthMode = NoAuth
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

-- --------------------------------------------------------------------
-- MCP
-- --------------------------------------------------------------------

data McpTransport = StdioTransport | HttpTransport | StreamableHttpTransport
  deriving stock (Eq, Show, Generic)

instance FromJSON McpTransport where
  parseJSON = withText "McpTransport" $ \case
    "stdio"           -> pure StdioTransport
    "http"            -> pure HttpTransport
    "streamable-http" -> pure StreamableHttpTransport
    other             -> fail $ "Unknown McpTransport: " <> show other

-- | OAuth 2.1 auth config for an MCP endpoint.
data McpAuthConfig = McpAuthConfig
  { macRedirectUri :: !Text   -- ^ e.g. "http://localhost:9876/callback"
  , macScopes      :: ![Text] -- ^ requested OAuth scopes
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON McpAuthConfig where
  parseJSON = Aeson.withObject "McpAuthConfig" $ \o ->
    McpAuthConfig
      <$> o .:  "redirectUri"
      <*> o .:? "scopes" .!= []

data McpEndpoint = McpEndpoint
  { endpointName :: !Text
  , command      :: !Text
  , args         :: ![Text]
  , env          :: !(Map Text Text)
  , transport    :: !McpTransport
  , auth         :: !(Maybe McpAuthConfig)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON)

-- --------------------------------------------------------------------
-- Concrete Sections
-- --------------------------------------------------------------------

data ServerSection = ServerSection
  { port                    :: !Int
  , host                    :: !Text
  , httpTimeout             :: !Int
  , gracefulShutdownTimeout :: !Int
  , pingInterval            :: !Int
  } deriving stock (Eq, Show)

data TelemetrySection = TelemetrySection
  { mode           :: !TelemetryMode
  , serviceName    :: !Text
  , endpoint       :: !(Maybe Text)
  , logToStderr    :: !Bool
  , metricsEnabled :: !Bool
  , metricsBindHost :: !Text
  , metricsPort    :: !(Maybe Int)
  , sampleRate     :: !Double        -- ^ Trace sampling ratio 0.0–1.0. Default 1.0.
  } deriving stock (Eq, Show)

data ContextSection = ContextSection
  { maxTokens         :: !Int
  , repoMapReserve    :: !Int
  , memoryReserve     :: !Int
  , compactionReserve :: !Int
  , responseReserve   :: !Int
  } deriving stock (Eq, Show)

data CompactionSection = CompactionSection
  { triggerFraction  :: !Double
  , maxContextTokens :: !Int
  } deriving stock (Eq, Show)

data ShellSection = ShellSection
  { defaultTimeout   :: !Int
  , maxConcurrent    :: !Int
  , outputTruncation :: !Int
  } deriving stock (Eq, Show)

data SessionSection = SessionSection
  { maxPerUser        :: !Int
  , inactivityTimeout :: !Int
  } deriving stock (Eq, Show)

data GuardrailsSection = GuardrailsSection
  { denyList       :: !(Set Text)
  , allowList      :: !(Set Text)
  , secretPatterns :: ![Text]
  } deriving stock (Eq, Show)

data BudgetSection = BudgetSection
  { maxSteps        :: !Int
  , sessionTokenMax :: !(Maybe Word64)
  , dailyTokenMax   :: !(Maybe Word64)
  } deriving stock (Eq, Show)

data RepoMapSection = RepoMapSection
  { rebuildIntervalMs :: !Int
  , rmMaxTokens       :: !Int
  , batchSize         :: !Int
  } deriving stock (Eq, Show)

data MemorySection = MemorySection
  { enabled      :: !Bool
  , maxEntries   :: !Int
  , autoDiscover :: !Bool
  } deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Partial Sections (Last monoid for layer merging)
-- --------------------------------------------------------------------

data PartialServerSection = PartialServerSection
  { port                    :: !(Last Int)
  , host                    :: !(Last Text)
  , httpTimeout             :: !(Last Int)
  , gracefulShutdownTimeout :: !(Last Int)
  , pingInterval            :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialServerSection where
  a <> b = PartialServerSection
    (a.port <> b.port) (a.host <> b.host) (a.httpTimeout <> b.httpTimeout)
    (a.gracefulShutdownTimeout <> b.gracefulShutdownTimeout) (a.pingInterval <> b.pingInterval)

instance Monoid PartialServerSection where
  mempty = PartialServerSection (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

data PartialTelemetrySection = PartialTelemetrySection
  { mode           :: !(Last TelemetryMode)
  , serviceName    :: !(Last Text)
  , endpoint       :: !(Last (Maybe Text))
  , logToStderr    :: !(Last Bool)
  , metricsEnabled :: !(Last Bool)
  , metricsBindHost :: !(Last Text)
  , metricsPort    :: !(Last (Maybe Int))
  , sampleRate     :: !(Last Double)
  } deriving stock (Eq, Show)

instance Semigroup PartialTelemetrySection where
  a <> b = PartialTelemetrySection
    (a.mode <> b.mode) (a.serviceName <> b.serviceName) (a.endpoint <> b.endpoint)
    (a.logToStderr <> b.logToStderr) (a.metricsEnabled <> b.metricsEnabled) (a.metricsBindHost <> b.metricsBindHost) (a.metricsPort <> b.metricsPort)
    (a.sampleRate <> b.sampleRate)

instance Monoid PartialTelemetrySection where
  mempty = PartialTelemetrySection
    (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

data PartialContextSection = PartialContextSection
  { maxTokens         :: !(Last Int)
  , repoMapReserve    :: !(Last Int)
  , memoryReserve     :: !(Last Int)
  , compactionReserve :: !(Last Int)
  , responseReserve   :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialContextSection where
  a <> b = PartialContextSection
    (a.maxTokens <> b.maxTokens) (a.repoMapReserve <> b.repoMapReserve)
    (a.memoryReserve <> b.memoryReserve) (a.compactionReserve <> b.compactionReserve)
    (a.responseReserve <> b.responseReserve)

instance Monoid PartialContextSection where
  mempty = PartialContextSection
    (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing)

data PartialCompactionSection = PartialCompactionSection
  { triggerFraction  :: !(Last Double)
  , maxContextTokens :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialCompactionSection where
  a <> b = PartialCompactionSection (a.triggerFraction <> b.triggerFraction) (a.maxContextTokens <> b.maxContextTokens)

instance Monoid PartialCompactionSection where
  mempty = PartialCompactionSection (Last Nothing) (Last Nothing)

data PartialShellSection = PartialShellSection
  { defaultTimeout   :: !(Last Int)
  , maxConcurrent    :: !(Last Int)
  , outputTruncation :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialShellSection where
  a <> b = PartialShellSection
    (a.defaultTimeout <> b.defaultTimeout) (a.maxConcurrent <> b.maxConcurrent) (a.outputTruncation <> b.outputTruncation)

instance Monoid PartialShellSection where
  mempty = PartialShellSection (Last Nothing) (Last Nothing) (Last Nothing)

data PartialSessionSection = PartialSessionSection
  { maxPerUser        :: !(Last Int)
  , inactivityTimeout :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialSessionSection where
  a <> b = PartialSessionSection (a.maxPerUser <> b.maxPerUser) (a.inactivityTimeout <> b.inactivityTimeout)

instance Monoid PartialSessionSection where
  mempty = PartialSessionSection (Last Nothing) (Last Nothing)

data PartialGuardrailsSection = PartialGuardrailsSection
  { denyList       :: !(Last (Set Text))
  , allowList      :: !(Last (Set Text))
  , secretPatterns :: !(Last [Text])
  } deriving stock (Eq, Show)

instance Semigroup PartialGuardrailsSection where
  a <> b = PartialGuardrailsSection (a.denyList <> b.denyList) (a.allowList <> b.allowList) (a.secretPatterns <> b.secretPatterns)

instance Monoid PartialGuardrailsSection where
  mempty = PartialGuardrailsSection (Last Nothing) (Last Nothing) (Last Nothing)

data PartialBudgetSection = PartialBudgetSection
  { maxSteps        :: !(Last Int)
  , sessionTokenMax :: !(Last (Maybe Word64))
  , dailyTokenMax   :: !(Last (Maybe Word64))
  } deriving stock (Eq, Show)

instance Semigroup PartialBudgetSection where
  a <> b = PartialBudgetSection (a.maxSteps <> b.maxSteps) (a.sessionTokenMax <> b.sessionTokenMax) (a.dailyTokenMax <> b.dailyTokenMax)

instance Monoid PartialBudgetSection where
  mempty = PartialBudgetSection (Last Nothing) (Last Nothing) (Last Nothing)

data PartialRepoMapSection = PartialRepoMapSection
  { rebuildIntervalMs :: !(Last Int)
  , rmMaxTokens       :: !(Last Int)
  , batchSize         :: !(Last Int)
  } deriving stock (Eq, Show)

instance Semigroup PartialRepoMapSection where
  a <> b = PartialRepoMapSection (a.rebuildIntervalMs <> b.rebuildIntervalMs) (a.rmMaxTokens <> b.rmMaxTokens) (a.batchSize <> b.batchSize)

instance Monoid PartialRepoMapSection where
  mempty = PartialRepoMapSection (Last Nothing) (Last Nothing) (Last Nothing)

data PartialMemorySection = PartialMemorySection
  { enabled      :: !(Last Bool)
  , maxEntries   :: !(Last Int)
  , autoDiscover :: !(Last Bool)
  } deriving stock (Eq, Show)

instance Semigroup PartialMemorySection where
  a <> b = PartialMemorySection (a.enabled <> b.enabled) (a.maxEntries <> b.maxEntries) (a.autoDiscover <> b.autoDiscover)

instance Monoid PartialMemorySection where
  mempty = PartialMemorySection (Last Nothing) (Last Nothing) (Last Nothing)

-- --------------------------------------------------------------------
-- Top-level Config
-- --------------------------------------------------------------------

data Config = Config
  { provider      :: !Text
  , modelRoles    :: !(Map ModelRole ModelSpec)
  , planningMode  :: !PlanningMode
  , sandboxMode   :: !SandboxMode
  , authMode      :: !AuthMode
  , workspacePath :: !FilePath
  , apiKey        :: !ApiKey
  , indexStrategy :: !IndexStrategy
  , permissions   :: ![Text]
  , mcpEndpoints  :: ![McpEndpoint]
  , server        :: !ServerSection
  , telemetry     :: !TelemetrySection
  , context       :: !ContextSection
  , compaction    :: !CompactionSection
  , shell         :: !ShellSection
  , session       :: !SessionSection
  , guardrails    :: !GuardrailsSection
  , budgets       :: !BudgetSection
  , repomap       :: !RepoMapSection
  , memory        :: !MemorySection
  } deriving stock (Show, Eq)

data PartialConfig = PartialConfig
  { provider      :: !(Last Text)
  , modelRoles    :: !(Last (Map ModelRole ModelSpec))
  , planningMode  :: !(Last PlanningMode)
  , sandboxMode   :: !(Last SandboxMode)
  , authMode      :: !(Last AuthMode)
  , workspacePath :: !(Last FilePath)
  , apiKey        :: !(Last ApiKey)
  , indexStrategy :: !(Last IndexStrategy)
  , permissions   :: !(Last [Text])
  , mcpEndpoints  :: !(Last [McpEndpoint])
  , server        :: !PartialServerSection
  , telemetry     :: !PartialTelemetrySection
  , context       :: !PartialContextSection
  , compaction    :: !PartialCompactionSection
  , shell         :: !PartialShellSection
  , session       :: !PartialSessionSection
  , guardrails    :: !PartialGuardrailsSection
  , budgets       :: !PartialBudgetSection
  , repomap       :: !PartialRepoMapSection
  , memory        :: !PartialMemorySection
  } deriving stock (Eq, Show)

instance Semigroup PartialConfig where
  a <> b = PartialConfig
    { provider      = a.provider <> b.provider
    , modelRoles    = a.modelRoles <> b.modelRoles
    , planningMode  = a.planningMode <> b.planningMode
    , sandboxMode   = a.sandboxMode <> b.sandboxMode
    , authMode      = a.authMode <> b.authMode
    , workspacePath = a.workspacePath <> b.workspacePath
    , apiKey        = a.apiKey <> b.apiKey
    , indexStrategy = a.indexStrategy <> b.indexStrategy
    , permissions   = a.permissions <> b.permissions
    , mcpEndpoints  = a.mcpEndpoints <> b.mcpEndpoints
    , server        = a.server <> b.server
    , telemetry     = a.telemetry <> b.telemetry
    , context       = a.context <> b.context
    , compaction    = a.compaction <> b.compaction
    , shell         = a.shell <> b.shell
    , session       = a.session <> b.session
    , guardrails    = a.guardrails <> b.guardrails
    , budgets       = a.budgets <> b.budgets
    , repomap       = a.repomap <> b.repomap
    , memory        = a.memory <> b.memory
    }

instance Monoid PartialConfig where
  mempty = PartialConfig
    { provider = Last Nothing, modelRoles = Last Nothing, planningMode = Last Nothing
    , sandboxMode = Last Nothing, authMode = Last Nothing, workspacePath = Last Nothing
    , apiKey = Last Nothing, indexStrategy = Last Nothing, permissions = Last Nothing
    , mcpEndpoints = Last Nothing, server = mempty, telemetry = mempty, context = mempty
    , compaction = mempty, shell = mempty, session = mempty, guardrails = mempty
    , budgets = mempty, repomap = mempty, memory = mempty
    }

-- --------------------------------------------------------------------
-- CLI types
-- --------------------------------------------------------------------

data CliArgs = CliArgs
  { cliCommand :: !CliCommand
  } deriving stock (Eq, Show)

data CliCommand
  = RunAgent !RunArgs
  | FetchGrammars !(Maybe Text)
  | MigrateConfig !(Maybe FilePath)
  | CacheGc !CacheGcArgs
  deriving stock (Eq, Show)

data CacheGcArgs = CacheGcArgs
  { cgDelete    :: !Bool
  , cgJson      :: !Bool
  , cgWorkspace :: !(Maybe FilePath)
  } deriving stock (Eq, Show)

data RunArgs = RunArgs
  { cliModel     :: !(Maybe Text)
  , cliPort      :: !(Maybe Int)
  , cliHeadless  :: !Bool
  , cliTask      :: !(Maybe Text)
  , cliWorkspace :: !(Maybe FilePath)
  , cliTelemetry :: !TelemetryMode
  } deriving stock (Eq, Show)
