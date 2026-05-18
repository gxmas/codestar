module CodeStar.Config.Defaults
  ( defaultConfig
  , defaultModels
  , defaultActiveModel
  ) where

import Data.Set qualified as Set
import Data.Text (Text)

import CodeStar.Config.Types
import CodeStar.Types (PlanningMode (..))

defaultConfig :: Config
defaultConfig = Config
  { provider      = "anthropic"
  , models        = defaultModels
  , activeModel   = defaultActiveModel
  , planningMode  = NoPlan
  , sandboxMode   = NoSandbox
  , authMode      = NoAuth
  , workspacePath = "."
  , apiKey        = ApiKey ""
  , indexStrategy = NoIndex
  , permissions   = []
  , mcpEndpoints  = []
  , server        = defaultServer
  , telemetry     = defaultTelemetry
  , context       = defaultContext
  , compaction    = defaultCompaction
  , shell         = defaultShell
  , session       = defaultSession
  , guardrails    = defaultGuardrails
  , budgets       = defaultBudget
  , repomap       = defaultRepoMap
  , memory        = defaultMemory
  }

defaultModels :: [ModelEntry]
defaultModels =
  [ ModelEntry "sonnet" "anthropic" "claude-sonnet-4-20250514" (ApiKey "") Nothing Nothing (Just 8192)
  ]

defaultActiveModel :: Text
defaultActiveModel = "sonnet"

defaultServer :: ServerSection
defaultServer = ServerSection
  { port                    = 8080
  , host                    = "127.0.0.1"
  , httpTimeout             = 3600
  , gracefulShutdownTimeout = 30
  , pingInterval            = 30
  }

defaultTelemetry :: TelemetrySection
defaultTelemetry = TelemetrySection
  { mode           = TelemetryOtlp
  , serviceName    = "codestar"
  , endpoint       = Nothing
  , logToStderr    = False
  , metricsEnabled = True
  , metricsBindHost = "127.0.0.1"
  , metricsPort    = Just 9100
  , sampleRate     = 1.0
  }

defaultContext :: ContextSection
defaultContext = ContextSection
  { maxTokens         = 200_000
  , repoMapReserve    = 4_096
  , memoryReserve     = 2_048
  , compactionReserve = 1_024
  , responseReserve   = 8_192
  }

defaultCompaction :: CompactionSection
defaultCompaction = CompactionSection
  { triggerFraction  = 0.85
  , maxContextTokens = 200_000
  }

defaultShell :: ShellSection
defaultShell = ShellSection
  { defaultTimeout   = 30_000
  , maxConcurrent    = 64
  , outputTruncation = 50_000
  }

defaultSession :: SessionSection
defaultSession = SessionSection
  { maxPerUser        = 3
  , inactivityTimeout = 1800
  }

defaultGuardrails :: GuardrailsSection
defaultGuardrails = GuardrailsSection
  { denyList       = Set.fromList ["shell"]
  , allowList      = Set.fromList ["read", "glob", "grep", "todo_read", "todo_write"]
  , secretPatterns = ["password", "secret", "api_key", "token", "private_key"]
  }

defaultBudget :: BudgetSection
defaultBudget = BudgetSection
  { maxSteps        = 50
  , sessionTokenMax = Nothing
  , dailyTokenMax   = Nothing
  }

defaultRepoMap :: RepoMapSection
defaultRepoMap = RepoMapSection
  { rebuildIntervalMs = 2000
  , rmMaxTokens       = 4096
  , batchSize         = 10
  }

defaultMemory :: MemorySection
defaultMemory = MemorySection
  { enabled      = True
  , maxEntries   = 100
  , autoDiscover = True
  }