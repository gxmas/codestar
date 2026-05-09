module CodeStar.Config.Defaults
  ( defaultConfig
  , defaultModelRoles
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import CodeStar.Config.Types
import CodeStar.Types (ModelRole (..), PlanningMode (..))

defaultConfig :: Config
defaultConfig = Config
  { provider      = "anthropic"
  , modelRoles    = defaultModelRoles
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

defaultModelRoles :: Map ModelRole ModelSpec
defaultModelRoles = Map.fromList
  [ (Architect,  ModelSpec "claude-sonnet-4-20250514"  (Just 0.7) Nothing (Just 8192))
  , (Coder,      ModelSpec "claude-sonnet-4-20250514"  (Just 0.0) Nothing (Just 8192))
  , (Validator,  ModelSpec "claude-sonnet-4-20250514"  (Just 0.0) Nothing (Just 4096))
  , (Summarizer, ModelSpec "claude-haiku-3-5-20241022" (Just 0.0) Nothing (Just 4096))
  ]

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