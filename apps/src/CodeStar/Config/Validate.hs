module CodeStar.Config.Validate
  ( resolve
  ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Last (..))
import Data.Text (Text)

import CodeStar.Config.Defaults (defaultConfig)
import CodeStar.Config.Types

resolve :: PartialConfig -> Either (NonEmpty ConfigError) Config
resolve partial =
  let cfg = applyDefaults partial
  in case validate cfg of
       [] -> Right cfg
       (e : es) -> Left (e :| es)

-- | Apply defaults for any unset field.
applyDefaults :: PartialConfig -> Config
applyDefaults p = Config
  { provider      = fromLast defaultConfig.provider       p.provider
  , modelRoles    = fromLast defaultConfig.modelRoles     p.modelRoles
  , planningMode  = fromLast defaultConfig.planningMode   p.planningMode
  , sandboxMode   = fromLast defaultConfig.sandboxMode    p.sandboxMode
  , authMode      = fromLast defaultConfig.authMode       p.authMode
  , workspacePath = fromLast defaultConfig.workspacePath  p.workspacePath
  , apiKey        = fromLast defaultConfig.apiKey          p.apiKey
  , indexStrategy = fromLast defaultConfig.indexStrategy   p.indexStrategy
  , permissions   = fromLast defaultConfig.permissions     p.permissions
  , mcpEndpoints  = fromLast defaultConfig.mcpEndpoints   p.mcpEndpoints
  , server        = resolveServer   p.server
  , telemetry     = resolveTelemetry p.telemetry
  , context       = resolveContext  p.context
  , compaction    = resolveCompaction p.compaction
  , shell         = resolveShell    p.shell
  , session       = resolveSession  p.session
  , guardrails    = resolveGuardrails p.guardrails
  , budgets       = resolveBudget   p.budgets
  , repomap       = resolveRepoMap  p.repomap
  , memory        = resolveMemory   p.memory
  }

resolveServer :: PartialServerSection -> ServerSection
resolveServer p = ServerSection
  { port                    = fromLast defaultConfig.server.port                    p.port
  , host                    = fromLast defaultConfig.server.host                    p.host
  , httpTimeout             = fromLast defaultConfig.server.httpTimeout             p.httpTimeout
  , gracefulShutdownTimeout = fromLast defaultConfig.server.gracefulShutdownTimeout p.gracefulShutdownTimeout
  , pingInterval            = fromLast defaultConfig.server.pingInterval            p.pingInterval
  }

resolveTelemetry :: PartialTelemetrySection -> TelemetrySection
resolveTelemetry p = TelemetrySection
  { mode           = fromLast defaultConfig.telemetry.mode           p.mode
  , serviceName    = fromLast defaultConfig.telemetry.serviceName    p.serviceName
  , endpoint       = fromLast defaultConfig.telemetry.endpoint       p.endpoint
  , logToStderr    = fromLast defaultConfig.telemetry.logToStderr    p.logToStderr
  , metricsEnabled = fromLast defaultConfig.telemetry.metricsEnabled p.metricsEnabled
  , metricsBindHost = fromLast defaultConfig.telemetry.metricsBindHost p.metricsBindHost
  , metricsPort    = fromLast defaultConfig.telemetry.metricsPort    p.metricsPort
  }

resolveContext :: PartialContextSection -> ContextSection
resolveContext p = ContextSection
  { maxTokens         = fromLast defaultConfig.context.maxTokens         p.maxTokens
  , repoMapReserve    = fromLast defaultConfig.context.repoMapReserve    p.repoMapReserve
  , memoryReserve     = fromLast defaultConfig.context.memoryReserve     p.memoryReserve
  , compactionReserve = fromLast defaultConfig.context.compactionReserve p.compactionReserve
  , responseReserve   = fromLast defaultConfig.context.responseReserve   p.responseReserve
  }

resolveCompaction :: PartialCompactionSection -> CompactionSection
resolveCompaction p = CompactionSection
  { triggerFraction  = fromLast defaultConfig.compaction.triggerFraction  p.triggerFraction
  , maxContextTokens = fromLast defaultConfig.compaction.maxContextTokens p.maxContextTokens
  }

resolveShell :: PartialShellSection -> ShellSection
resolveShell p = ShellSection
  { defaultTimeout   = fromLast defaultConfig.shell.defaultTimeout   p.defaultTimeout
  , maxConcurrent    = fromLast defaultConfig.shell.maxConcurrent    p.maxConcurrent
  , outputTruncation = fromLast defaultConfig.shell.outputTruncation p.outputTruncation
  }

resolveSession :: PartialSessionSection -> SessionSection
resolveSession p = SessionSection
  { maxPerUser        = fromLast defaultConfig.session.maxPerUser        p.maxPerUser
  , inactivityTimeout = fromLast defaultConfig.session.inactivityTimeout p.inactivityTimeout
  }

resolveGuardrails :: PartialGuardrailsSection -> GuardrailsSection
resolveGuardrails p = GuardrailsSection
  { denyList       = fromLast defaultConfig.guardrails.denyList       p.denyList
  , allowList      = fromLast defaultConfig.guardrails.allowList      p.allowList
  , secretPatterns = fromLast defaultConfig.guardrails.secretPatterns p.secretPatterns
  }

resolveBudget :: PartialBudgetSection -> BudgetSection
resolveBudget p = BudgetSection
  { maxSteps        = fromLast defaultConfig.budgets.maxSteps        p.maxSteps
  , sessionTokenMax = fromLast defaultConfig.budgets.sessionTokenMax p.sessionTokenMax
  , dailyTokenMax   = fromLast defaultConfig.budgets.dailyTokenMax   p.dailyTokenMax
  }

resolveRepoMap :: PartialRepoMapSection -> RepoMapSection
resolveRepoMap p = RepoMapSection
  { rebuildIntervalMs = fromLast defaultConfig.repomap.rebuildIntervalMs p.rebuildIntervalMs
  , rmMaxTokens       = fromLast defaultConfig.repomap.rmMaxTokens       p.rmMaxTokens
  , batchSize         = fromLast defaultConfig.repomap.batchSize         p.batchSize
  }

resolveMemory :: PartialMemorySection -> MemorySection
resolveMemory p = MemorySection
  { enabled      = fromLast defaultConfig.memory.enabled      p.enabled
  , maxEntries   = fromLast defaultConfig.memory.maxEntries   p.maxEntries
  , autoDiscover = fromLast defaultConfig.memory.autoDiscover p.autoDiscover
  }

-- --------------------------------------------------------------------
-- Validation rules
-- --------------------------------------------------------------------

validate :: Config -> [ConfigError]
validate cfg = concat
  [ validatePort    "server.port"         cfg.server.port
  , validatePort    "telemetry.metrics_port" `foldMap` cfg.telemetry.metricsPort
  , validatePositive "server.http_timeout"             cfg.server.httpTimeout
  , validatePositive "server.graceful_shutdown_timeout" cfg.server.gracefulShutdownTimeout
  , validatePositive "server.ping_interval"            cfg.server.pingInterval
  , validatePositive "context.max_tokens"              cfg.context.maxTokens
  , validatePositive "context.repo_map_reserve"        cfg.context.repoMapReserve
  , validatePositive "context.memory_reserve"          cfg.context.memoryReserve
  , validatePositive "context.compaction_reserve"      cfg.context.compactionReserve
  , validatePositive "context.response_reserve"        cfg.context.responseReserve
  , validatePositive "compaction.max_context_tokens"   cfg.compaction.maxContextTokens
  , validatePositive "shell.default_timeout"           cfg.shell.defaultTimeout
  , validatePositive "shell.max_concurrent"            cfg.shell.maxConcurrent
  , validatePositive "shell.output_truncation"         cfg.shell.outputTruncation
  , validatePositive "session.max_per_user"            cfg.session.maxPerUser
  , validatePositive "session.inactivity_timeout"      cfg.session.inactivityTimeout
  , validatePositive "budget.max_steps"                cfg.budgets.maxSteps
  , validatePositive "repomap.rebuild_interval_ms"     cfg.repomap.rebuildIntervalMs
  , validatePositive "repomap.max_tokens"              cfg.repomap.rmMaxTokens
  , validatePositive "repomap.batch_size"              cfg.repomap.batchSize
  , validatePositive "memory.max_entries"              cfg.memory.maxEntries
  , validateFraction "compaction.trigger_fraction"     cfg.compaction.triggerFraction
  , checkPortConflict cfg
  , checkApiKey cfg
  ]

validatePort :: Text -> Int -> [ConfigError]
validatePort name val
  | val >= 0 && val <= 65535 = []
  | otherwise = [InvalidRange name "must be 0-65535"]

validatePositive :: Text -> Int -> [ConfigError]
validatePositive name val
  | val > 0   = []
  | otherwise = [InvalidRange name "must be positive"]

validateFraction :: Text -> Double -> [ConfigError]
validateFraction name val
  | val > 0 && val <= 1 = []
  | otherwise = [InvalidFraction name val]

checkPortConflict :: Config -> [ConfigError]
checkPortConflict cfg = case cfg.telemetry.metricsPort of
  Just mp | mp == cfg.server.port ->
    [PortConflict "server.port" "telemetry.metrics_port" mp]
  _ -> []

checkApiKey :: Config -> [ConfigError]
checkApiKey cfg =
  let ApiKey k = cfg.apiKey
  in if k == "" then [MissingRequired "api_key (set ANTHROPIC_API_KEY)"] else []

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

fromLast :: a -> Last a -> a
fromLast def (Last Nothing)  = def
fromLast _   (Last (Just x)) = x
