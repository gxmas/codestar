module CodeStar.Config.Toml
  ( parseTomlConfig
  ) where


import Control.Monad (join)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import TOML (Decoder, Value, decodeWith, getFieldOpt, getFieldWith, getArrayOf, tomlDecoder)

import CodeStar.Config.Types
import CodeStar.Types (PlanningMode (..))

parseTomlConfig :: Text -> Either Text PartialConfig
parseTomlConfig input =
  case decodeWith configDecoder input of
    Right pc -> Right pc
    Left err -> Left (Text.pack (show err))

configDecoder :: Decoder PartialConfig
configDecoder = do
  provider     <- getFieldOpt "provider"
  planningMode <- getFieldOpt "planning_mode"
  indexStrat   <- getFieldOpt "index_strategy"
  permissions  <- getFieldOpt "permissions"
  let mcpEndpoints = Nothing :: Maybe [McpEndpoint]
  srv          <- optSection serverDecoder "server"
  tel          <- optSection telemetryDecoder "telemetry"
  ctx          <- optSection contextDecoder "context"
  comp         <- optSection compactionDecoder "compaction"
  sh           <- optSection shellDecoder "shell"
  sess         <- optSection sessionDecoder "session"
  gr           <- optSection guardrailsDecoder "guardrails"
  bgt          <- optSection budgetDecoder "budget"
  rm           <- optSection repoMapDecoder "repomap"
  mem          <- optSection memoryDecoder "memory"
  -- [[model_entries]] uses a distinct key to avoid collision with legacy [models.architect] sub-tables
  mdlEntries   <- optFieldWith (getArrayOf modelEntryDecoder) "model_entries" :: Decoder (Maybe [ModelEntry])
  activeMdl    <- getFieldOpt "active_model"
  authSect     <- optSection authDecoder "auth"

  pure PartialConfig
    { provider      = Last provider
    , models        = Last mdlEntries
    , activeModel   = Last activeMdl
    , planningMode  = Last (fmap toPlanningMode planningMode)
    , sandboxMode   = Last Nothing
    , auth          = maybe mempty id authSect
    , workspacePath = Last Nothing
    , apiKey        = Last Nothing
    , indexStrategy = Last (fmap toIndexStrategy indexStrat)
    , permissions   = Last permissions
    , mcpEndpoints  = Last mcpEndpoints
    , server        = maybe mempty id srv
    , telemetry     = maybe mempty id tel
    , context       = maybe mempty id ctx
    , compaction    = maybe mempty id comp
    , shell         = maybe mempty id sh
    , session       = maybe mempty id sess
    , guardrails    = maybe mempty id gr
    , budgets       = maybe mempty id bgt
    , repomap       = maybe mempty id rm
    , memory        = maybe mempty id mem
    }

optSection :: Decoder a -> Text -> Decoder (Maybe a)
optSection dec key = do
  mVal <- getFieldOpt key
  case (mVal :: Maybe Value) of
    Nothing -> pure Nothing
    Just _  -> Just <$> getFieldWith dec key

-- --------------------------------------------------------------------
-- Section decoders
-- --------------------------------------------------------------------

serverDecoder :: Decoder PartialServerSection
serverDecoder = do
  p  <- getFieldOpt "port"
  h  <- getFieldOpt "host"
  ht <- getFieldOpt "http_timeout"
  gs <- getFieldOpt "graceful_shutdown_timeout"
  pingInt <- getFieldOpt "ping_interval"
  pure PartialServerSection
    { port                    = Last p
    , host                    = Last h
    , httpTimeout             = Last ht
    , gracefulShutdownTimeout = Last gs
    , pingInterval            = Last pingInt
    }

telemetryDecoder :: Decoder PartialTelemetrySection
telemetryDecoder = do
  m  <- fmap (fmap toTelemetryMode) (getFieldOpt "mode")
  sn <- getFieldOpt "service_name"
  ep <- getFieldOpt "endpoint"
  ls <- getFieldOpt "log_to_stderr"
  me <- getFieldOpt "metrics_enabled"
  mbh <- getFieldOpt "metrics_bind_host"
  mp <- getFieldOpt "metrics_port"
  sr <- getFieldOpt "sample_rate"
  pure PartialTelemetrySection
    { mode           = Last (Control.Monad.join m)
    , serviceName    = Last sn
    , endpoint       = Last (Just <$> ep)
    , logToStderr    = Last ls
    , metricsEnabled = Last me
    , metricsBindHost = Last mbh
    , metricsPort    = Last (Just <$> mp)
    , sampleRate     = Last sr
    }

contextDecoder :: Decoder PartialContextSection
contextDecoder = do
  mt <- getFieldOpt "max_tokens"
  rr <- getFieldOpt "repo_map_reserve"
  mr <- getFieldOpt "memory_reserve"
  cr <- getFieldOpt "compaction_reserve"
  rsr <- getFieldOpt "response_reserve"
  pure PartialContextSection
    { maxTokens         = Last mt
    , repoMapReserve    = Last rr
    , memoryReserve     = Last mr
    , compactionReserve = Last cr
    , responseReserve   = Last rsr
    }

compactionDecoder :: Decoder PartialCompactionSection
compactionDecoder = do
  tf <- getFieldOpt "trigger_fraction"
  mc <- getFieldOpt "max_context_tokens"
  pure PartialCompactionSection
    { triggerFraction  = Last tf
    , maxContextTokens = Last mc
    }

shellDecoder :: Decoder PartialShellSection
shellDecoder = do
  dt <- getFieldOpt "default_timeout"
  mc <- getFieldOpt "max_concurrent"
  ot <- getFieldOpt "output_truncation"
  pure PartialShellSection
    { defaultTimeout   = Last dt
    , maxConcurrent    = Last mc
    , outputTruncation = Last ot
    }

sessionDecoder :: Decoder PartialSessionSection
sessionDecoder = do
  mp <- getFieldOpt "max_per_user"
  it <- getFieldOpt "inactivity_timeout"
  pure PartialSessionSection
    { maxPerUser        = Last mp
    , inactivityTimeout = Last it
    }

guardrailsDecoder :: Decoder PartialGuardrailsSection
guardrailsDecoder = do
  dl <- fmap (fmap Set.fromList) (getFieldOpt "deny_list")
  al <- fmap (fmap Set.fromList) (getFieldOpt "allow_list")
  sp <- getFieldOpt "secret_patterns"
  pure PartialGuardrailsSection
    { denyList       = Last dl
    , allowList      = Last al
    , secretPatterns = Last sp
    }

budgetDecoder :: Decoder PartialBudgetSection
budgetDecoder = do
  ms  <- getFieldOpt "max_steps"
  stm <- getFieldOpt "session_token_max"
  dtm <- getFieldOpt "daily_token_max"
  pure PartialBudgetSection
    { maxSteps        = Last ms
    , sessionTokenMax = Last (Just <$> (stm :: Maybe Word64))
    , dailyTokenMax   = Last (Just <$> (dtm :: Maybe Word64))
    }

repoMapDecoder :: Decoder PartialRepoMapSection
repoMapDecoder = do
  ri <- getFieldOpt "rebuild_interval_ms"
  mt <- getFieldOpt "max_tokens"
  bs <- getFieldOpt "batch_size"
  pure PartialRepoMapSection
    { rebuildIntervalMs = Last ri
    , rmMaxTokens       = Last mt
    , batchSize         = Last bs
    }

memoryDecoder :: Decoder PartialMemorySection
memoryDecoder = do
  en <- getFieldOpt "enabled"
  me <- getFieldOpt "max_entries"
  ad <- getFieldOpt "auto_discover"
  pure PartialMemorySection
    { enabled      = Last en
    , maxEntries   = Last me
    , autoDiscover = Last ad
    }

authDecoder :: Decoder PartialAuthSection
authDecoder = do
  m   <- getFieldOpt "mode"
  uri <- getFieldOpt "jwks_uri"
  inl <- getFieldOpt "jwks_inline"
  sec <- getFieldOpt "secret"
  iss <- getFieldOpt "issuer"
  aud <- getFieldOpt "audience"
  cu  <- getFieldOpt "claim_user_id"
  co  <- getFieldOpt "claim_org_id"
  cr  <- getFieldOpt "claim_roles"
  ttl <- getFieldOpt "cache_ttl_seconds"
  pure PartialAuthSection
    { mode            = Last m
    , jwksUri         = Last uri
    , jwksInline      = Last inl
    , secret          = Last sec
    , issuer          = Last (Just <$> iss)
    , audience        = Last (Just <$> aud)
    , claimUserId     = Last cu
    , claimOrgId      = Last co
    , claimRoles      = Last cr
    , cacheTtlSeconds = Last ttl
    }

modelEntryDecoder :: Decoder ModelEntry
modelEntryDecoder = do
  n   <- getFieldWith tomlDecoder "name"
  prv <- getFieldWith tomlDecoder "provider"
  mdl <- getFieldWith tomlDecoder "model"
  k   <- fmap (maybe (ApiKey "") ApiKey) (getFieldOpt "api_key")
  tmp <- getFieldOpt "temperature"
  tp  <- getFieldOpt "top_p"
  mt  <- getFieldOpt "max_tokens"
  pure ModelEntry
    { meName        = n
    , meProvider    = prv
    , meModel       = mdl
    , meApiKey      = k
    , meTemperature = tmp
    , meTopP        = tp
    , meMaxTokens   = mt
    }

-- --------------------------------------------------------------------
-- Enum conversions
-- --------------------------------------------------------------------

toTelemetryMode :: Text -> Maybe TelemetryMode
toTelemetryMode "otlp"   = Just TelemetryOtlp
toTelemetryMode "stderr"  = Just TelemetryStderr
toTelemetryMode "off"     = Just TelemetryOff
toTelemetryMode _         = Nothing

toIndexStrategy :: Text -> IndexStrategy
toIndexStrategy "repomap"  = RepoMapIndex
toIndexStrategy "semantic" = SemanticIndex
toIndexStrategy _          = NoIndex

toPlanningMode :: Text -> PlanningMode
toPlanningMode "list" = ListPlan
toPlanningMode "dag"  = DagPlan
toPlanningMode _      = NoPlan

optFieldWith :: Decoder a -> Text -> Decoder (Maybe a)
optFieldWith dec key = do
  mVal <- getFieldOpt key
  case (mVal :: Maybe Value) of
    Nothing -> pure Nothing
    Just _  -> Just <$> getFieldWith dec key

