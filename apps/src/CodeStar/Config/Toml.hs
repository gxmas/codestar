module CodeStar.Config.Toml
  ( parseTomlConfig
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Control.Monad (join)
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import TOML (Decoder, Value, decodeWith, getFieldOpt, getFieldWith)

import CodeStar.Config.Types
import CodeStar.Types (ModelRole (..), PlanningMode (..))

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
  mdls         <- optSection modelsDecoder' "models"

  pure PartialConfig
    { provider      = Last provider
    , modelRoles    = Last (join mdls)
    , planningMode  = Last (fmap toPlanningMode planningMode)
    , sandboxMode   = Last Nothing
    , authMode      = Last Nothing
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

modelsDecoder' :: Decoder (Maybe (Map ModelRole ModelSpec))
modelsDecoder' = do
  arch <- optFieldWith modelSpecDecoder "architect"
  cod  <- optFieldWith modelSpecDecoder "coder"
  val  <- optFieldWith modelSpecDecoder "validator"
  summ <- optFieldWith modelSpecDecoder "summarizer"
  let pairs = concat
        [ maybe [] (\s -> [(Architect, s)]) arch
        , maybe [] (\s -> [(Coder, s)]) cod
        , maybe [] (\s -> [(Validator, s)]) val
        , maybe [] (\s -> [(Summarizer, s)]) summ
        ]
  pure $ if null pairs
    then Nothing
    else Just (Map.fromList pairs)

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

modelSpecDecoder :: Decoder ModelSpec
modelSpecDecoder = ModelSpec
  <$> getFieldOpt "name" `withDefault` ""
  <*> getFieldOpt "temperature"
  <*> getFieldOpt "top_p"
  <*> getFieldOpt "max_tokens"
 where
  withDefault dec def = fmap (maybe def id) dec

optFieldWith :: Decoder a -> Text -> Decoder (Maybe a)
optFieldWith dec key = do
  mVal <- getFieldOpt key
  case (mVal :: Maybe Value) of
    Nothing -> pure Nothing
    Just _  -> Just <$> getFieldWith dec key

