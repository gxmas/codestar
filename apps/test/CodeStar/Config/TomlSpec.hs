{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Config.TomlSpec (spec) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import CodeStar.Config.Gen ()
import CodeStar.Config.Toml (parseTomlConfig)
import CodeStar.Config.Types
import CodeStar.Config.Types qualified as CT
import CodeStar.Types (ModelRole (..), PlanningMode (..))

-- --------------------------------------------------------------------
-- Normalisation
--
-- The TOML decoder hard-codes some fields to Last Nothing and cannot
-- represent Last (Just Nothing) for nested-Maybe fields.  Normalise
-- a PartialConfig onto the representable subset before comparing.
-- --------------------------------------------------------------------

-- Last (Just Nothing) is structurally distinct from Last Nothing but
-- both produce Last Nothing after a TOML round-trip.
collapseJustNothing :: Last (Maybe a) -> Last (Maybe a)
collapseJustNothing (Last (Just Nothing)) = Last Nothing
collapseJustNothing x                     = x

normalizeTelemetry :: PartialTelemetrySection -> PartialTelemetrySection
normalizeTelemetry (PartialTelemetrySection mo sn ep ls me mbh mp sr) =
  PartialTelemetrySection mo sn (collapseJustNothing ep) ls me mbh (collapseJustNothing mp) sr

normalizeBudget :: PartialBudgetSection -> PartialBudgetSection
normalizeBudget (PartialBudgetSection ms stm dtm) =
  PartialBudgetSection ms (collapseJustNothing stm) (collapseJustNothing dtm)

-- Fields absent from the TOML schema (hard-coded to mempty/Last Nothing by
-- the decoder): sandboxMode, auth, workspacePath, apiKey, mcpEndpoints.
normalizeForToml :: PartialConfig -> PartialConfig
normalizeForToml (PartialConfig pr mr _ _ pm _ _ _ _ is pe _ sv tel cx cp sh se gr bu rm mn) =
  PartialConfig
    pr mr (Last Nothing) (Last Nothing) pm
    (Last Nothing) mempty (Last Nothing) (Last Nothing)
    is pe (Last Nothing)
    sv (normalizeTelemetry tel) cx cp sh se gr (normalizeBudget bu) rm mn

-- --------------------------------------------------------------------
-- TOML rendering
-- --------------------------------------------------------------------

renderToml :: PartialConfig -> Text
renderToml p = mconcat $ concat
  [ [ kv "provider"       renderStr          v | v <- mf p.provider ]
  , [ kv "planning_mode"  renderPlanningMode v | v <- mf p.planningMode ]
  , [ kv "index_strategy" renderIndexStrategy v | v <- mf p.indexStrategy ]
  , [ listKv "permissions" vs                   | vs <- mf p.permissions ]
  , renderModelRoles (getLast p.modelRoles)
  , renderServer     p.server
  , renderTelemetry  p.telemetry
  , renderContext    p.context
  , renderCompaction p.compaction
  , renderShell      p.shell
  , renderSession    p.session
  , renderGuardrails p.guardrails
  , renderBudget     p.budgets
  , renderRepoMap    p.repomap
  , renderMemory     p.memory
  ]

-- Extract the value from Last, or empty list if Nothing.
mf :: Last a -> [a]
mf = maybe [] pure . getLast

-- key = value\n
kv :: Text -> (a -> Text) -> a -> Text
kv k f v = k <> " = " <> f v <> "\n"

-- key = ["a", "b"]\n
listKv :: Text -> [Text] -> Text
listKv k vs = k <> " = [" <> Text.intercalate ", " (map renderStr vs) <> "]\n"

-- Emit a section header only if there are fields to write.
sectionLines :: Text -> [Text] -> [Text]
sectionLines _   []     = []
sectionLines hdr fields = ("[" <> hdr <> "]\n") : fields

renderStr :: Text -> Text
renderStr t = "\"" <> t <> "\""

renderBool :: Bool -> Text
renderBool True  = "true"
renderBool False = "false"

renderInt :: Int -> Text
renderInt = Text.pack . show

renderWord64 :: Word64 -> Text
renderWord64 = Text.pack . show

renderDouble :: Double -> Text
renderDouble = Text.pack . show

renderPlanningMode :: PlanningMode -> Text
renderPlanningMode NoPlan   = "\"none\""   -- toPlanningMode "none" -> NoPlan (else case)
renderPlanningMode ListPlan = "\"list\""
renderPlanningMode DagPlan  = "\"dag\""

renderIndexStrategy :: IndexStrategy -> Text
renderIndexStrategy NoIndex       = "\"none\""   -- toIndexStrategy "none" -> NoIndex (else case)
renderIndexStrategy RepoMapIndex  = "\"repomap\""
renderIndexStrategy SemanticIndex = "\"semantic\""

renderTelemetryMode :: TelemetryMode -> Text
renderTelemetryMode TelemetryOtlp   = "\"otlp\""
renderTelemetryMode TelemetryStderr = "\"stderr\""
renderTelemetryMode TelemetryOff    = "\"off\""

renderModelRoles :: Maybe (Map ModelRole ModelSpec) -> [Text]
renderModelRoles Nothing  = []
renderModelRoles (Just m) = concatMap go (Map.toList m)
  where
    go (role, ms) =
      sectionLines ("models." <> roleKey role) (renderModelSpec ms)
    roleKey Architect  = "architect"
    roleKey Coder      = "coder"
    roleKey Validator  = "validator"
    roleKey Summarizer = "summarizer"

renderModelSpec :: ModelSpec -> [Text]
renderModelSpec ms = concat
  [ [ kv "name"        renderStr    ms.modelName ]
  , [ kv "temperature" renderDouble d | Just d <- [ms.temperature] ]
  , [ kv "top_p"       renderDouble d | Just d <- [ms.topP] ]
  , [ kv "max_tokens"  renderInt    n | Just n <- [ms.maxTokens] ]
  ]

renderServer :: PartialServerSection -> [Text]
renderServer s = sectionLines "server" $ concat
  [ [ kv "port"                      renderInt v | v <- mf s.port ]
  , [ kv "host"                      renderStr v | v <- mf s.host ]
  , [ kv "http_timeout"              renderInt v | v <- mf s.httpTimeout ]
  , [ kv "graceful_shutdown_timeout" renderInt v | v <- mf s.gracefulShutdownTimeout ]
  , [ kv "ping_interval"             renderInt v | v <- mf s.pingInterval ]
  ]

renderTelemetry :: PartialTelemetrySection -> [Text]
renderTelemetry s = sectionLines "telemetry" $ concat
  [ [ kv "mode"            renderTelemetryMode m | m <- mf s.mode ]
  , [ kv "service_name"    renderStr           n | n <- mf s.serviceName ]
    -- endpoint :: Last (Maybe Text); after normalisation only Just (Just t) reaches here
  , [ kv "endpoint"        renderStr           t | Just (Just t) <- [getLast s.endpoint] ]
  , [ kv "log_to_stderr"   renderBool          b | b <- mf s.logToStderr ]
  , [ kv "metrics_enabled" renderBool          b | b <- mf s.metricsEnabled ]
  , [ kv "metrics_bind_host" renderStr         h | h <- mf s.metricsBindHost ]
  , [ kv "metrics_port"    renderInt           n | Just (Just n) <- [getLast s.metricsPort] ]
  , [ kv "sample_rate"    renderDouble        r | r <- mf s.sampleRate ]
  ]

renderContext :: PartialContextSection -> [Text]
renderContext s = sectionLines "context" $ concat
  [ [ kv "max_tokens"         renderInt v | v <- mf s.maxTokens ]
  , [ kv "repo_map_reserve"   renderInt v | v <- mf s.repoMapReserve ]
  , [ kv "memory_reserve"     renderInt v | v <- mf s.memoryReserve ]
  , [ kv "compaction_reserve" renderInt v | v <- mf s.compactionReserve ]
  , [ kv "response_reserve"   renderInt v | v <- mf s.responseReserve ]
  ]

renderCompaction :: PartialCompactionSection -> [Text]
renderCompaction s = sectionLines "compaction" $ concat
  [ [ kv "trigger_fraction"   renderDouble v | v <- mf s.triggerFraction ]
  , [ kv "max_context_tokens" renderInt    v | v <- mf s.maxContextTokens ]
  ]

renderShell :: PartialShellSection -> [Text]
renderShell s = sectionLines "shell" $ concat
  [ [ kv "default_timeout"   renderInt v | v <- mf s.defaultTimeout ]
  , [ kv "max_concurrent"    renderInt v | v <- mf s.maxConcurrent ]
  , [ kv "output_truncation" renderInt v | v <- mf s.outputTruncation ]
  ]

renderSession :: PartialSessionSection -> [Text]
renderSession s = sectionLines "session" $ concat
  [ [ kv "max_per_user"        renderInt v | v <- mf s.maxPerUser ]
  , [ kv "inactivity_timeout"  renderInt v | v <- mf s.inactivityTimeout ]
  ]

renderGuardrails :: PartialGuardrailsSection -> [Text]
renderGuardrails s = sectionLines "guardrails" $ concat
  [ [ listKv "deny_list"       (Set.toAscList dl) | dl <- mf s.denyList ]
  , [ listKv "allow_list"      (Set.toAscList al) | al <- mf s.allowList ]
  , [ listKv "secret_patterns" sp                  | sp <- mf s.secretPatterns ]
  ]

renderBudget :: PartialBudgetSection -> [Text]
renderBudget s = sectionLines "budget" $ concat
  [ [ kv "max_steps"         renderInt    v | v <- mf s.maxSteps ]
  , [ kv "session_token_max" renderWord64 w | Just (Just w) <- [getLast s.sessionTokenMax] ]
  , [ kv "daily_token_max"   renderWord64 w | Just (Just w) <- [getLast s.dailyTokenMax] ]
  ]

renderRepoMap :: PartialRepoMapSection -> [Text]
renderRepoMap s = sectionLines "repomap" $ concat
  [ [ kv "rebuild_interval_ms" renderInt v | v <- mf s.rebuildIntervalMs ]
  , [ kv "max_tokens"          renderInt v | v <- mf s.rmMaxTokens ]
  , [ kv "batch_size"          renderInt v | v <- mf s.batchSize ]
  ]

renderMemory :: PartialMemorySection -> [Text]
renderMemory s = sectionLines "memory" $ concat
  [ [ kv "enabled"       renderBool b | b <- mf s.enabled ]
  , [ kv "max_entries"   renderInt  n | n <- mf s.maxEntries ]
  , [ kv "auto_discover" renderBool b | b <- mf s.autoDiscover ]
  ]

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Config.Toml" $ do

  prop "TOML round-trip: parse (render (normalize p)) == normalize p" $
    \p ->
      let norm     = normalizeForToml p
          tomlText = renderToml norm
          result   = parseTomlConfig tomlText
       in counterexample ("TOML:\n" <> Text.unpack tomlText) $
          result === Right norm

  it "parses top-level and section values" $ do
    let input =
          "provider = \"openai\"\n\
          \planning_mode = \"dag\"\n\
          \\n\
          \[server]\n\
          \port = 9090\n\
          \\n\
          \[telemetry]\n\
          \metrics_bind_host = \"0.0.0.0\"\n"
    case parseTomlConfig input of
      Left err -> expectationFailure ("Expected Right PartialConfig, got Left: " <> show err)
      Right pc -> do
        let CT.PartialConfig
              { CT.provider = p
              , CT.planningMode = pm
              , CT.server = CT.PartialServerSection{CT.port = prt}
              , CT.telemetry = CT.PartialTelemetrySection{CT.metricsBindHost = mbh}
              } = pc
        getLast p `shouldBe` Just "openai"
        getLast pm `shouldBe` Just DagPlan
        getLast prt `shouldBe` Just 9090
        getLast mbh `shouldBe` Just "0.0.0.0"

  it "parses model role sections into modelRoles map" $ do
    let input =
          "[models.architect]\n\
          \name = \"model-a\"\n\
          \max_tokens = 1234\n"
    case parseTomlConfig input of
      Left err -> expectationFailure ("Expected Right PartialConfig, got Left: " <> show err)
      Right pc -> do
        let CT.PartialConfig{CT.modelRoles = roles} = pc
        getLast roles `shouldSatisfy` maybe False (not . null)

  it "returns Left for invalid TOML syntax" $
    case parseTomlConfig "[server" of
      Left _ -> pure ()
      Right _ -> expectationFailure "Expected parse failure for invalid TOML"
