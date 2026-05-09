module CodeStar.PlanExecution
  ( -- * Execution
    runWithPlan
  , runWithPlanDag

    -- * Config
  , PlanExecutionConfig (..)
  , defaultPlanExecutionConfig
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base
  ( CompletionRequest (..)
  , CompletionResponse (..)
  , Content (..)
  , LlmClientDict (..)
  , Message (..)
  , Role (..)
  )
import CodeStar.Localization
  ( LocalizationResult (..)
  , LocalizedFile (..)
  , LocalizedFunction (..)
  , defaultLocalizationConfig
  , localize
  )
import CodeStar.Planning
  ( Plan (..)
  , PlanFingerprint
  , PlanStep (..)
  , StepStatus (..)
  , fingerprintPlan
  , validatePlan
  )
import CodeStar.Types
  ( CheckResult (..)
  , ControlSignal (..)
  , Evidence (..)
  , ObjectiveSpec (..)
  , StepId (..)
  , TaskType (..)
  )

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data PlanExecutionConfig = PlanExecutionConfig
  { maxReplans :: !Int
  -- ^ how many times to replan before giving up
  , repoContext :: !Text
  -- ^ pre-rendered repo map injected into prompts
  }
  deriving stock (Eq, Show)

defaultPlanExecutionConfig :: PlanExecutionConfig
defaultPlanExecutionConfig =
  PlanExecutionConfig
    { maxReplans = 3
    , repoContext = Text.empty
    }

-- --------------------------------------------------------------------
-- Top-level entry point
-- --------------------------------------------------------------------

{- | Execute a task using the three-phase List plan pipeline.
  Phase 1 (Architect): select relevant files from the repo map.
  Phase 2 (Planner):  generate a task list with Uses: per step.
  Phase 3 (Coder):    execute each step with scoped context.
For Bug tasks, runs Localization first and injects results into
the Planner prompt. Returns NeedsInput if a repeated approach is detected.
-}
runWithPlan ::
  -- | Architect role
  LlmClientDict ->
  -- | Planner role (same or different)
  LlmClientDict ->
  -- | Coder role
  LlmClientDict ->
  PlanExecutionConfig ->
  ObjectiveSpec ->
  -- | already-tried plan fingerprints
  Set PlanFingerprint ->
  -- | step executor (provided by AgentLoop)
  (PlanStep -> IO ControlSignal) ->
  IO ControlSignal
runWithPlan arch planner _coder cfg spec triedFps executeStep = do
  locCtx <- case spec.taskType of
    Bug -> do
      lr <- localize arch defaultLocalizationConfig spec cfg.repoContext
      pure (either (const Text.empty) formatLocResult lr)
    _ -> pure Text.empty

  archResult <- phase1Architect arch spec cfg
  case archResult of
    Left err -> pure (Blocked err)
    Right fileSelection -> do
      planResult <- phase2PlannerValidated planner spec cfg fileSelection locCtx
      case planResult of
        Left err -> pure (Blocked err)
        Right plan ->
          let fp = fingerprintPlan plan
           in if Set.member fp triedFps
                then pure (NeedsInput "Repeated approach detected — need a different strategy")
                else phase3Execute cfg.maxReplans plan executeStep

-- --------------------------------------------------------------------
-- Phase 1: Architect
-- --------------------------------------------------------------------

phase1Architect ::
  LlmClientDict ->
  ObjectiveSpec ->
  PlanExecutionConfig ->
  IO (Either Text Text)
phase1Architect client spec cfg = do
  let prompt =
        Text.unlines
          [ "You are an expert software architect. Given the task and codebase,"
          , "identify the files that need to be read or modified."
          , ""
          , "Task: " <> spec.description
          , ""
          , "Repository map:"
          , cfg.repoContext
          , ""
          , "List the relevant files, one per line."
          ]
  singleCall client prompt

-- --------------------------------------------------------------------
-- Phase 2: Planner
-- --------------------------------------------------------------------

phase2Planner ::
  LlmClientDict ->
  ObjectiveSpec ->
  PlanExecutionConfig ->
  -- | architect's file selection
  Text ->
  -- | localization context (empty for non-Bug tasks)
  Text ->
  IO (Either Text Plan)
phase2Planner client spec _cfg fileSelection locCtx = do
  let locSection =
        if Text.null locCtx
          then ""
          else "\n\nFault localisation:\n" <> locCtx
      prompt =
        Text.unlines
          [ "You are a task planner. Decompose the task into sequential steps."
          , "For each step, list the files it needs (Uses:)."
          , ""
          , "Task: " <> spec.description
          , "Files available: " <> fileSelection <> locSection
          , ""
          , "Respond with steps in this format:"
          , "STEP: <description>"
          , "USES: <file1>, <file2>"
          , "---"
          ]
  result <- singleCall client prompt
  case result of
    Left err -> pure (Left err)
    Right txt -> pure (Right (parsePlanText txt))

phase2PlannerValidated ::
  LlmClientDict ->
  ObjectiveSpec ->
  PlanExecutionConfig ->
  Text ->
  Text ->
  IO (Either Text Plan)
phase2PlannerValidated client spec cfg fileSelection locCtx = go 0
 where
  go attempts = do
    result <- phase2Planner client spec cfg fileSelection locCtx
    case result of
      Left err -> pure (Left err)
      Right plan ->
        case validatePlan plan of
          Right () -> pure (Right plan)
          Left _ ->
            if attempts >= cfg.maxReplans
              then pure (Left "Generated plan failed validation")
              else go (attempts + 1)

parsePlanText :: Text -> Plan
parsePlanText txt =
  let blocks = splitOn "---" (Text.lines txt)
      steps = zipWith parseBlock [0 ..] (filter (not . null) blocks)
   in Plan{steps = steps}
 where
  splitOn sep =
    foldr
      ( \l acc -> case acc of
          (cur : rest) ->
            if Text.strip l == sep
              then [] : cur : rest
              else (l : cur) : rest
          [] -> [[l]]
      )
      [[]]

  parseBlock i ls =
    let desc = Text.strip $ maybe "" id (findPrefix "STEP:" ls)
        uses = maybe [] parseUses (findPrefix "USES:" ls)
        sid = maybe (Text.pack ("step-" <> show (i :: Int))) Text.strip (findPrefix "ID:" ls)
        deps = maybe [] parseDepends (findPrefix "DEPENDS:" ls)
     in PlanStep
          { stepId = coerce sid
          , description = desc
          , contextFiles = uses
          , dependsOn = Set.fromList (map coerce deps)
          , alternatives = []
          , status = Pending
          }

  findPrefix p ls = case filter (Text.isPrefixOf p . Text.strip) ls of
    [] -> Nothing
    (l : _) -> Just (Text.drop (Text.length p) (Text.strip l))

  parseUses t =
    map (Text.unpack . Text.strip) (Text.splitOn "," t)

  parseDepends t =
    filter (not . Text.null) (map Text.strip (Text.splitOn "," t))

  coerce = coerceStepId

coerceStepId :: Text -> StepId
coerceStepId = StepId

-- --------------------------------------------------------------------
-- Phase 3: Execute
-- --------------------------------------------------------------------

phase3Execute :: Int -> Plan -> (PlanStep -> IO ControlSignal) -> IO ControlSignal
phase3Execute maxRetries plan executeStep = go plan.steps
 where
  go [] =
    pure
      ( Done
          Evidence
            { testsPass = NotChecked
            , buildSucceeds = NotChecked
            , filesVerified = []
            , regressions = []
            }
      )
  go (s : ss) = do
    sig <- executeWithRetry maxRetries executeStep s
    case sig of
      Continue -> go ss
      Done e -> pure (Done e)
      NeedsInput q -> pure (NeedsInput q)
      Blocked r -> pure (Blocked r)

-- --------------------------------------------------------------------
-- DAG entry point
-- --------------------------------------------------------------------

{- | Like runWithPlan but executes steps in dependency order via Kahn's
algorithm. Steps whose dependsOn set is satisfied execute sequentially
in topological order (no parallelism for MVP).
-}
runWithPlanDag ::
  LlmClientDict ->
  LlmClientDict ->
  LlmClientDict ->
  PlanExecutionConfig ->
  ObjectiveSpec ->
  Set PlanFingerprint ->
  (PlanStep -> IO ControlSignal) ->
  IO ControlSignal
runWithPlanDag arch planner _coder cfg spec triedFps executeStep = do
  locCtx <- case spec.taskType of
    Bug -> do
      lr <- localize arch defaultLocalizationConfig spec cfg.repoContext
      pure (either (const Text.empty) formatLocResult lr)
    _ -> pure Text.empty
  archResult <- phase1Architect arch spec cfg
  case archResult of
    Left err -> pure (Blocked err)
    Right fileSelection -> do
      planResult <- phase2PlannerValidated planner spec cfg fileSelection locCtx
      case planResult of
        Left err -> pure (Blocked err)
        Right plan ->
          let fp = fingerprintPlan plan
           in if Set.member fp triedFps
                then pure (NeedsInput "Repeated approach detected")
                else dagExecute cfg.maxReplans plan executeStep

dagExecute :: Int -> Plan -> (PlanStep -> IO ControlSignal) -> IO ControlSignal
dagExecute maxRetries plan executeStep =
  let ready = [s | s <- plan.steps, Set.null s.dependsOn]
   in go ready Set.empty plan.steps
 where
  go [] done remaining
    | all (\s -> Set.member s.stepId done) remaining =
        pure
          ( Done
              Evidence
                { testsPass = NotChecked
                , buildSucceeds = NotChecked
                , filesVerified = []
                , regressions = []
                }
          )
    | otherwise = pure (Blocked "DAG deadlock: unresolvable dependencies")
  go (step : queue) done allSteps = do
    sig <- executeWithRetry maxRetries executeStep step
    case sig of
      Blocked r -> pure (Blocked r)
      NeedsInput q -> pure (NeedsInput q)
      _ -> do
        let done' = Set.insert step.stepId done
            newlyReady =
              [ s
              | s <- allSteps
              , not (Set.member s.stepId done')
              , s.stepId `notElem` map (.stepId) queue
              , Set.isSubsetOf s.dependsOn done'
              ]
        go (queue ++ newlyReady) done' allSteps

executeWithRetry :: Int -> (PlanStep -> IO ControlSignal) -> PlanStep -> IO ControlSignal
executeWithRetry maxRetries executeStep step = go 0
 where
  go attempts = do
    sig <- executeStep step
    case sig of
      Continue -> pure Continue
      Done e -> pure (Done e)
      NeedsInput _ | attempts < maxRetries -> go (attempts + 1)
      Blocked _ | attempts < maxRetries -> go (attempts + 1)
      _ -> pure sig

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

singleCall :: LlmClientDict -> Text -> IO (Either Text Text)
singleCall client prompt = do
  let req =
        CompletionRequest
          { messages = [Message User [TextContent prompt]]
          , systemPrompt = Nothing
          , tools = []
          , maxTokens = 4096
          , temperature = Just 0.0
          , topP = Nothing
          }
  result <- client.complete req
  case result of
    Left err -> pure (Left (Text.pack (show err)))
    Right resp -> pure (Right (extractText resp))

extractText :: CompletionResponse -> Text
extractText resp = foldMap go resp.responseContent
 where
  go (TextContent t) = t
  go _ = Text.empty

formatLocResult :: LocalizationResult -> Text
formatLocResult lr =
  let files = Text.intercalate ", " (map (Text.pack . (.lfPath)) lr.lrFiles)
      fns = Text.intercalate "; " (map (\f -> f.lnName <> " in " <> Text.pack f.lnFile) lr.lrFunctions)
   in "Suspicious files: " <> files <> "\nSuspicious functions: " <> fns
