module CodeStar.AgentLoopSpec (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable qualified as Foldable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.JsonSchema (objectSchema)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import CodeStar.AgentLoop
import CodeStar.Compaction (CompactionConfig (..), CompactionState (..), emptyCompactionState, shouldCompact)
import CodeStar.Config (defaultConfig)
import CodeStar.Guardrails qualified as GR
import CodeStar.LLM.Base
import CodeStar.Telemetry (noOpRecorder)
import CodeStar.Tools.Registry
import CodeStar.TreeSitter (GrammarRegistry (..))
import CodeStar.Types (ControlSignal (..), ModelRole (..), SessionId (..), UserId (..))

spec :: Spec
spec = describe "CodeStar.AgentLoop" $ do
  stateMachineProps
  it "supports a simplified state model: Idle -> Tooling -> Completed" $ do
    callsRef <- newIORef ([] :: [Text])
    let registry = register (okHandler callsRef "dummy") emptyRegistry
    env <-
      mkTestEnv
        registry
        [mkToolUseResponse "toolu_1" "dummy", mkDoneResponse]
        [mkSummaryResponse]
        defaultCompCfg

    let model0 = Idle
    model1 <- evolveModel model0 (runOnce env "task")
    model1 `shouldBe` Completed
    model2 <- evolveModel model1 (runOnce env "task 2")
    model2 `shouldBe` Completed

  it "routes tool dispatch to the correct handler" $ do
    alphaRef <- newIORef (0 :: Int)
    betaRef <- newIORef (0 :: Int)
    let registry =
          register (countingHandler alphaRef "alpha") $
            register (countingHandler betaRef "beta") emptyRegistry
    env <-
      mkTestEnv
        registry
        [mkToolUseResponse "toolu_b" "beta", mkDoneResponse]
        [mkSummaryResponse]
        defaultCompCfg

    _ <- runAgentTurn env "sys" (sessionFromEnv env) "route beta"
    readIORef alphaRef `shouldReturn` 0
    readIORef betaRef `shouldReturn` 1

  it "recovers from tool errors into a valid terminal state" $ do
    let registry = register failingHandler emptyRegistry
    env <-
      mkTestEnv
        registry
        [mkToolUseResponse "toolu_fail" "failing_tool", mkDoneResponse]
        [mkSummaryResponse]
        defaultCompCfg

    (sig, session) <- runAgentTurn env "sys" (sessionFromEnv env) "attempt failing tool"
    sig `shouldSatisfy` isDoneSignal
    hasErrorToolResult session.ssHistory `shouldBe` True

  it "triggers compaction when context is over threshold and reduces history growth" $ do
    let cfg = CompactionConfig{triggerFraction = 0.15, maxContextTokens = 120}
    eventsRef <- newIORef ([] :: [AgentEvent])
    env0 <-
      mkTestEnv
        emptyRegistry
        (replicate 10 mkDoneResponse)
        (replicate 10 mkSummaryResponse)
        cfg
    let env = env0{envOnEvent = \ev -> modifyIORef' eventsRef (++ [ev])}
        seedHistory =
          Seq.fromList
            [ Message User [TextContent (Text.replicate 80 "u")]
            , Message Assistant [TextContent (Text.replicate 80 "a")]
            , Message User [TextContent (Text.replicate 80 "u2")]
            , Message Assistant [TextContent (Text.replicate 80 "a2")]
            ]
        seedSession = (sessionFromEnv env){ssHistory = seedHistory}
    shouldCompact cfg seedSession.ssHistory `shouldBe` True
    (_, s1) <- runAgentTurn env "sys" seedSession "next"
    evs <- readIORef eventsRef
    evs `shouldSatisfy` any isCompacting
    Seq.length s1.ssHistory `shouldSatisfy` (< Seq.length seedHistory + 2)

-- --------------------------------------------------------------------
-- State machine properties
-- --------------------------------------------------------------------

-- | Commands we can inject into the agent: each maps to a scripted LLM
-- response sequence.  CmdToolOk/CmdToolFail each consume two responses
-- (one tool-call reply, one follow-up done reply).
data AgentCmd = CmdDone | CmdToolOk | CmdToolFail
  deriving stock (Eq, Show, Enum, Bounded)

instance Arbitrary AgentCmd where
  arbitrary = elements [minBound .. maxBound]

-- | A non-empty sequence of commands ending with CmdDone, so the mock
-- LLM always has a terminal response available regardless of length.
arbitraryCmds :: Gen [AgentCmd]
arbitraryCmds = do
  n <- choose (0, 5)
  prefix <- vectorOf n (elements [CmdToolOk, CmdToolFail])
  pure (prefix ++ [CmdDone])

-- Each command maps to exactly one LLM response.  The sequence must end
-- with CmdDone so the agent always receives a terminal response.
responseFor :: AgentCmd -> CompletionResponse
responseFor CmdDone     = mkDoneResponse
responseFor CmdToolOk   = mkToolUseResponse "toolu_ok" "sm_ok_tool"
responseFor CmdToolFail = mkToolUseResponse "toolu_fail" "sm_fail_tool"

mkStateMachineEnv :: [AgentCmd] -> IO AgentEnv
mkStateMachineEnv cmds = do
  let responses = map responseFor cmds
      registry =
        register (okHandler' "sm_ok_tool") $
          register failingHandler' emptyRegistry
  mkTestEnv registry responses [mkSummaryResponse] defaultCompCfg
 where
  okHandler' nm =
    ToolHandlerDict
      { definition =
          ToolDefinition
            { name = ToolName nm
            , description = "state machine ok handler"
            , parameters = objectSchema []
            , riskTier = ReadOnly
            }
      , invoke = \_ -> pure (Right ToolOutput{content = "ok", truncated = False})
      }
  failingHandler' =
    ToolHandlerDict
      { definition =
          ToolDefinition
            { name = ToolName "sm_fail_tool"
            , description = "state machine failing handler"
            , parameters = objectSchema []
            , riskTier = ReadOnly
            }
      , invoke = \_ -> pure (Left (InvalidInput "injected failure"))
      }

isTerminalSignal :: ControlSignal -> Bool
isTerminalSignal Continue    = False
isTerminalSignal (Done _)    = True
isTerminalSignal (Blocked _) = True
isTerminalSignal (NeedsInput _) = True

toolResultCount :: Seq.Seq Message -> Int
toolResultCount history =
  length
    [ ()
    | msg <- Foldable.toList history
    , ToolResultContent _ <- msg.content
    ]

errorToolResultCount :: Seq.Seq Message -> Int
errorToolResultCount history =
  length
    [ ()
    | msg <- Foldable.toList history
    , ToolResultContent tr <- msg.content
    , tr.isError
    ]

stateMachineProps :: Spec
stateMachineProps = describe "state machine properties" $ do

  prop "always produces a terminal signal for any command sequence" $
    forAll arbitraryCmds $ \cmds ->
      checkCoverage $
        cover 50 (any (/= CmdDone) cmds) "contains tool calls" $
        monadicIO $ do
          env <- run $ mkStateMachineEnv cmds
          (sig, _) <- run $ runAgentTurn env "sys" (sessionFromEnv env) "task"
          assert (isTerminalSignal sig)

  -- The agent may abort early after consecutive failures, so we cannot
  -- assert all N tool calls execute.  We assert that at least one
  -- executes whenever tool commands appear in the sequence.
  prop "tool calls are recorded in history when present" $
    forAll arbitraryCmds $ \cmds ->
      let hasTools = any (/= CmdDone) cmds
       in checkCoverage $
            cover 50 hasTools "has tool calls" $
            monadicIO $ do
              env <- run $ mkStateMachineEnv cmds
              (_, session) <- run $ runAgentTurn env "sys" (sessionFromEnv env) "task"
              if hasTools
                then assert (toolResultCount session.ssHistory > 0)
                else assert (toolResultCount session.ssHistory == 0)

  prop "failing tool calls produce error results in history" $
    forAll arbitraryCmds $ \cmds ->
      let hasFails = any (== CmdToolFail) cmds
       in checkCoverage $
            cover 30 hasFails "has failing tool calls" $
            monadicIO $ do
              env <- run $ mkStateMachineEnv cmds
              (_, session) <- run $ runAgentTurn env "sys" (sessionFromEnv env) "task"
              if hasFails
                then assert (errorToolResultCount session.ssHistory > 0)
                else pure ()

  -- ----------------------------------------------------------------
  -- Compaction-interleaving properties
  --
  -- The seed history starts above the aggressiveCompCfg threshold so
  -- compaction fires at the beginning of each turn, interleaving with
  -- whatever tool-call sequence the generator produces.  The existing
  -- state machine properties test tool success/failure; these three
  -- test the same sequences under the additional constraint that
  -- compaction must fire and invariants must still hold.
  -- ----------------------------------------------------------------

  prop "terminates and compaction fires under aggressive context limit" $
    forAll arbitraryCmds $ \cmds ->
      checkCoverage $
        cover 50 (any (/= CmdDone) cmds) "contains tool calls" $
        monadicIO $ do
          eventsRef <- run $ newIORef ([] :: [AgentEvent])
          env0 <- run $ mkStateMachineEnv cmds
          let env = env0
                { envCompaction = aggressiveCompCfg
                , envOnEvent    = \ev -> modifyIORef' eventsRef (++ [ev])
                }
              sess = (sessionFromEnv env){ssHistory = compactionSeedHistory}
          (sig, _) <- run $ runAgentTurn env "sys" sess "task"
          evs     <- run $ readIORef eventsRef
          assert (isTerminalSignal sig)
          assert (any isCompacting evs)

  prop "history length stays bounded after compaction" $
    forAll arbitraryCmds $ \cmds ->
      monadicIO $ do
        env0 <- run $ mkStateMachineEnv cmds
        let env     = env0{envCompaction = aggressiveCompCfg}
            seedLen = Seq.length compactionSeedHistory
            sess    = (sessionFromEnv env){ssHistory = compactionSeedHistory}
        (_, finalSession) <- run $ runAgentTurn env "sys" sess "task"
        let finalLen = Seq.length finalSession.ssHistory
        monitor $ counterexample
          ("seed length=" <> show seedLen <> " final length=" <> show finalLen)
        assert (finalLen < seedLen + 2)

  prop "failing tool calls don't prevent termination under compaction" $
    forAll (arbitraryCmds `suchThat` any (== CmdToolFail)) $ \cmds ->
      monadicIO $ do
        eventsRef <- run $ newIORef ([] :: [AgentEvent])
        env0 <- run $ mkStateMachineEnv cmds
        let env = env0
              { envCompaction = aggressiveCompCfg
              , envOnEvent    = \ev -> modifyIORef' eventsRef (++ [ev])
              }
            sess = (sessionFromEnv env){ssHistory = compactionSeedHistory}
        (sig, _) <- run $ runAgentTurn env "sys" sess "task"
        evs     <- run $ readIORef eventsRef
        assert (isTerminalSignal sig)
        assert (any isCompacting evs)

-- Compaction config whose threshold (0.15 × 120 = 18 chars) the seed
-- history easily exceeds, triggering compaction on every turn.
aggressiveCompCfg :: CompactionConfig
aggressiveCompCfg = CompactionConfig{triggerFraction = 0.15, maxContextTokens = 120}

-- Seed history identical to the one in the existing compaction example test:
-- four messages of 80 chars each ≈ 320 chars >> 18 char threshold.
compactionSeedHistory :: Seq.Seq Message
compactionSeedHistory = Seq.fromList
  [ Message User      [TextContent (Text.replicate 80 "u")]
  , Message Assistant [TextContent (Text.replicate 80 "a")]
  , Message User      [TextContent (Text.replicate 80 "u2")]
  , Message Assistant [TextContent (Text.replicate 80 "a2")]
  ]

-- --------------------------------------------------------------------
-- Existing model state machinery
-- --------------------------------------------------------------------

data ModelState = Idle | Tooling | Completed
  deriving stock (Eq, Show)

runOnce :: AgentEnv -> Text -> IO ControlSignal
runOnce env task = fst <$> runAgentTurn env "sys" (sessionFromEnv env) task

evolveModel :: ModelState -> IO ControlSignal -> IO ModelState
evolveModel st ioSig = do
  sig <- ioSig
  pure $ case (st, sig) of
    (Idle, Continue) -> Tooling
    (Idle, Done{}) -> Completed
    (Tooling, Done{}) -> Completed
    (Tooling, Continue) -> Tooling
    (_, Blocked{}) -> Completed
    (_, NeedsInput{}) -> Tooling
    (Completed, _) -> Completed

mkTestEnv ::
  ToolRegistry ->
  [CompletionResponse] ->
  [CompletionResponse] ->
  CompactionConfig ->
  IO AgentEnv
mkTestEnv registry coderResponses summarizerResponses compCfg = do
  coderRef <- newIORef coderResponses
  sumRef <- newIORef summarizerResponses
  let coderClient = scriptedClient coderRef
      summClient = scriptedClient sumRef
      resolver role = case role of
        Summarizer -> summClient
        _ -> coderClient
  pure
    AgentEnv
      { envLlm = resolver
      , envTools = registry
      , envConfig = defaultConfig
      , envTelemetry = noOpRecorder
      , envOnEvent = \_ -> pure ()
      , envGuardrails = defaultGuardrailConfigShim
      , envPermissions = Nothing
      , envCompaction = compCfg
      , envCompState = emptyCompactionState{csTask = "test"}
      , envCostTracker = Nothing
      , envSessionId = SessionId "test-session"
      , envUserId = UserId "test-user"
      , envGrammarReg = GrammarRegistry Map.empty
      , envMemoryStore = Nothing
      , envWaitForInput = Nothing
      , envWaitForApproval = Nothing
      }

scriptedClient :: IORef [CompletionResponse] -> LlmClientDict
scriptedClient ref =
  LlmClientDict
    { clientInfo = ClientInfo "test" "test-model"
    , complete = \_ -> Right <$> popResponse ref
    , stream = \_ onEvent -> do
        resp <- popResponse ref
        mapM_ (emitToken onEvent) resp.responseContent
        pure (Right resp)
    , countTokens = \_ -> pure (Right TokenCount{inputTokens = 0, outputTokens = 0})
    }

emitToken :: (CompletionEvent -> IO ()) -> Content -> IO ()
emitToken onEvent (TextContent t) = onEvent (EventToken t)
emitToken _ _ = pure ()

popResponse :: IORef [CompletionResponse] -> IO CompletionResponse
popResponse ref = do
  xs <- readIORef ref
  case xs of
    (x : rest) -> writeIORef ref rest >> pure x
    [] -> pure mkDoneResponse

mkDoneResponse :: CompletionResponse
mkDoneResponse =
  CompletionResponse
    { responseContent = [TextContent "done"]
    , stopReason = EndTurn
    , usage = TokenCount 1 1
    }

mkSummaryResponse :: CompletionResponse
mkSummaryResponse =
  CompletionResponse
    { responseContent = [TextContent "summary"]
    , stopReason = EndTurn
    , usage = TokenCount 1 1
    }

mkToolUseResponse :: Text -> Text -> CompletionResponse
mkToolUseResponse callId tool =
  CompletionResponse
    { responseContent =
        [ ToolUseContent
            ToolCall
              { toolCallId = ToolCallId callId
              , toolName = ToolName tool
              , arguments = Object (KM.fromList [(Key.fromText "path", String "file.txt")])
              }
        ]
    , stopReason = ToolUse
    , usage = TokenCount 1 1
    }

okHandler :: IORef [Text] -> Text -> ToolHandlerDict
okHandler callsRef nm =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName nm
          , description = "ok handler"
          , parameters = objectSchema []
          , riskTier = ReadOnly
          }
    , invoke = \_ -> do
        modifyIORef' callsRef (++ [nm])
        pure (Right ToolOutput{content = "ok", truncated = False})
    }

countingHandler :: IORef Int -> Text -> ToolHandlerDict
countingHandler ref nm =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName nm
          , description = "counter"
          , parameters = objectSchema []
          , riskTier = ReadOnly
          }
    , invoke = \_ -> do
        modifyIORef' ref (+ 1)
        pure (Right ToolOutput{content = "ok", truncated = False})
    }

failingHandler :: ToolHandlerDict
failingHandler =
  ToolHandlerDict
    { definition =
        ToolDefinition
          { name = ToolName "failing_tool"
          , description = "always fails"
          , parameters = objectSchema []
          , riskTier = ReadOnly
          }
    , invoke = \_ -> pure (Left (InvalidInput "bad input"))
    }

isDoneSignal :: ControlSignal -> Bool
isDoneSignal (Done _) = True
isDoneSignal _ = False

isCompacting :: AgentEvent -> Bool
isCompacting AgentCompacting = True
isCompacting _ = False

hasErrorToolResult :: Seq.Seq Message -> Bool
hasErrorToolResult history =
  any isErr [tr | msg <- toList history, ToolResultContent tr <- msg.content]
 where
  toList = Foldable.toList
  isErr tr = tr.isError

defaultCompCfg :: CompactionConfig
defaultCompCfg = CompactionConfig{triggerFraction = 0.95, maxContextTokens = 20_000}

-- Local shim avoids importing full Guardrails defaults into this isolated harness.
defaultGuardrailConfigShim :: GR.GuardrailConfig
defaultGuardrailConfigShim = GR.defaultGuardrailConfig
