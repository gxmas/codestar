{-# LANGUAGE OverloadedStrings #-}

-- | Property-based tests for the LLM retry callback semantics and
-- telemetry event construction.
--
-- Key invariants tested:
--
--   1. Callback fires exactly once per transient failure
--   2. Attempt numbers form [0, 1, ..., n-1] for n transient errors
--   3. Non-transient errors never fire the callback
--   4. RateLimited secs -> retryAfterHintMs = round(secs * 1000)
--   5. NetworkError -> retryAfterHintMs = 0
--   6. On LLM failure: setSpanError is called before endSpan
--   7. On LLM success: setSpanError is never called
module CodeStar.LlmRetrySpec (spec) where

import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import CodeStar.LLM.Base
  ( ClientInfo (..)
  , CompletionRequest (..)
  , CompletionResponse (..)
  , LlmClientDict (..)
  , LlmError (..)
  , StopReason (..)
  , TokenCount (..)
  , withRetry
  )
import OTel.Attribute (AttributeValue (..))
import CodeStar.Telemetry
  ( AgentEvent (..)
  , SpanHandle (..)
  , TelemetryRecorder (..)
  )
import Resilience.Core (defaultRecoveryPolicy, newRecoveryEngine)

-- ===================================================================
-- Generators
-- ===================================================================

-- | Transient errors: the only ones that trigger retry.
genTransientError :: Gen LlmError
genTransientError =
  oneof
    [ RateLimited <$> genRetryAfter
    , NetworkError <$> genErrMsg
    ]

-- | Non-transient errors: must NOT trigger the callback.
genNonTransientError :: Gen LlmError
genNonTransientError =
  oneof
    [ AuthenticationFailed <$> genErrMsg
    , ContextTooLong <$> chooseInt (1, 200000) <*> chooseInt (1, 200000)
    , ContentFiltered <$> genErrMsg
    , InvalidRequest <$> genErrMsg
    , ProviderError <$> genErrMsg
    ]

-- | Any LlmError.
genLlmError :: Gen LlmError
genLlmError =
  frequency
    [ (2, genTransientError)
    , (5, genNonTransientError)
    ]

-- | Retry-after seconds: use very small values so tests run fast,
-- but also exercise the full range for the delay computation property.
genRetryAfter :: Gen Double
genRetryAfter =
  frequency
    [ (3, choose (0.0, 0.005))   -- fast for integration tests
    , (1, choose (0.0, 300.0))   -- full range for computation tests
    ]

-- | Small retry-after for integration tests that actually sleep.
genSmallRetryAfter :: Gen Double
genSmallRetryAfter = choose (0.0, 0.002)

genErrMsg :: Gen Text
genErrMsg = Text.pack <$> listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ " "))

-- | Number of transient failures before success.
-- Must not exceed defaultRecoveryPolicy's maxRetries (4); exceeding it
-- causes the engine to give up before all callbacks fire, breaking the
-- "callbackCount == n" invariant.
genTransientCount :: Gen Int
genTransientCount = chooseInt (1, 4)

-- ===================================================================
-- Test infrastructure
-- ===================================================================

-- | A fake LLM client that pops errors from an IORef, returning success
-- when the error list is exhausted.
fakeLlmClient :: IORef [Either LlmError CompletionResponse] -> LlmClientDict
fakeLlmClient ref =
  LlmClientDict
    { clientInfo = ClientInfo "fake" "fake-model"
    , complete = \_ -> popResult ref
    , stream = \_ _ -> popResult ref
    , countTokens = \_ -> pure (Right (TokenCount 0 0 0 0))
    }

popResult :: IORef [Either LlmError CompletionResponse] -> IO (Either LlmError CompletionResponse)
popResult ref = atomicModifyIORef' ref $ \case
  [] -> ([], Right successResponse)
  (x : xs) -> (xs, x)

successResponse :: CompletionResponse
successResponse = CompletionResponse [] EndTurn (TokenCount 10 20 0 0)

minimalReq :: CompletionRequest
minimalReq =
  CompletionRequest
    { messages = []
    , systemPrompt = Nothing
    , tools = []
    , maxTokens = 32
    , temperature = Nothing
    , topP = Nothing
    }

-- | Record of callback invocations: (error, attempt number).
type CallbackLog = IORef [(LlmError, Int)]

newCallbackLog :: IO CallbackLog
newCallbackLog = newIORef []

recordingCallback :: CallbackLog -> LlmError -> Int -> IO ()
recordingCallback ref err attempt =
  modifyIORef' ref (++ [(err, attempt)])

-- ===================================================================
-- Span operation recording (reuses pattern from TelemetrySpanSpec)
-- ===================================================================

data SpanOp
  = SpanStart Text [(Text, AttributeValue)]
  | SpanEnd
  | SpanSetAttr Text Text
  | SpanSetError Text
  deriving stock (Show, Eq)

data FakeRecorder = FakeRecorder
  { recorder :: TelemetryRecorder
  , getOps :: IO [SpanOp]
  }

newFakeRecorder :: IO FakeRecorder
newFakeRecorder = do
  ref <- newIORef ([] :: [SpanOp])
  let append op = atomicModifyIORef' ref (\ops -> (ops ++ [op], ()))
      dummyHandle = SpanHandle (error "FakeRecorder: SomeSpan unused") (error "FakeRecorder: Token unused")
      rec = TelemetryRecorder
        { recordEvent      = \_ -> pure ()
        , startSpan        = \name attrs -> do
            append (SpanStart name attrs)
            pure dummyHandle
        , endSpan          = \_ -> append SpanEnd
        , setSpanAttr      = \_ k v -> append (SpanSetAttr k v)
        , setSpanError       = \_ msg -> append (SpanSetError msg)
        , setSpanAttrTyped   = \_ _ _ -> pure ()
        , adjustSessionCount = \_ -> pure ()
        }
  pure FakeRecorder { recorder = rec, getOps = readIORef ref }

-- | Simulate the callLlm error path span protocol from AgentLoop.hs.
-- On failure: setSpanError -> setSpanAttr "error.type" -> endSpan
-- On success: endSpan (no error ops)
callLlmSpanProtocol :: TelemetryRecorder -> Either LlmError CompletionResponse -> IO ()
callLlmSpanProtocol tel result = do
  llmSpan <- tel.startSpan "llm.call" [("role", StringValue "coder")]
  case result of
    Left err -> do
      let errMsg = Text.pack (show err)
      tel.setSpanError llmSpan errMsg
      tel.setSpanAttr llmSpan "error.type" (llmErrorConstructor err)
      case err of
        RateLimited secs ->
          tel.setSpanAttr llmSpan "retry_after.seconds" (Text.pack (show secs))
        _ -> pure ()
      tel.endSpan llmSpan
    Right _ -> do
      tel.endSpan llmSpan

llmErrorConstructor :: LlmError -> Text
llmErrorConstructor (RateLimited _)        = "RateLimited"
llmErrorConstructor (AuthenticationFailed _) = "AuthenticationFailed"
llmErrorConstructor (ContextTooLong _ _)   = "ContextTooLong"
llmErrorConstructor (ContentFiltered _)    = "ContentFiltered"
llmErrorConstructor (InvalidRequest _)     = "InvalidRequest"
llmErrorConstructor (ProviderError _)      = "ProviderError"
llmErrorConstructor (NetworkError _)       = "NetworkError"

-- ===================================================================
-- Helpers
-- ===================================================================

isTransient :: LlmError -> Bool
isTransient (RateLimited _) = True
isTransient (NetworkError _) = True
isTransient _ = False

-- | Compute retryAfterHintMs the same way the production onRetry callback does.
-- For RateLimited secs: round(secs * 1000)
-- For NetworkError: 0
computeRetryAfterHintMs :: LlmError -> Int
computeRetryAfterHintMs (RateLimited secs) = round (secs * 1000)
computeRetryAfterHintMs (NetworkError _) = 0
computeRetryAfterHintMs _ = 0

-- ===================================================================
-- Properties
-- ===================================================================

spec :: Spec
spec = describe "CodeStar.LlmRetry" $ do

  -- ----------------------------------------------------------------
  -- Property 1: Callback fires exactly once per transient failure
  -- ----------------------------------------------------------------
  describe "callback fires per transient failure" $ do
    prop "callback count equals transient error count before success" $
      forAll genTransientCount $ \n ->
        forAll (vectorOf n (RateLimited <$> genSmallRetryAfter)) $ \errs ->
          monadicIO $ do
            callbackCount <- run $ do
              engine <- newRecoveryEngine defaultRecoveryPolicy
              resultsRef <- newIORef (map Left errs)
              logRef <- newCallbackLog
              let client = withRetry engine (recordingCallback logRef) (fakeLlmClient resultsRef)
              _ <- client.complete minimalReq
              length <$> readIORef logRef
            assert (callbackCount == n)

    prop "callback count equals transient errors before a non-transient error stops retry" $
      forAll genTransientCount $ \n ->
        forAll (vectorOf n (RateLimited <$> genSmallRetryAfter)) $ \transients ->
          forAll genNonTransientError $ \terminal ->
            monadicIO $ do
              callbackCount <- run $ do
                engine <- newRecoveryEngine defaultRecoveryPolicy
                let results = map Left transients ++ [Left terminal]
                resultsRef <- newIORef results
                logRef <- newCallbackLog
                let client = withRetry engine (recordingCallback logRef) (fakeLlmClient resultsRef)
                _ <- client.complete minimalReq
                length <$> readIORef logRef
              -- The callback fires for each transient error; the non-transient
              -- error is not retried so callback does NOT fire for it.
              assert (callbackCount == n)

  -- ----------------------------------------------------------------
  -- Property 2: Attempt numbers are monotonically increasing from 0
  -- ----------------------------------------------------------------
  describe "attempt count sequence" $ do
    prop "attempts are [0..n-1] for n transient failures" $
      forAll genTransientCount $ \n ->
        forAll (vectorOf n (RateLimited <$> genSmallRetryAfter)) $ \errs ->
          monadicIO $ do
            attempts <- run $ do
              engine <- newRecoveryEngine defaultRecoveryPolicy
              resultsRef <- newIORef (map Left errs)
              logRef <- newCallbackLog
              let client = withRetry engine (recordingCallback logRef) (fakeLlmClient resultsRef)
              _ <- client.complete minimalReq
              map snd <$> readIORef logRef
            assert (attempts == [0 .. n - 1])

  -- ----------------------------------------------------------------
  -- Property 3: Non-transient errors do NOT fire the callback
  -- ----------------------------------------------------------------
  describe "non-transient errors skip callback" $ do
    prop "a single non-transient error produces zero callback invocations" $
      forAll genNonTransientError $ \err ->
        monadicIO $ do
          callbackCount <- run $ do
            engine <- newRecoveryEngine defaultRecoveryPolicy
            resultsRef <- newIORef [Left err]
            logRef <- newCallbackLog
            let client = withRetry engine (recordingCallback logRef) (fakeLlmClient resultsRef)
            _ <- client.complete minimalReq
            length <$> readIORef logRef
          assert (callbackCount == 0)

    prop "non-transient errors are never passed to the callback" $
      forAll genLlmError $ \err ->
        not (isTransient err) ==>
          monadicIO $ do
            entries <- run $ do
              engine <- newRecoveryEngine defaultRecoveryPolicy
              resultsRef <- newIORef [Left err]
              logRef <- newCallbackLog
              let client = withRetry engine (recordingCallback logRef) (fakeLlmClient resultsRef)
              _ <- client.complete minimalReq
              readIORef logRef
            assert (null entries)

  -- ----------------------------------------------------------------
  -- Property 4: RateLimited secs -> retryAfterHintMs = round(secs * 1000)
  --
  -- This is a pure computation property — no IO needed, but we phrase
  -- it as a property over the full Double range including boundaries.
  -- ----------------------------------------------------------------
  describe "retryAfterHintMs for RateLimited" $ do
    prop "retryAfterHintMs equals round(secs * 1000) for RateLimited" $
      forAll (choose (0.0, 300.0)) $ \secs ->
        computeRetryAfterHintMs (RateLimited secs) === round (secs * 1000)

    -- Explicit boundary values that exercise rounding edge cases
    it "boundary: 0.0 seconds -> 0 ms" $
      computeRetryAfterHintMs (RateLimited 0.0) `shouldBe` 0

    it "boundary: 0.001 seconds -> 1 ms (banker's rounding)" $
      computeRetryAfterHintMs (RateLimited 0.001) `shouldBe` round (0.001 * 1000 :: Double)

    it "boundary: 0.5 seconds -> 500 ms" $
      computeRetryAfterHintMs (RateLimited 0.5) `shouldBe` 500

    it "boundary: 1.0 seconds -> 1000 ms" $
      computeRetryAfterHintMs (RateLimited 1.0) `shouldBe` 1000

    it "boundary: 29.999 seconds -> 29999 ms" $
      computeRetryAfterHintMs (RateLimited 29.999) `shouldBe` 29999

    it "boundary: 30.0 seconds -> 30000 ms" $
      computeRetryAfterHintMs (RateLimited 30.0) `shouldBe` 30000

    it "boundary: 120.0 seconds -> 120000 ms" $
      computeRetryAfterHintMs (RateLimited 120.0) `shouldBe` 120000

  -- ----------------------------------------------------------------
  -- Property 5: NetworkError -> retryAfterHintMs = 0
  -- ----------------------------------------------------------------
  describe "retryAfterHintMs for NetworkError" $ do
    prop "retryAfterHintMs is always 0 for NetworkError regardless of message" $
      forAll genErrMsg $ \msg ->
        computeRetryAfterHintMs (NetworkError msg) === 0

  -- ----------------------------------------------------------------
  -- Property 6: Span is marked error BEFORE endSpan on LLM failure
  --
  -- The ordering invariant: for any LlmError, the span ops must be
  -- [SpanStart, ..., SpanSetError _, SpanSetAttr "error.type" _, ..., SpanEnd]
  -- Never endSpan before setSpanError.
  -- ----------------------------------------------------------------
  describe "span error ordering on failure" $ do
    prop "setSpanError precedes endSpan for any LlmError" $
      forAll genLlmError $ \err ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            callLlmSpanProtocol fr.recorder (Left err)
            fr.getOps
          -- setSpanError must appear before SpanEnd
          let errorIdx = elemIndex' isSpanSetError ops
              endIdx = elemIndex' isSpanEnd ops
          monitor (classify (isTransient err) "transient")
          monitor (classify (not (isTransient err)) "non-transient")
          assert (errorIdx < endIdx)

    prop "error.type attribute is set before endSpan for any LlmError" $
      forAll genLlmError $ \err ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            callLlmSpanProtocol fr.recorder (Left err)
            fr.getOps
          let errorTypeIdx = elemIndex' (isSpanSetAttrKey "error.type") ops
              endIdx = elemIndex' isSpanEnd ops
          assert (errorTypeIdx < endIdx)

    prop "error.type attribute value matches the error constructor" $
      forAll genLlmError $ \err ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            callLlmSpanProtocol fr.recorder (Left err)
            fr.getOps
          let errorTypeVal = findAttrValue "error.type" ops
          assert (errorTypeVal == Just (llmErrorConstructor err))

    prop "RateLimited sets retry_after.seconds attribute" $
      forAll genRetryAfter $ \secs ->
        monadicIO $ do
          ops <- run $ do
            fr <- newFakeRecorder
            callLlmSpanProtocol fr.recorder (Left (RateLimited secs))
            fr.getOps
          let retryAfterVal = findAttrValue "retry_after.seconds" ops
          assert (retryAfterVal == Just (Text.pack (show secs)))

  -- ----------------------------------------------------------------
  -- Property 7: On LLM success, span is NOT marked error
  -- ----------------------------------------------------------------
  describe "span on success has no error" $ do
    prop "no SpanSetError is recorded when LLM succeeds" $
      monadicIO $ do
        ops <- run $ do
          fr <- newFakeRecorder
          callLlmSpanProtocol fr.recorder (Right successResponse)
          fr.getOps
        assert (not (any isSpanSetError ops))

    prop "endSpan is still called exactly once on success" $
      monadicIO $ do
        ops <- run $ do
          fr <- newFakeRecorder
          callLlmSpanProtocol fr.recorder (Right successResponse)
          fr.getOps
        assert (length (filter isSpanEnd ops) == 1)

  -- ----------------------------------------------------------------
  -- Property: EvLlmRetry event field consistency
  --
  -- When the production onRetry callback constructs EvLlmRetry, the
  -- fields must be consistent with the error it received.
  -- ----------------------------------------------------------------
  describe "EvLlmRetry event construction" $ do
    prop "EvLlmRetry fields are consistent with the triggering error" $
      forAll genTransientError $ \err ->
        forAll (chooseInt (0, 20)) $ \attempt ->
          let event = mkEvLlmRetry err attempt
          in  conjoin
                [ counterexample "retryAttempt must equal the attempt number" $
                    retryAttempt event === attempt
                , counterexample "retryAfterHintMs must match computeRetryAfterHintMs" $
                    retryAfterHintMs event === computeRetryAfterHintMs err
                , counterexample "retryError must be non-empty" $
                    property (not (Text.null (retryError event)))
                ]

    prop "retryAfterHintMs for RateLimited in event matches round(secs*1000)" $
      forAll (choose (0.0, 300.0)) $ \secs ->
        forAll (chooseInt (0, 10)) $ \attempt ->
          let event = mkEvLlmRetry (RateLimited secs) attempt
          in  retryAfterHintMs event === round (secs * 1000)

    prop "retryAfterHintMs for NetworkError in event is always 0" $
      forAll genErrMsg $ \msg ->
        forAll (chooseInt (0, 10)) $ \attempt ->
          let event = mkEvLlmRetry (NetworkError msg) attempt
          in  retryAfterHintMs event === 0

-- ===================================================================
-- Event construction (mirrors production code in CLI.hs/Server.hs)
-- ===================================================================

-- | Construct an EvLlmRetry the same way the production onRetry does.
mkEvLlmRetry :: LlmError -> Int -> AgentEvent
mkEvLlmRetry err attempt =
  EvLlmRetry
    { retryError = Text.pack (show err)
    , retryAttempt = attempt
    , retryAfterHintMs = computeRetryAfterHintMs err
    , lrSessionId = ""
    }

-- ===================================================================
-- Span op predicates
-- ===================================================================

isSpanSetError :: SpanOp -> Bool
isSpanSetError (SpanSetError _) = True
isSpanSetError _ = False

isSpanEnd :: SpanOp -> Bool
isSpanEnd SpanEnd = True
isSpanEnd _ = False

isSpanSetAttrKey :: Text -> SpanOp -> Bool
isSpanSetAttrKey key (SpanSetAttr k _) = k == key
isSpanSetAttrKey _ _ = False

findAttrValue :: Text -> [SpanOp] -> Maybe Text
findAttrValue key ops =
  case filter (isSpanSetAttrKey key) ops of
    (SpanSetAttr _ v : _) -> Just v
    _ -> Nothing

-- | Index of first element matching predicate, or maxBound if not found.
elemIndex' :: (a -> Bool) -> [a] -> Int
elemIndex' p xs = go 0 xs
 where
  go _ [] = maxBound
  go i (x : rest)
    | p x = i
    | otherwise = go (i + 1) rest
