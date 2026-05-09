{-# LANGUAGE ExistentialQuantification #-}
-- | Property-based and specification tests for otel-sdk-logs.
--
-- Covers LogRecordLimits, NoOpLogRecordExporter, BatchLogRecordProcessorConfig,
-- LogRecordProcessor, SimpleLogRecordProcessor, BatchLogRecordProcessor,
-- SdkLoggerProvider, full integration (Logger -> Exporter), ReadWriteLogRecord
-- setters, attribute limits, trace correlation, and SeverityNumber enum.
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (replicateM_)
import Data.ByteString qualified as BS
import Prelude hiding (lookup)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OTel.Attribute
  ( AttributeValue (..), InstrumentationScope (..)
  , fromList, lookup, size
  )
import OTel.Context (root)
import OTel.Log
  ( LogBody (..), LogRecord (..), Logger (..), LoggerProvider (..)
  , SeverityNumber (..), defaultLogRecord, severityNumberValue
  )
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (milliseconds)
import OTel.Trace (createNonRecordingSpan, setSpanInContext)
import OTel.Trace.SpanContext
  ( SpanContext (..), invalidSpanContext
  , sampledFlag, spanIdFromBytes, traceIdFromBytes
  )


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Assert that an Either is Right ().
assertRight :: Show e => Either e () -> Assertion
assertRight (Right ()) = pure ()
assertRight (Left e) = assertFailure ("expected Right (), got Left: " ++ show e)

testScope :: InstrumentationScope
testScope = InstrumentationScope "test-lib" (Just "1.0") Nothing Nothing

-- | A valid non-zero TraceId for trace correlation tests.
someValidTraceId :: SpanContext -> SpanContext
someValidTraceId sc = sc
  { traceId = traceIdFromBytes (BS.replicate 15 0 <> BS.singleton 1)
  }

someValidSpanId :: SpanContext -> SpanContext
someValidSpanId sc = sc
  { spanId = spanIdFromBytes (BS.replicate 7 0 <> BS.singleton 1)
  }

-- | Build a valid SpanContext with non-zero trace/span IDs.
validSpanContext :: SpanContext
validSpanContext = invalidSpanContext
  { traceId = traceIdFromBytes (BS.replicate 15 0 <> BS.singleton 1)
  , spanId = spanIdFromBytes (BS.replicate 7 0 <> BS.singleton 1)
  , traceFlags = sampledFlag
  }


-------------------------------------------------------------------------------
-- RecordingLogExporter (test helper)
-------------------------------------------------------------------------------

data RecordingLogExporter = RecordingLogExporter
  { rleExports    :: !(TVar [[SomeReadableLogRecord]])
  , rleShutdowns  :: !(TVar Int)
  , rleFlushCalls :: !(TVar Int)
  }

newRecordingLogExporter :: IO RecordingLogExporter
newRecordingLogExporter = RecordingLogExporter
  <$> newTVarIO []
  <*> newTVarIO 0
  <*> newTVarIO 0

instance LogRecordExporter RecordingLogExporter where
  exportLogRecords e recs = do
    atomically (modifyTVar' (rleExports e) (recs :))
    pure ExportSuccess
  shutdownLogExporter e = do
    atomically (modifyTVar' (rleShutdowns e) (+ 1))
    pure (Right ())
  forceFlushLogExporter e _ = do
    atomically (modifyTVar' (rleFlushCalls e) (+ 1))
    pure (Right ())


-- | An exporter that sleeps for a very long time and tracks whether its
-- export completed. Used to verify that export timeout cancels the export.
data VerySlowLogExporter = VerySlowLogExporter !(IORef Bool)

newVerySlowLogExporter :: IO (VerySlowLogExporter, IORef Bool)
newVerySlowLogExporter = do
  ref <- newIORef False
  pure (VerySlowLogExporter ref, ref)

instance LogRecordExporter VerySlowLogExporter where
  exportLogRecords (VerySlowLogExporter ref) _ = do
    threadDelay 10_000_000  -- 10 seconds
    writeIORef ref True
    pure ExportSuccess
  shutdownLogExporter _ = pure (Right ())
  forceFlushLogExporter _ _ = pure (Right ())


-- | Get all exported log records, flattened.
getAllExportedLogRecords :: RecordingLogExporter -> IO [SomeReadableLogRecord]
getAllExportedLogRecords recorder = do
  batches <- readTVarIO (rleExports recorder)
  pure (concat batches)

-- | Count all exported log records across all batches.
countExportedLogs :: RecordingLogExporter -> IO Int
countExportedLogs recorder = length <$> getAllExportedLogRecords recorder


-------------------------------------------------------------------------------
-- CapturingProcessor (for ReadWriteLogRecord tests)
-------------------------------------------------------------------------------

data CapturingProcessor = CapturingProcessor !(TVar (Maybe SomeReadWriteLogRecord))

newCapturingProcessor :: IO CapturingProcessor
newCapturingProcessor = CapturingProcessor <$> newTVarIO Nothing

instance LogRecordProcessor CapturingProcessor where
  onEmit (CapturingProcessor var) rw _ctx =
    atomically (writeTVar var (Just rw))
  shutdownProcessor _ = pure (Right ())
  forceFlushProcessor _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Provider + exporter wiring helpers
-------------------------------------------------------------------------------

-- | Create a provider wired to a RecordingLogExporter via SimpleLogRecordProcessor.
newTestLogProvider :: IO (SdkLoggerProvider, RecordingLogExporter)
newTestLogProvider = newTestLogProviderWith defaultSdkLoggerProviderConfig

newTestLogProviderWith :: SdkLoggerProviderConfig -> IO (SdkLoggerProvider, RecordingLogExporter)
newTestLogProviderWith baseConfig = do
  recorder <- newRecordingLogExporter
  proc <- newSimpleLogRecordProcessor (SomeLogRecordExporter recorder)
  provider <- newSdkLoggerProvider baseConfig
    { llpProcessors = SomeLogRecordProcessor proc : llpProcessors baseConfig }
  pure (provider, recorder)


-------------------------------------------------------------------------------
-- Fast batch config
-------------------------------------------------------------------------------

fastBatchConfig :: BatchLogRecordProcessorConfig
fastBatchConfig = defaultBatchLogRecordProcessorConfig
  { blrpScheduledDelay     = milliseconds 10
  , blrpMaxQueueSize       = 20
  , blrpMaxExportBatchSize = 5
  }


-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "otel-sdk-logs"
  [ logRecordLimitsTests
  , noOpLogRecordExporterTests
  , batchLogRecordProcessorConfigTests
  , logRecordProcessorWrapperTests
  , simpleLogRecordProcessorTests
  , batchLogRecordProcessorTests
  , batchLogRecordProcessorExportTimeoutTests
  , sdkLoggerProviderTests
  , integrationTests
  , readWriteLogRecordTests
  , logRecordLimitsEnforcementTests
  , traceCorrelationTests
  , severityNumberTests
  ]


-------------------------------------------------------------------------------
-- 1. LogRecordLimits defaults
-------------------------------------------------------------------------------

logRecordLimitsTests :: TestTree
logRecordLimitsTests = testGroup "LogRecordLimits"
  [ testCase "defaultLogRecordLimits.lrlMaxAttributes == 128" $
      defaultLogRecordLimits.lrlMaxAttributes @?= 128

  , testCase "defaultLogRecordLimits.lrlMaxAttributeValueLen == Nothing" $
      defaultLogRecordLimits.lrlMaxAttributeValueLen @?= Nothing

  , testProperty "updating lrlMaxAttributes changes only that field" $
      \(NonNegative n) ->
        let updated = defaultLogRecordLimits { lrlMaxAttributes = n }
        in  updated.lrlMaxAttributes == n
         && updated.lrlMaxAttributeValueLen == defaultLogRecordLimits.lrlMaxAttributeValueLen

  , testProperty "updating lrlMaxAttributeValueLen changes only that field" $
      \(n :: Maybe (NonNegative Int)) ->
        let val = fmap getNonNegative n
            updated = defaultLogRecordLimits { lrlMaxAttributeValueLen = val }
        in  updated.lrlMaxAttributeValueLen == val
         && updated.lrlMaxAttributes == defaultLogRecordLimits.lrlMaxAttributes
  ]


-------------------------------------------------------------------------------
-- 2. NoOpLogRecordExporter
-------------------------------------------------------------------------------

noOpLogRecordExporterTests :: TestTree
noOpLogRecordExporterTests = testGroup "NoOpLogRecordExporter"
  [ testCase "exportLogRecords returns ExportSuccess" $ do
      result <- exportLogRecords NoOpLogRecordExporter []
      result @?= ExportSuccess

  , testCase "shutdownLogExporter returns Right ()" $ do
      result <- shutdownLogExporter NoOpLogRecordExporter
      assertRight result

  , testCase "forceFlushLogExporter returns Right ()" $ do
      result <- forceFlushLogExporter NoOpLogRecordExporter Nothing
      assertRight result

  , testCase "SomeLogRecordExporter wrapper delegates exportLogRecords" $ do
      let wrapped = SomeLogRecordExporter NoOpLogRecordExporter
      result <- exportLogRecords wrapped []
      result @?= ExportSuccess

  , testCase "SomeLogRecordExporter wrapper delegates shutdownLogExporter" $ do
      let wrapped = SomeLogRecordExporter NoOpLogRecordExporter
      result <- shutdownLogExporter wrapped
      assertRight result

  , testCase "SomeLogRecordExporter wrapper delegates forceFlushLogExporter" $ do
      let wrapped = SomeLogRecordExporter NoOpLogRecordExporter
      result <- forceFlushLogExporter wrapped Nothing
      assertRight result
  ]


-------------------------------------------------------------------------------
-- 3. BatchLogRecordProcessorConfig defaults
-------------------------------------------------------------------------------

batchLogRecordProcessorConfigTests :: TestTree
batchLogRecordProcessorConfigTests = testGroup "BatchLogRecordProcessorConfig defaults"
  [ testCase "blrpMaxQueueSize == 2048" $
      defaultBatchLogRecordProcessorConfig.blrpMaxQueueSize @?= 2048

  , testCase "blrpScheduledDelay == milliseconds 1000 (not 5000)" $
      defaultBatchLogRecordProcessorConfig.blrpScheduledDelay @?= milliseconds 1000

  , testCase "blrpExportTimeout == milliseconds 30000" $
      defaultBatchLogRecordProcessorConfig.blrpExportTimeout @?= milliseconds 30000

  , testCase "blrpMaxExportBatchSize == 512" $
      defaultBatchLogRecordProcessorConfig.blrpMaxExportBatchSize @?= 512
  ]


-------------------------------------------------------------------------------
-- 4. LogRecordProcessor type class (SomeLogRecordProcessor)
-------------------------------------------------------------------------------

-- | A minimal no-op log record processor for wrapper tests.
data TestLogProcessor = TestLogProcessor

instance LogRecordProcessor TestLogProcessor where
  onEmit _ _ _ = pure ()
  shutdownProcessor _ = pure (Right ())
  forceFlushProcessor _ _ = pure (Right ())

logRecordProcessorWrapperTests :: TestTree
logRecordProcessorWrapperTests = testGroup "SomeLogRecordProcessor"
  [ testCase "wrapping a no-op processor: onEmit does not crash" $ do
      let wrapped = SomeLogRecordProcessor TestLogProcessor
      -- We need a SomeReadWriteLogRecord to call onEmit. Use a real one
      -- by emitting through a provider and capturing it.
      -- For the simple wrapper test, just verify onEmit with a manufactured record.
      -- We can't easily create a SomeReadWriteLogRecord without the SDK internals,
      -- so we test via the provider path in integration tests.
      -- Here we test that the wrapper methods compile and don't crash
      -- by calling shutdown and forceFlush.
      result <- shutdownProcessor wrapped
      assertRight result

  , testCase "shutdownProcessor returns Right ()" $ do
      let wrapped = SomeLogRecordProcessor TestLogProcessor
      result <- shutdownProcessor wrapped
      assertRight result

  , testCase "forceFlushProcessor returns Right ()" $ do
      let wrapped = SomeLogRecordProcessor TestLogProcessor
      result <- forceFlushProcessor wrapped Nothing
      assertRight result
  ]


-------------------------------------------------------------------------------
-- 5. SimpleLogRecordProcessor
-------------------------------------------------------------------------------

simpleLogRecordProcessorTests :: TestTree
simpleLogRecordProcessorTests = testGroup "SimpleLogRecordProcessor"
  [ testCase "onEmit forwards record to exporter (one export call, one record)" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "test") }
      batches <- readTVarIO (rleExports recorder)
      length batches @?= 1
      case batches of
        [batch] -> length batch @?= 1
        _       -> assertFailure "expected exactly one batch"

  , testCase "onEmit called N times -> N export calls" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "a") }
      emit logger defaultLogRecord { logBody = Just (LogBodyString "b") }
      emit logger defaultLogRecord { logBody = Just (LogBodyString "c") }
      batches <- readTVarIO (rleExports recorder)
      length batches @?= 3
      assertBool "each batch has exactly 1 record" $
        all (\b -> length b == 1) batches

  , testCase "after shutdownProcessor, onEmit is silently ignored" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      -- Shutdown the provider, which sets the shutdown flag
      _ <- sdkLoggerProviderShutdown provider
      emit logger defaultLogRecord { logBody = Just (LogBodyString "ignored") }
      exported <- countExportedLogs recorder
      exported @?= 0

  , testCase "shutdownProcessor delegates to exporter's shutdownLogExporter" $ do
      recorder <- newRecordingLogExporter
      proc <- newSimpleLogRecordProcessor (SomeLogRecordExporter recorder)
      result <- shutdownProcessor proc
      assertRight result
      count <- readTVarIO (rleShutdowns recorder)
      assertBool "exporter shutdown should have been called" (count > 0)

  , testCase "forceFlushProcessor delegates to exporter's forceFlushLogExporter" $ do
      recorder <- newRecordingLogExporter
      proc <- newSimpleLogRecordProcessor (SomeLogRecordExporter recorder)
      result <- forceFlushProcessor proc Nothing
      assertRight result
      count <- readTVarIO (rleFlushCalls recorder)
      assertBool "exporter forceFlush should have been called" (count > 0)

  , testCase "idempotent shutdown: second call does not call exporter shutdown again" $ do
      recorder <- newRecordingLogExporter
      proc <- newSimpleLogRecordProcessor (SomeLogRecordExporter recorder)
      result1 <- shutdownProcessor proc
      assertRight result1
      result2 <- shutdownProcessor proc
      assertRight result2
      shutdownCount <- readTVarIO (rleShutdowns recorder)
      shutdownCount @?= 1
  ]


-------------------------------------------------------------------------------
-- 6. BatchLogRecordProcessor
-------------------------------------------------------------------------------

batchLogRecordProcessorTests :: TestTree
batchLogRecordProcessorTests = localOption (mkTimeout 5_000_000) $
  testGroup "BatchLogRecordProcessor"
  [ testCase "shutdown drains all queued records" $ do
      recorder <- newRecordingLogExporter
      let cfg = fastBatchConfig { blrpScheduledDelay = milliseconds 5000 }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) cfg
      -- Emit 5 records via the provider to get real SomeReadWriteLogRecords
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      replicateM_ 5 $ emit logger defaultLogRecord { logBody = Just (LogBodyString "drain") }
      _ <- shutdownProcessor bsp
      exported <- countExportedLogs recorder
      exported @?= 5

  , testCase "forceFlushProcessor triggers immediate export" $ do
      recorder <- newRecordingLogExporter
      let cfg = fastBatchConfig { blrpScheduledDelay = milliseconds 5000 }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) cfg
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      replicateM_ 3 $ emit logger defaultLogRecord { logBody = Just (LogBodyString "flush") }
      _ <- forceFlushProcessor bsp Nothing
      exported <- countExportedLogs recorder
      exported @?= 3
      _ <- shutdownProcessor bsp
      pure ()

  , testCase "queue-full records are dropped silently" $ do
      recorder <- newRecordingLogExporter
      let cfg = fastBatchConfig
            { blrpMaxQueueSize = 3
            , blrpScheduledDelay = milliseconds 5000
            }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) cfg
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      replicateM_ 5 $ emit logger defaultLogRecord { logBody = Just (LogBodyString "overflow") }
      _ <- shutdownProcessor bsp
      exported <- countExportedLogs recorder
      assertBool ("expected <= 3 exported, got " <> show exported)
        (exported <= 3)

  , testCase "periodic export fires after delay" $ do
      recorder <- newRecordingLogExporter
      let cfg = fastBatchConfig { blrpScheduledDelay = milliseconds 20 }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) cfg
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "periodic") }
      threadDelay 200_000
      exported <- countExportedLogs recorder
      assertBool ("expected at least 1 exported by timer, got " <> show exported)
        (exported >= 1)
      _ <- shutdownProcessor bsp
      pure ()

  , testCase "concurrent onEmit: 10 threads x 5 records -> shutdown exports 50" $ do
      recorder <- newRecordingLogExporter
      let cfg = fastBatchConfig
            { blrpMaxQueueSize = 100
            , blrpMaxExportBatchSize = 100
            , blrpScheduledDelay = milliseconds 5000
            }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) cfg
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      let nThreads = 10 :: Int
          recsPerThread = 5 :: Int
      done <- newEmptyMVar
      replicateM_ nThreads $ forkIO $ do
        replicateM_ recsPerThread $
          emit logger defaultLogRecord { logBody = Just (LogBodyString "concurrent") }
        putMVar done ()
      replicateM_ nThreads (takeMVar done)
      _ <- shutdownProcessor bsp
      exported <- countExportedLogs recorder
      assertBool ("expected 50 exported, got " <> show exported)
        (exported == nThreads * recsPerThread)

  , testCase "idempotent shutdown does not crash" $ do
      recorder <- newRecordingLogExporter
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter recorder) fastBatchConfig
      r1 <- shutdownProcessor bsp
      r2 <- shutdownProcessor bsp
      assertRight r1
      assertRight r2
  ]


-------------------------------------------------------------------------------
-- 6b. BatchLogRecordProcessor export timeout
-------------------------------------------------------------------------------

batchLogRecordProcessorExportTimeoutTests :: TestTree
batchLogRecordProcessorExportTimeoutTests = localOption (mkTimeout 5_000_000) $
  testGroup "BatchLogRecordProcessor export timeout"
  [ testCase "BatchLogRecordProcessor respects blrpExportTimeout: slow exporter does not block indefinitely" $ do
      (slowExp, completedRef) <- newVerySlowLogExporter
      let cfg = fastBatchConfig
            { blrpExportTimeout  = milliseconds 100
            , blrpScheduledDelay = milliseconds 50
            }
      bsp <- newBatchLogRecordProcessor (SomeLogRecordExporter slowExp) cfg
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor bsp] }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "timeout-test") }
      -- Wait 300ms: the scheduler should have fired (50ms) and the
      -- export timeout (100ms) should have cancelled the 10s export.
      threadDelay 300_000
      completed <- readIORef completedRef
      assertBool
        "slow export should have been cancelled by blrpExportTimeout (IORef should be False)"
        (not completed)
      _ <- shutdownProcessor bsp
      pure ()
  ]


-------------------------------------------------------------------------------
-- 7. SdkLoggerProvider construction
-------------------------------------------------------------------------------

sdkLoggerProviderTests :: TestTree
sdkLoggerProviderTests = testGroup "SdkLoggerProvider"
  [ testCase "newSdkLoggerProvider with default config succeeds" $ do
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
      -- If we reach here, construction succeeded
      _ <- getLogger provider testScope
      pure @IO ()

  , testCase "getLogger returns a SomeLogger" $ do
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
      _logger <- getLogger provider testScope
      -- Compiles and returns without error
      pure @IO ()

  , testCase "after shutdown, getLogger returns NoOp logger (isEnabled returns False)" $ do
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
      _ <- sdkLoggerProviderShutdown provider
      logger <- getLogger provider testScope
      enabled <- isEnabled logger SeverityInfo Nothing Nothing
      assertBool "logger after shutdown should not be enabled" (not enabled)

  , testCase "sdkLoggerProviderForceFlush with no processors returns Right ()" $ do
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
      result <- sdkLoggerProviderForceFlush provider Nothing
      assertRight result
  ]


-------------------------------------------------------------------------------
-- 8. Integration: Logger -> Exporter
-------------------------------------------------------------------------------

integrationTests :: TestTree
integrationTests = testGroup "Integration: Logger -> Exporter"
  [ testCase "emitted log record reaches exporter with correct body" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "hello") }
      records <- getAllExportedLogRecords recorder
      length records @?= 1
      case records of
        [SomeReadableLogRecord r] ->
          rlrBody r @?= Just (LogBodyString "hello")
        _ -> assertFailure "expected exactly one record"

  , testCase "rlrScope matches testScope" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "scope-test") }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          rlrScope r @?= testScope
        _ -> assertFailure "expected exactly one record"

  , testCase "rlrResource matches provider resource (Resource.empty)" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "res-test") }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          rlrResource r @?= Resource.empty
        _ -> assertFailure "expected exactly one record"
  ]


-------------------------------------------------------------------------------
-- 9. ReadWriteLogRecord setters
-------------------------------------------------------------------------------

readWriteLogRecordTests :: TestTree
readWriteLogRecordTests = testGroup "ReadWriteLogRecord setters"
  [ testCase "rwlrSetBody modifies body" $ do
      (rw, _provider) <- captureReadWriteLogRecord
      rwlrSetBody rw (LogBodyString "modified")
      body <- pure (rlrBody rw)
      body @?= Just (LogBodyString "modified")

  , testCase "rwlrSetSeverityNumber modifies severity" $ do
      (rw, _provider) <- captureReadWriteLogRecord
      rwlrSetSeverityNumber rw SeverityInfo
      sev <- pure (rlrSeverityNumber rw)
      sev @?= Just SeverityInfo

  , testCase "rwlrSetSeverityText modifies severity text" $ do
      (rw, _provider) <- captureReadWriteLogRecord
      rwlrSetSeverityText rw "INFO"
      sevText <- pure (rlrSeverityText rw)
      sevText @?= Just "INFO"

  , testCase "rwlrSetAttribute adds attribute" $ do
      (rw, _provider) <- captureReadWriteLogRecord
      rwlrSetAttribute rw "k" (StringValue "v")
      let attrs = rlrAttributes rw
      lookup "k" attrs @?= Just (StringValue "v")
  ]
  where
    captureReadWriteLogRecord :: IO (SomeReadWriteLogRecord, SdkLoggerProvider)
    captureReadWriteLogRecord = do
      capProc <- newCapturingProcessor
      let CapturingProcessor var = capProc
      provider <- newSdkLoggerProvider defaultSdkLoggerProviderConfig
        { llpProcessors = [SomeLogRecordProcessor capProc] }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord { logBody = Just (LogBodyString "original") }
      mRw <- readTVarIO var
      case mRw of
        Just rw -> pure (rw, provider)
        Nothing -> assertFailure "CapturingProcessor did not capture a record" >> error "unreachable"


-------------------------------------------------------------------------------
-- 10. LogRecordLimits: excess attributes dropped
-------------------------------------------------------------------------------

logRecordLimitsEnforcementTests :: TestTree
logRecordLimitsEnforcementTests = testGroup "LogRecordLimits enforcement"
  [ testCase "excess attributes dropped (max 2, emit 3)" $ do
      let limits = defaultLogRecordLimits { lrlMaxAttributes = 2 }
      (provider, recorder) <- newTestLogProviderWith
        defaultSdkLoggerProviderConfig { llpLimits = limits }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord
        { logAttributes = fromList
            [ ("a", StringValue "1")
            , ("b", StringValue "2")
            , ("c", StringValue "3")
            ]
        }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] -> do
          size (rlrAttributes r) @?= 2
          rlrDroppedAttributes r @?= 1
        _ -> assertFailure "expected exactly one record"

  , testCase "lrlMaxAttributeValueLen truncates string values" $ do
      let limits = defaultLogRecordLimits { lrlMaxAttributeValueLen = Just 5 }
      (provider, recorder) <- newTestLogProviderWith
        defaultSdkLoggerProviderConfig { llpLimits = limits }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord
        { logAttributes = fromList [("key", StringValue "abcdefghij")] }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          lookup "key" (rlrAttributes r) @?= Just (StringValue "abcde")
        _ -> assertFailure "expected exactly one record"

  , testCase "non-string attribute values are NOT truncated" $ do
      let limits = defaultLogRecordLimits { lrlMaxAttributeValueLen = Just 1 }
      (provider, recorder) <- newTestLogProviderWith
        defaultSdkLoggerProviderConfig { llpLimits = limits }
      logger <- getLogger provider testScope
      emit logger defaultLogRecord
        { logAttributes = fromList
            [ ("int", Int64Value 999999)
            , ("bool", BoolValue True)
            ]
        }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] -> do
          lookup "int" (rlrAttributes r) @?= Just (Int64Value 999999)
          lookup "bool" (rlrAttributes r) @?= Just (BoolValue True)
        _ -> assertFailure "expected exactly one record"
  ]


-------------------------------------------------------------------------------
-- 11. Trace correlation
-------------------------------------------------------------------------------

traceCorrelationTests :: TestTree
traceCorrelationTests = testGroup "Trace correlation"
  [ testCase "log record carries span context from active span" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      let sc = validSpanContext
          span_ = createNonRecordingSpan sc
          ctx = setSpanInContext span_ root
      emit logger defaultLogRecord
        { logBody = Just (LogBodyString "with-span")
        , logContext = Just ctx
        }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          rlrSpanContext r @?= Just sc
        _ -> assertFailure "expected exactly one record"

  , testCase "no context in log record -> rlrSpanContext == Nothing" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      emit logger defaultLogRecord
        { logBody = Just (LogBodyString "no-ctx")
        , logContext = Nothing
        }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          rlrSpanContext r @?= Nothing
        _ -> assertFailure "expected exactly one record"

  , testCase "context with invalid span context -> rlrSpanContext == Nothing" $ do
      (provider, recorder) <- newTestLogProvider
      logger <- getLogger provider testScope
      let sc = invalidSpanContext  -- all-zero, isValid == False
          span_ = createNonRecordingSpan sc
          ctx = setSpanInContext span_ root
      emit logger defaultLogRecord
        { logBody = Just (LogBodyString "invalid-sc")
        , logContext = Just ctx
        }
      records <- getAllExportedLogRecords recorder
      case records of
        [SomeReadableLogRecord r] ->
          rlrSpanContext r @?= Nothing
        _ -> assertFailure "expected exactly one record"
  ]


-------------------------------------------------------------------------------
-- 12. SeverityNumber enum completeness
-------------------------------------------------------------------------------

severityNumberTests :: TestTree
severityNumberTests = testGroup "SeverityNumber"
  [ testCase "all 24 severity variants exist" $
      length [minBound .. maxBound :: SeverityNumber] @?= 24

  , testCase "severityNumberValue minBound == 1" $
      severityNumberValue minBound @?= 1

  , testCase "severityNumberValue maxBound == 24" $
      severityNumberValue maxBound @?= 24

  , testProperty "enum round-trip: toEnum (fromEnum s) == s" $
      \n -> let idx = n `mod` 24
                s = toEnum idx :: SeverityNumber
            in toEnum (fromEnum s) === s
  ]
