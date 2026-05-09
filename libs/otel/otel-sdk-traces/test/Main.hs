-- | Property-based and specification tests for otel-sdk-traces.
--
-- These tests verify the algebraic properties and specification-mandated
-- defaults of SpanLimits, SpanEvent, Link, SamplingDecision, and the
-- existential wrappers for SpanExporter, SpanProcessor, Sampler, and
-- IdGenerator.
module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', writeTVar, atomically)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (replicateM_)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word8, Word64)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import OTel.Attribute (InstrumentationScope(..), emptyAttributes, fromList, size, AttributeValue(..), lookup)
import Prelude hiding (lookup)
import OTel.Context (root)
import OTel.SDK.Export (ExportResult(..), FlushError(..))
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace
import OTel.SDK.Trace.Export
import OTel.SDK.Trace.IdGenerator
import OTel.SDK.Trace.Processor
import OTel.SDK.Trace.Sampler
import OTel.Timestamp (fromNanos, milliseconds)
import OTel.Trace
  ( Span(..), SpanKind(..), SpanStatus(..), StatusCode(..)
  , Tracer(..), TracerProvider(..)
  , SpanConfig(..), defaultSpanConfig, setSpanInContext
  , createNonRecordingSpan
  )
import OTel.Trace.SpanContext
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Assert that an Either is Right ().
assertRight :: Show e => Either e () -> Assertion
assertRight (Right ()) = pure ()
assertRight (Left e) = assertFailure ("expected Right (), got Left: " ++ show e)


-------------------------------------------------------------------------------
-- Dummy test instances
-------------------------------------------------------------------------------

-- | A minimal exporter that always succeeds.
data TestExporter = TestExporter

instance SpanExporter TestExporter where
  exportSpans _ _ = pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


-- | A minimal span type implementing both Span and ReadableSpan, for use
-- with SpanProcessor.onStart which requires SomeReadWriteSpan.
data TestSpan = TestSpan

instance Span TestSpan where
  getSpanContext _ = pure invalidSpanContext
  isRecording _ = pure False
  setAttribute _ _ _ = pure ()
  addEvent _ _ _ _ = pure ()
  addLink _ _ _ = pure ()
  setStatus _ _ _ = pure ()
  recordException _ _ _ = pure ()
  updateName _ _ = pure ()
  end _ _ = pure ()

instance ReadableSpan TestSpan where
  readSpanContext _ = invalidSpanContext
  readParentSpanContext _ = Nothing
  readName _ = "test-span"
  readKind _ = Internal
  readStartTimestamp _ = fromNanos 0
  readEndTimestamp _ = fromNanos 0
  readAttributes _ = emptyAttributes
  readEvents _ = []
  readLinks _ = []
  readStatus _ = SpanStatus Unset Nothing
  readResource _ = Resource.empty
  readInstrumentationScope _ = InstrumentationScope "test" Nothing Nothing Nothing
  readDroppedAttributesCount _ = 0
  readDroppedEventsCount _ = 0
  readDroppedLinksCount _ = 0

instance ReadWriteSpan TestSpan


-- | A minimal processor that does nothing.
data TestProcessor = TestProcessor

instance SpanProcessor TestProcessor where
  onStart _ _ _ = pure ()
  onEnd _ _ = pure ()
  shutdownProcessor _ = pure (Right ())
  forceFlushProcessor _ _ = pure (Right ())


-- | A minimal sampler that always records and samples.
data TestSampler = TestSampler

instance Sampler TestSampler where
  shouldSample _ _ _ _ _ _ _ =
    pure (SamplingResult RecordAndSample emptyAttributes TraceState.empty)
  samplerDescription _ = "TestSampler"


-- | A minimal ID generator that produces invalid (zero) IDs.
data TestIdGenerator = TestIdGenerator

instance IdGenerator TestIdGenerator where
  generateTraceId _ = pure invalidTraceId
  generateSpanId _ = pure invalidSpanId


-- | An exporter that sleeps before returning, used to trigger flush timeouts.
data SlowExporter = SlowExporter

instance SpanExporter SlowExporter where
  exportSpans _ _ = threadDelay 200_000 >> pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


-- | An exporter that sleeps for a very long time and tracks whether its
-- export completed. Used to verify that export timeout cancels the export.
data VerySlowExporter = VerySlowExporter !(IORef Bool)

newVerySlowExporter :: IO (VerySlowExporter, IORef Bool)
newVerySlowExporter = do
  ref <- newIORef False
  pure (VerySlowExporter ref, ref)

instance SpanExporter VerySlowExporter where
  exportSpans (VerySlowExporter ref) _ = do
    threadDelay 10_000_000  -- 10 seconds
    writeIORef ref True
    pure ExportSuccess
  shutdownExporter _ = pure (Right ())
  forceFlushExporter _ _ = pure (Right ())


-------------------------------------------------------------------------------
-- Newtype wrappers for Arbitrary (avoiding orphans)
-------------------------------------------------------------------------------

newtype ArbSpanLimits = ArbSpanLimits { getArbSpanLimits :: SpanLimits }
  deriving stock Show

instance Arbitrary ArbSpanLimits where
  arbitrary = fmap ArbSpanLimits $ SpanLimits
    <$> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)
    <*> (fmap getNonNegative <$> arbitrary)
  shrink (ArbSpanLimits sl) =
    [ ArbSpanLimits sl { maxAttributes = a }
    | a <- shrink sl.maxAttributes, a >= 0
    ] ++
    [ ArbSpanLimits sl { maxEvents = e }
    | e <- shrink sl.maxEvents, e >= 0
    ] ++
    [ ArbSpanLimits sl { maxLinks = l }
    | l <- shrink sl.maxLinks, l >= 0
    ] ++
    [ ArbSpanLimits sl { maxAttributesPerEvent = ae }
    | ae <- shrink sl.maxAttributesPerEvent, ae >= 0
    ] ++
    [ ArbSpanLimits sl { maxAttributesPerLink = al }
    | al <- shrink sl.maxAttributesPerLink, al >= 0
    ] ++
    [ ArbSpanLimits sl { maxAttributeValueLength = vl }
    | vl <- shrink sl.maxAttributeValueLength
    , maybe True (>= 0) vl
    ]

newtype ArbSamplingDecision = ArbSamplingDecision { getArbSamplingDecision :: SamplingDecision }
  deriving stock Show

instance Arbitrary ArbSamplingDecision where
  arbitrary = ArbSamplingDecision <$> elements [Drop, RecordOnly, RecordAndSample]
  shrink (ArbSamplingDecision RecordAndSample) = ArbSamplingDecision <$> [Drop, RecordOnly]
  shrink (ArbSamplingDecision RecordOnly) = [ArbSamplingDecision Drop]
  shrink (ArbSamplingDecision Drop) = []


-------------------------------------------------------------------------------
-- Test tree
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "otel-sdk-traces"
  [ spanLimitsTests
  , spanEventTests
  , linkTests
  , samplingDecisionTests
  , samplingResultTests
  , existentialWrapperTests
  , instanceBehaviorTests
  , someReadWriteSpanTests
  , simpleSpanProcessorTests
  , sdkSpanCreationTests
  , sdkSpanLifecycleTests
  , sdkSpanLimitsTests
  , sdkStatusOverrideTests
  , sdkParentChildTests
  , sdkProcessorNotificationTests
  , sdkProviderLifecycleTests
  , sdkSamplingTests
  , samplerTests
  , batchSpanProcessorTests
  ]


-------------------------------------------------------------------------------
-- SpanLimits
-------------------------------------------------------------------------------

spanLimitsTests :: TestTree
spanLimitsTests = testGroup "SpanLimits"
  [ testGroup "defaultSpanLimits spec compliance"
      [ testCase "maxAttributes == 128" $
          defaultSpanLimits.maxAttributes @?= 128
      , testCase "maxEvents == 128" $
          defaultSpanLimits.maxEvents @?= 128
      , testCase "maxLinks == 128" $
          defaultSpanLimits.maxLinks @?= 128
      , testCase "maxAttributesPerEvent == 128" $
          defaultSpanLimits.maxAttributesPerEvent @?= 128
      , testCase "maxAttributesPerLink == 128" $
          defaultSpanLimits.maxAttributesPerLink @?= 128
      , testCase "maxAttributeValueLength == Nothing" $
          defaultSpanLimits.maxAttributeValueLength @?= Nothing
      ]

  , testGroup "record update properties"
      [ testProperty "updating maxAttributes changes only that field" $
          \(NonNegative n) ->
            let updated = defaultSpanLimits { maxAttributes = n }
            in  updated.maxAttributes == n
             && updated.maxEvents == defaultSpanLimits.maxEvents
             && updated.maxLinks == defaultSpanLimits.maxLinks
             && updated.maxAttributesPerEvent == defaultSpanLimits.maxAttributesPerEvent
             && updated.maxAttributesPerLink == defaultSpanLimits.maxAttributesPerLink
             && updated.maxAttributeValueLength == defaultSpanLimits.maxAttributeValueLength

      , testProperty "updating maxEvents changes only that field" $
          \(NonNegative n) ->
            let updated = defaultSpanLimits { maxEvents = n }
            in  updated.maxEvents == n
             && updated.maxAttributes == defaultSpanLimits.maxAttributes

      , testProperty "updating maxLinks changes only that field" $
          \(NonNegative n) ->
            let updated = defaultSpanLimits { maxLinks = n }
            in  updated.maxLinks == n
             && updated.maxAttributes == defaultSpanLimits.maxAttributes

      , testProperty "updating maxAttributeValueLength changes only that field" $
          \(n :: Maybe (NonNegative Int)) ->
            let val = fmap getNonNegative n
                updated = defaultSpanLimits { maxAttributeValueLength = val }
            in  updated.maxAttributeValueLength == val
             && updated.maxAttributes == defaultSpanLimits.maxAttributes
      ]

  , testGroup "Eq instance"
      [ testProperty "reflexivity" $
          \(ArbSpanLimits sl) -> sl == sl

      , testProperty "defaultSpanLimits equals itself" $
          once $ defaultSpanLimits == defaultSpanLimits

      , testProperty "distinct values are not equal" $
          \(ArbSpanLimits sl) ->
            sl.maxAttributes /= sl.maxAttributes + 1 ==>
              sl /= sl { maxAttributes = sl.maxAttributes + 1 }
      ]

  , testGroup "Show instance"
      [ testProperty "show produces non-empty string" $
          \(ArbSpanLimits sl) -> not (null (show sl))
      ]
  ]


-------------------------------------------------------------------------------
-- SpanEvent
-------------------------------------------------------------------------------

spanEventTests :: TestTree
spanEventTests = testGroup "SpanEvent"
  [ testCase "fields are accessible after construction" $ do
      let ts = fromNanos 1000
          attrs = fromList [("key", StringValue "value")]
          event = SpanEvent
            { eventName = "test-event"
            , eventTimestamp = ts
            , eventAttributes = attrs
            , eventDroppedAttributesCount = 3
            }
      event.eventName @?= "test-event"
      event.eventTimestamp @?= ts
      event.eventAttributes @?= attrs
      event.eventDroppedAttributesCount @?= 3

  , testCase "Eq: identical events are equal" $ do
      let ts = fromNanos 500
          event = SpanEvent "evt" ts emptyAttributes 0
      event @?= event

  , testCase "Eq: different names are not equal" $ do
      let ts = fromNanos 500
          e1 = SpanEvent "a" ts emptyAttributes 0
          e2 = SpanEvent "b" ts emptyAttributes 0
      assertBool "events with different names should not be equal" (e1 /= e2)

  , testCase "Eq: different dropped counts are not equal" $ do
      let ts = fromNanos 500
          e1 = SpanEvent "a" ts emptyAttributes 0
          e2 = SpanEvent "a" ts emptyAttributes 1
      assertBool "events with different dropped counts should not be equal" (e1 /= e2)
  ]


-------------------------------------------------------------------------------
-- Link
-------------------------------------------------------------------------------

linkTests :: TestTree
linkTests = testGroup "Link"
  [ testCase "fields are accessible after construction" $ do
      let attrs = fromList [("link.type", StringValue "parent")]
          link = Link
            { linkSpanContext = invalidSpanContext
            , linkAttributes = attrs
            , linkDroppedAttributesCount = 2
            }
      link.linkSpanContext @?= invalidSpanContext
      link.linkAttributes @?= attrs
      link.linkDroppedAttributesCount @?= 2

  , testCase "Eq: identical links are equal" $ do
      let link = Link invalidSpanContext emptyAttributes 0
      link @?= link

  , testCase "Eq: different dropped counts are not equal" $ do
      let l1 = Link invalidSpanContext emptyAttributes 0
          l2 = Link invalidSpanContext emptyAttributes 5
      assertBool "links with different dropped counts should not be equal" (l1 /= l2)
  ]


-------------------------------------------------------------------------------
-- SamplingDecision
-------------------------------------------------------------------------------

samplingDecisionTests :: TestTree
samplingDecisionTests = testGroup "SamplingDecision"
  [ testCase "all three constructors are distinct" $ do
      assertBool "Drop /= RecordOnly" (Drop /= RecordOnly)
      assertBool "Drop /= RecordAndSample" (Drop /= RecordAndSample)
      assertBool "RecordOnly /= RecordAndSample" (RecordOnly /= RecordAndSample)

  , testProperty "Enum round-trip: toEnum . fromEnum == id" $
      \(ArbSamplingDecision d) -> toEnum (fromEnum d) == d

  , testProperty "fromEnum values are 0, 1, 2" $
      \(ArbSamplingDecision d) -> fromEnum d `elem` [0, 1, 2 :: Int]

  , testCase "Bounded: minBound is Drop" $
      (minBound :: SamplingDecision) @?= Drop

  , testCase "Bounded: maxBound is RecordAndSample" $
      (maxBound :: SamplingDecision) @?= RecordAndSample

  , testProperty "Eq is reflexive" $
      \(ArbSamplingDecision d) -> d == d

  , testProperty "Show produces non-empty string" $
      \(ArbSamplingDecision d) -> not (null (show d))
  ]


-------------------------------------------------------------------------------
-- SamplingResult
-------------------------------------------------------------------------------

samplingResultTests :: TestTree
samplingResultTests = testGroup "SamplingResult"
  [ testCase "fields are accessible after construction" $ do
      let result = SamplingResult
            { samplingDecision = RecordAndSample
            , samplingAttributes = fromList [("sampler", StringValue "test")]
            , samplingTraceState = TraceState.set "vendor" "data" TraceState.empty
            }
      result.samplingDecision @?= RecordAndSample
      result.samplingAttributes @?= fromList [("sampler", StringValue "test")]

  , testCase "Eq: identical results are equal" $ do
      let r = SamplingResult Drop emptyAttributes TraceState.empty
      r @?= r

  , testCase "Eq: different decisions are not equal" $ do
      let r1 = SamplingResult Drop emptyAttributes TraceState.empty
          r2 = SamplingResult RecordAndSample emptyAttributes TraceState.empty
      assertBool "results with different decisions should not be equal" (r1 /= r2)
  ]


-------------------------------------------------------------------------------
-- Existential wrappers
-------------------------------------------------------------------------------

existentialWrapperTests :: TestTree
existentialWrapperTests = testGroup "Existential wrappers"
  [ testCase "SomeSpanExporter can be constructed" $ do
      let wrapped = SomeSpanExporter TestExporter
      -- Verify we can call methods through the wrapper
      result <- exportSpans wrapped []
      result @?= ExportSuccess

  , testCase "SomeSpanProcessor can be constructed" $ do
      let wrapped = SomeSpanProcessor TestProcessor
      -- Verify we can call shutdownProcessor through the wrapper
      result <- shutdownProcessor wrapped
      assertRight result

  , testCase "SomeSampler can be constructed" $ do
      let wrapped = SomeSampler TestSampler
      desc <- pure (samplerDescription wrapped)
      desc @?= "TestSampler"

  , testCase "SomeIdGenerator can be constructed" $ do
      let wrapped = SomeIdGenerator TestIdGenerator
      tid <- generateTraceId wrapped
      tid @?= invalidTraceId
  ]


-------------------------------------------------------------------------------
-- Instance behavior tests
-------------------------------------------------------------------------------

instanceBehaviorTests :: TestTree
instanceBehaviorTests = testGroup "Test instance behavior"
  [ testCase "TestExporter.exportSpans returns ExportSuccess" $ do
      result <- exportSpans TestExporter []
      result @?= ExportSuccess

  , testCase "TestExporter.shutdownExporter returns Right ()" $ do
      result <- shutdownExporter TestExporter
      assertRight result

  , testCase "TestExporter.forceFlushExporter returns Right ()" $ do
      result <- forceFlushExporter TestExporter Nothing
      assertRight result

  , testCase "TestProcessor.onStart does not crash" $ do
      let rwSpan = SomeReadWriteSpan TestSpan
      onStart TestProcessor rwSpan root
      -- If we reach here, onStart didn't crash
      pure @IO ()

  , testCase "TestProcessor.onEnd does not crash" $ do
      let readSpan = SomeReadableSpan TestSpan
      onEnd TestProcessor readSpan
      pure @IO ()

  , testCase "TestProcessor.shutdownProcessor returns Right ()" $ do
      result <- shutdownProcessor TestProcessor
      assertRight result

  , testCase "TestSampler.shouldSample returns RecordAndSample" $ do
      result <- shouldSample TestSampler root invalidTraceId "test" Internal emptyAttributes []
      result.samplingDecision @?= RecordAndSample

  , testCase "TestSampler.shouldSample returns empty attributes" $ do
      result <- shouldSample TestSampler root invalidTraceId "test" Internal emptyAttributes []
      result.samplingAttributes @?= emptyAttributes

  , testCase "TestSampler.samplerDescription returns \"TestSampler\"" $
      samplerDescription TestSampler @?= ("TestSampler" :: Text)

  , testCase "TestIdGenerator.generateTraceId returns invalidTraceId" $ do
      tid <- generateTraceId TestIdGenerator
      tid @?= invalidTraceId

  , testCase "TestIdGenerator.generateSpanId returns invalidSpanId" $ do
      sid <- generateSpanId TestIdGenerator
      sid @?= invalidSpanId

    -- Existential wrapper delegation: verify the wrapper delegates correctly
  , testCase "SomeSpanExporter delegates exportSpans" $ do
      result <- exportSpans (SomeSpanExporter TestExporter) []
      result @?= ExportSuccess

  , testCase "SomeSpanProcessor delegates shutdownProcessor" $ do
      result <- shutdownProcessor (SomeSpanProcessor TestProcessor)
      assertRight result

  , testCase "SomeSampler delegates shouldSample" $ do
      result <- shouldSample (SomeSampler TestSampler) root invalidTraceId "x" Internal emptyAttributes []
      result.samplingDecision @?= RecordAndSample

  , testCase "SomeSampler delegates samplerDescription" $
      samplerDescription (SomeSampler TestSampler) @?= ("TestSampler" :: Text)

  , testCase "SomeIdGenerator delegates generateTraceId" $ do
      tid <- generateTraceId (SomeIdGenerator TestIdGenerator)
      tid @?= invalidTraceId

  , testCase "SomeIdGenerator delegates generateSpanId" $ do
      sid <- generateSpanId (SomeIdGenerator TestIdGenerator)
      sid @?= invalidSpanId
  ]


-------------------------------------------------------------------------------
-- SomeReadWriteSpan forwarding instances
-------------------------------------------------------------------------------

someReadWriteSpanTests :: TestTree
someReadWriteSpanTests = testGroup "SomeReadWriteSpan forwarding instances"
  [ testGroup "ReadableSpan forwarding"
      [ testCase "readName forwards to underlying span" $
          readName wrapped @?= "test-span"

      , testCase "readKind forwards to underlying span" $
          readKind wrapped @?= Internal

      , testCase "readSpanContext forwards to underlying span" $
          readSpanContext wrapped @?= invalidSpanContext

      , testCase "readParentSpanContext forwards to underlying span" $
          readParentSpanContext wrapped @?= Nothing

      , testCase "readStartTimestamp forwards to underlying span" $
          readStartTimestamp wrapped @?= fromNanos 0

      , testCase "readEndTimestamp forwards to underlying span" $
          readEndTimestamp wrapped @?= fromNanos 0

      , testCase "readAttributes forwards to underlying span" $
          readAttributes wrapped @?= emptyAttributes

      , testCase "readEvents forwards to underlying span" $
          readEvents wrapped @?= []

      , testCase "readLinks forwards to underlying span" $
          readLinks wrapped @?= []

      , testCase "readStatus forwards to underlying span" $
          readStatus wrapped @?= SpanStatus Unset Nothing

      , testCase "readResource forwards to underlying span" $
          readResource wrapped @?= Resource.empty

      , testCase "readInstrumentationScope forwards to underlying span" $
          readInstrumentationScope wrapped @?= InstrumentationScope "test" Nothing Nothing Nothing

      , testCase "readDroppedAttributesCount forwards to underlying span" $
          readDroppedAttributesCount wrapped @?= 0

      , testCase "readDroppedEventsCount forwards to underlying span" $
          readDroppedEventsCount wrapped @?= 0

      , testCase "readDroppedLinksCount forwards to underlying span" $
          readDroppedLinksCount wrapped @?= 0
      ]

  , testGroup "Span forwarding"
      [ testCase "isRecording forwards to underlying span" $ do
          recording <- isRecording wrapped
          recording @?= False

      , testCase "getSpanContext forwards to underlying span" $ do
          ctx <- getSpanContext wrapped
          ctx @?= invalidSpanContext
      ]

  , testGroup "Mutating Span ops do not crash"
      [ testCase "setAttribute does not crash" $ do
          setAttribute wrapped "key" (StringValue "value")
          pure @IO ()

      , testCase "addEvent does not crash" $ do
          addEvent wrapped "event" emptyAttributes (Just (fromNanos 100))
          pure @IO ()

      , testCase "setStatus does not crash" $ do
          setStatus wrapped Ok (Just "all good")
          pure @IO ()

      , testCase "end does not crash" $ do
          end wrapped (Just (fromNanos 200))
          pure @IO ()
      ]

  , testGroup "ReadableSpan accessible without unwrapping"
      [ testCase "readAttributes callable directly on SomeReadWriteSpan" $ do
          -- The ergonomic property: no pattern match needed
          let attrs = readAttributes (SomeReadWriteSpan TestSpan)
          attrs @?= emptyAttributes

      , testCase "readName callable directly on SomeReadWriteSpan" $ do
          let name = readName (SomeReadWriteSpan TestSpan)
          name @?= "test-span"
      ]
  ]
  where
    wrapped :: SomeReadWriteSpan
    wrapped = SomeReadWriteSpan TestSpan


-------------------------------------------------------------------------------
-- RecordingExporter (test helper for SimpleSpanProcessor)
-------------------------------------------------------------------------------

-- | An exporter that records all calls for verification in tests.
data RecordingExporter = RecordingExporter
  { exportedBatches :: !(TVar [[SomeReadableSpan]])
  , exporterShutdownCount :: !(TVar Int)
  , exporterFlushCalled :: !(TVar Bool)
  }

newRecordingExporter :: IO RecordingExporter
newRecordingExporter = RecordingExporter
  <$> newTVarIO []
  <*> newTVarIO 0
  <*> newTVarIO False

instance SpanExporter RecordingExporter where
  exportSpans e spans = do
    atomically $ modifyTVar' (exportedBatches e) (spans :)
    pure ExportSuccess
  shutdownExporter e = do
    atomically $ modifyTVar' (exporterShutdownCount e) (+ 1)
    pure (Right ())
  forceFlushExporter e _ = do
    atomically $ writeTVar (exporterFlushCalled e) True
    pure (Right ())


-------------------------------------------------------------------------------
-- SimpleSpanProcessor
-------------------------------------------------------------------------------

simpleSpanProcessorTests :: TestTree
simpleSpanProcessorTests = testGroup "SimpleSpanProcessor"
  [ testCase "onEnd forwards span to exporter" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      let span_ = SomeReadableSpan TestSpan
      onEnd proc span_
      batches <- readTVarIO (exportedBatches recorder)
      length batches @?= 1

  , testCase "onEnd exports each span individually (one batch per call)" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      let span_ = SomeReadableSpan TestSpan
      onEnd proc span_
      onEnd proc span_
      onEnd proc span_
      batches <- readTVarIO (exportedBatches recorder)
      -- 3 export calls, each with exactly 1 span
      length batches @?= 3
      assertBool "each batch has exactly 1 span" $
        all (\b -> length b == 1) batches

  , testCase "onStart is a no-op (exporter not called)" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      let rwSpan = SomeReadWriteSpan TestSpan
      onStart proc rwSpan root
      batches <- readTVarIO (exportedBatches recorder)
      length batches @?= 0

  , testCase "after shutdown, onEnd is a no-op" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      _ <- shutdownProcessor proc
      let span_ = SomeReadableSpan TestSpan
      onEnd proc span_
      batches <- readTVarIO (exportedBatches recorder)
      length batches @?= 0

  , testCase "shutdown delegates to exporter's shutdownExporter" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      result <- shutdownProcessor proc
      assertRight result
      count <- readTVarIO (exporterShutdownCount recorder)
      assertBool "exporter shutdown should have been called" (count > 0)

  , testCase "forceFlush delegates to exporter's forceFlushExporter" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      result <- forceFlushProcessor proc Nothing
      assertRight result
      called <- readTVarIO (exporterFlushCalled recorder)
      assertBool "exporter forceFlush should have been called" called

  , testCase "shutdown returns exporter's shutdown result" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      result <- shutdownProcessor proc
      assertRight result

  , testCase "SomeSpanProcessor wrapping preserves onEnd behavior" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      let wrapped = SomeSpanProcessor proc
          span_ = SomeReadableSpan TestSpan
      onEnd wrapped span_
      batches <- readTVarIO (exportedBatches recorder)
      length batches @?= 1

  , testCase "repeated shutdown is idempotent — exporter shutdown called only once" $ do
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      result1 <- shutdownProcessor proc
      assertRight result1
      result2 <- shutdownProcessor proc
      assertRight result2
      shutdownCount <- readTVarIO (exporterShutdownCount recorder)
      shutdownCount @?= 1
  ]


-------------------------------------------------------------------------------
-- SDK test helpers
-------------------------------------------------------------------------------

-- | An InstrumentationScope for SDK integration tests.
testScope :: InstrumentationScope
testScope = InstrumentationScope "test-lib" (Just "1.0") Nothing Nothing

-- | Create a provider wired to a RecordingExporter via SimpleSpanProcessor.
-- Returns the provider and the recorder for inspection.
newTestProvider :: IO (SdkTracerProvider, RecordingExporter)
newTestProvider = newTestProviderWith defaultSdkTracerProviderConfig

-- | Create a provider with custom config, wired to a RecordingExporter.
newTestProviderWith :: SdkTracerProviderConfig -> IO (SdkTracerProvider, RecordingExporter)
newTestProviderWith baseConfig = do
  recorder <- newRecordingExporter
  proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
  provider <- newSdkTracerProvider baseConfig
    { providerProcessors = SomeSpanProcessor proc : providerProcessors baseConfig
    }
  pure (provider, recorder)

-- | Get the first exported span from a RecordingExporter. Fails the test if
-- nothing was exported.
getExportedSpan :: RecordingExporter -> IO SomeReadableSpan
getExportedSpan recorder = do
  batches <- readTVarIO (exportedBatches recorder)
  case concat batches of
    (s:_) -> pure s
    [] -> assertFailure "no spans exported" >> error "unreachable"

-- | Get all exported spans (flattened, most recent export first).
getAllExportedSpans :: RecordingExporter -> IO [SomeReadableSpan]
getAllExportedSpans recorder = do
  batches <- readTVarIO (exportedBatches recorder)
  pure (concat batches)

-- | A processor that records onStart and onEnd calls for verification.
data RecordingProcessor = RecordingProcessor
  { rpOnStartCalls :: !(TVar [SomeReadWriteSpan])
  , rpOnEndCalls :: !(TVar [SomeReadableSpan])
  , rpShutdownCalled :: !(TVar Bool)
  , rpFlushCalled :: !(TVar Bool)
  }

newRecordingProcessor :: IO RecordingProcessor
newRecordingProcessor = RecordingProcessor
  <$> newTVarIO []
  <*> newTVarIO []
  <*> newTVarIO False
  <*> newTVarIO False

instance SpanProcessor RecordingProcessor where
  onStart rp rwSpan _ctx = atomically $
    modifyTVar' (rpOnStartCalls rp) (rwSpan :)
  onEnd rp rSpan = atomically $
    modifyTVar' (rpOnEndCalls rp) (rSpan :)
  shutdownProcessor rp = do
    atomically $ writeTVar (rpShutdownCalled rp) True
    pure (Right ())
  forceFlushProcessor rp _ = do
    atomically $ writeTVar (rpFlushCalled rp) True
    pure (Right ())


-------------------------------------------------------------------------------
-- Span creation via SdkTracerProvider
-------------------------------------------------------------------------------

sdkSpanCreationTests :: TestTree
sdkSpanCreationTests = testGroup "SDK span creation"
  [ testCase "created span has valid SpanContext (non-zero IDs)" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "valid-ctx" root defaultSpanConfig
      ctx <- getSpanContext span_
      assertBool "span context should be valid" (isValid ctx)
      assertBool "traceId should not be invalid" (traceId ctx /= invalidTraceId)
      assertBool "spanId should not be invalid" (spanId ctx /= invalidSpanId)

  , testCase "created span: isRecording returns True" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "recording" root defaultSpanConfig
      recording <- isRecording span_
      assertBool "span should be recording" recording

  , testCase "created span: spanKind from config is reflected" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      let config = defaultSpanConfig { spanKind = Server }
      span_ <- startSpan tracer "server-span" root config
      end span_ Nothing
      exported <- getExportedSpan recorder
      readKind exported @?= Server

  , testCase "span name matches what was passed to startSpan" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "my-operation" root defaultSpanConfig
      end span_ Nothing
      exported <- getExportedSpan recorder
      readName exported @?= "my-operation"

  , testProperty "each created span gets a unique spanId" $
      once $ ioProperty $ do
        (provider, _recorder) <- newTestProvider
        tracer <- getTracer provider testScope
        span1 <- startSpan tracer "s1" root defaultSpanConfig
        span2 <- startSpan tracer "s2" root defaultSpanConfig
        ctx1 <- getSpanContext span1
        ctx2 <- getSpanContext span2
        pure (spanId ctx1 /= spanId ctx2)
  ]


-------------------------------------------------------------------------------
-- Span lifecycle
-------------------------------------------------------------------------------

sdkSpanLifecycleTests :: TestTree
sdkSpanLifecycleTests = testGroup "SDK span lifecycle"
  [ testCase "setAttribute: attributes visible after end" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "attrs" root defaultSpanConfig
      setAttribute span_ "key1" (StringValue "value1")
      end span_ Nothing
      exported <- getExportedSpan recorder
      readAttributes exported @?= fromList [("key1", StringValue "value1")]

  , testCase "addEvent: events visible after end" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "events" root defaultSpanConfig
      let ts = fromNanos 42000
      addEvent span_ "my-event" emptyAttributes (Just ts)
      end span_ Nothing
      exported <- getExportedSpan recorder
      let events = readEvents exported
      length events @?= 1
      case events of
        [evt] -> do
          evt.eventName @?= "my-event"
          evt.eventTimestamp @?= ts
        _ -> assertFailure "expected exactly one event"

  , testCase "setStatus: status visible after end" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "status" root defaultSpanConfig
      setStatus span_ Error (Just "something failed")
      end span_ Nothing
      exported <- getExportedSpan recorder
      readStatus exported @?= SpanStatus Error (Just "something failed")

  , testCase "updateName: name changes" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "original" root defaultSpanConfig
      updateName span_ "renamed"
      end span_ Nothing
      exported <- getExportedSpan recorder
      readName exported @?= "renamed"

  , testCase "end: span is exported to the processor/exporter" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "exported" root defaultSpanConfig
      -- Before end: nothing exported
      spansBefore <- getAllExportedSpans recorder
      length spansBefore @?= 0
      end span_ Nothing
      spansAfter <- getAllExportedSpans recorder
      length spansAfter @?= 1

  , testCase "after end: isRecording returns False" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "ended" root defaultSpanConfig
      end span_ Nothing
      recording <- isRecording span_
      assertBool "span should not be recording after end" (not recording)

  , testCase "after end: setAttribute is silently ignored" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "ended-attr" root defaultSpanConfig
      setAttribute span_ "before" (StringValue "yes")
      end span_ Nothing
      -- setAttribute after end should be silently ignored
      setAttribute span_ "after" (StringValue "no")
      exported <- getExportedSpan recorder
      -- The exported snapshot was taken at end time, so "after" was never added
      readAttributes exported @?= fromList [("before", StringValue "yes")]

  , testCase "double end: only first end exports the span" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "double-end" root defaultSpanConfig
      end span_ Nothing
      end span_ Nothing
      spans <- getAllExportedSpans recorder
      length spans @?= 1
  ]


-------------------------------------------------------------------------------
-- SpanLimits enforcement
-------------------------------------------------------------------------------

sdkSpanLimitsTests :: TestTree
sdkSpanLimitsTests = testGroup "SDK SpanLimits enforcement"
  [ testCase "maxAttributes: excess attributes are dropped" $ do
      let limits = defaultSpanLimits { maxAttributes = 2 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "limited-attrs" root defaultSpanConfig
      setAttribute span_ "a" (StringValue "1")
      setAttribute span_ "b" (StringValue "2")
      setAttribute span_ "c" (StringValue "3")  -- should be dropped
      end span_ Nothing
      exported <- getExportedSpan recorder
      readDroppedAttributesCount exported @?= 1

  , testCase "maxEvents: excess events are dropped" $ do
      let limits = defaultSpanLimits { maxEvents = 1 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "limited-events" root defaultSpanConfig
      addEvent span_ "e1" emptyAttributes (Just (fromNanos 1))
      addEvent span_ "e2" emptyAttributes (Just (fromNanos 2))  -- dropped
      addEvent span_ "e3" emptyAttributes (Just (fromNanos 3))  -- dropped
      end span_ Nothing
      exported <- getExportedSpan recorder
      length (readEvents exported) @?= 1
      readDroppedEventsCount exported @?= 2

  , testCase "maxLinks: excess links are dropped" $ do
      let limits = defaultSpanLimits { maxLinks = 1 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "limited-links" root defaultSpanConfig
      addLink span_ invalidSpanContext emptyAttributes
      addLink span_ invalidSpanContext emptyAttributes  -- dropped
      end span_ Nothing
      exported <- getExportedSpan recorder
      length (readLinks exported) @?= 1
      readDroppedLinksCount exported @?= 1

    -- maxAttributesPerEvent enforcement
  , testCase "maxAttributesPerEvent: excess attributes are truncated" $ do
      let limits = defaultSpanLimits { maxAttributesPerEvent = 2 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "event-attr-limit" root defaultSpanConfig
      let manyAttrs = fromList [("a", StringValue "1"), ("b", StringValue "2"), ("c", StringValue "3"), ("d", StringValue "4"), ("e", StringValue "5")]
      addEvent span_ "evt" manyAttrs (Just (fromNanos 1))
      end span_ Nothing
      exported <- getExportedSpan recorder
      let events = readEvents exported
      length events @?= 1
      case events of
        [evt] -> size (eventAttributes evt) @?= 2
        _ -> assertFailure "expected exactly one event"

  , testCase "maxAttributesPerEvent: eventDroppedAttributesCount equals excess" $ do
      let limits = defaultSpanLimits { maxAttributesPerEvent = 2 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "event-drop-count" root defaultSpanConfig
      let manyAttrs = fromList [("a", StringValue "1"), ("b", StringValue "2"), ("c", StringValue "3"), ("d", StringValue "4"), ("e", StringValue "5")]
      addEvent span_ "evt" manyAttrs (Just (fromNanos 1))
      end span_ Nothing
      exported <- getExportedSpan recorder
      case readEvents exported of
        [evt] -> eventDroppedAttributesCount evt @?= 3
        _ -> assertFailure "expected exactly one event"

  , testCase "maxAttributesPerEvent: fewer than limit preserves all" $ do
      let limits = defaultSpanLimits { maxAttributesPerEvent = 10 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "event-under-limit" root defaultSpanConfig
      let attrs = fromList [("x", StringValue "1"), ("y", StringValue "2")]
      addEvent span_ "evt" attrs (Just (fromNanos 1))
      end span_ Nothing
      exported <- getExportedSpan recorder
      case readEvents exported of
        [evt] -> do
          size (eventAttributes evt) @?= 2
          eventDroppedAttributesCount evt @?= 0
        _ -> assertFailure "expected exactly one event"

    -- maxAttributesPerLink enforcement
  , testCase "maxAttributesPerLink: excess attributes are truncated" $ do
      let limits = defaultSpanLimits { maxAttributesPerLink = 2 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "link-attr-limit" root defaultSpanConfig
      let manyAttrs = fromList [("a", StringValue "1"), ("b", StringValue "2"), ("c", StringValue "3"), ("d", StringValue "4"), ("e", StringValue "5")]
      addLink span_ invalidSpanContext manyAttrs
      end span_ Nothing
      exported <- getExportedSpan recorder
      let links = readLinks exported
      length links @?= 1
      case links of
        [lnk] -> size (linkAttributes lnk) @?= 2
        _ -> assertFailure "expected exactly one link"

  , testCase "maxAttributesPerLink: linkDroppedAttributesCount equals excess" $ do
      let limits = defaultSpanLimits { maxAttributesPerLink = 2 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "link-drop-count" root defaultSpanConfig
      let manyAttrs = fromList [("a", StringValue "1"), ("b", StringValue "2"), ("c", StringValue "3"), ("d", StringValue "4"), ("e", StringValue "5")]
      addLink span_ invalidSpanContext manyAttrs
      end span_ Nothing
      exported <- getExportedSpan recorder
      case readLinks exported of
        [lnk] -> linkDroppedAttributesCount lnk @?= 3
        _ -> assertFailure "expected exactly one link"

  , testCase "maxAttributesPerLink: fewer than limit preserves all" $ do
      let limits = defaultSpanLimits { maxAttributesPerLink = 10 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "link-under-limit" root defaultSpanConfig
      let attrs = fromList [("x", StringValue "1"), ("y", StringValue "2")]
      addLink span_ invalidSpanContext attrs
      end span_ Nothing
      exported <- getExportedSpan recorder
      case readLinks exported of
        [lnk] -> do
          size (linkAttributes lnk) @?= 2
          linkDroppedAttributesCount lnk @?= 0
        _ -> assertFailure "expected exactly one link"

    -- maxAttributeValueLength enforcement
  , testCase "maxAttributeValueLength: StringValue truncated to limit" $ do
      let limits = defaultSpanLimits { maxAttributeValueLength = Just 5 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "val-len-string" root defaultSpanConfig
      setAttribute span_ "key" (StringValue "abcdefghij")
      end span_ Nothing
      exported <- getExportedSpan recorder
      lookup "key" (readAttributes exported) @?= Just (StringValue "abcde")

  , testCase "maxAttributeValueLength: StringArrayValue elements truncated" $ do
      let limits = defaultSpanLimits { maxAttributeValueLength = Just 3 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "val-len-array" root defaultSpanConfig
      let val = StringArrayValue (Vector.fromList ["hello", "ab", "world"])
      setAttribute span_ "key" val
      end span_ Nothing
      exported <- getExportedSpan recorder
      lookup "key" (readAttributes exported) @?= Just (StringArrayValue (Vector.fromList ["hel", "ab", "wor"]))

  , testCase "maxAttributeValueLength: non-string values unchanged" $ do
      let limits = defaultSpanLimits { maxAttributeValueLength = Just 1 }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "val-len-nonstring" root defaultSpanConfig
      setAttribute span_ "int" (Int64Value 999999)
      setAttribute span_ "bool" (BoolValue True)
      setAttribute span_ "float" (Float64Value 3.14159)
      end span_ Nothing
      exported <- getExportedSpan recorder
      lookup "int" (readAttributes exported) @?= Just (Int64Value 999999)
      lookup "bool" (readAttributes exported) @?= Just (BoolValue True)
      lookup "float" (readAttributes exported) @?= Just (Float64Value 3.14159)

  , testCase "maxAttributeValueLength Nothing: no truncation occurs" $ do
      let limits = defaultSpanLimits { maxAttributeValueLength = Nothing }
      (provider, recorder) <- newTestProviderWith
        defaultSdkTracerProviderConfig { providerSpanLimits = limits }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "val-len-nothing" root defaultSpanConfig
      let longStr = Text.replicate 1000 "x"
      setAttribute span_ "key" (StringValue longStr)
      end span_ Nothing
      exported <- getExportedSpan recorder
      lookup "key" (readAttributes exported) @?= Just (StringValue longStr)
  ]


-------------------------------------------------------------------------------
-- Status override rules (OTel spec)
-------------------------------------------------------------------------------

sdkStatusOverrideTests :: TestTree
sdkStatusOverrideTests = testGroup "SDK status override rules"
  [ testCase "setStatus Error then Ok: final status is Ok" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "err-then-ok" root defaultSpanConfig
      setStatus span_ Error (Just "bad")
      setStatus span_ Ok Nothing
      end span_ Nothing
      exported <- getExportedSpan recorder
      statusCode (readStatus exported) @?= Ok

  , testCase "setStatus Ok then Error: final status is Ok (Ok is terminal)" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "ok-then-err" root defaultSpanConfig
      setStatus span_ Ok Nothing
      setStatus span_ Error (Just "should be ignored")
      end span_ Nothing
      exported <- getExportedSpan recorder
      statusCode (readStatus exported) @?= Ok

  , testCase "setStatus Error with description: description preserved" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "err-desc" root defaultSpanConfig
      setStatus span_ Error (Just "detailed error message")
      end span_ Nothing
      exported <- getExportedSpan recorder
      readStatus exported @?= SpanStatus Error (Just "detailed error message")

  , testCase "setStatus Ok with description: description discarded" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "ok-desc" root defaultSpanConfig
      setStatus span_ Ok (Just "this should be dropped")
      end span_ Nothing
      exported <- getExportedSpan recorder
      readStatus exported @?= SpanStatus Ok Nothing

  , testCase "setStatus Unset never overwrites Error" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "unset-over-error" root defaultSpanConfig
      setStatus span_ Error (Just "something broke")
      setStatus span_ Unset Nothing
      end span_ Nothing
      exported <- getExportedSpan recorder
      statusCode (readStatus exported) @?= Error

  , testCase "setStatus Unset never overwrites Ok" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "unset-over-ok" root defaultSpanConfig
      setStatus span_ Ok Nothing
      setStatus span_ Unset Nothing
      end span_ Nothing
      exported <- getExportedSpan recorder
      statusCode (readStatus exported) @?= Ok
  ]


-------------------------------------------------------------------------------
-- Parent-child context
-------------------------------------------------------------------------------

sdkParentChildTests :: TestTree
sdkParentChildTests = testGroup "SDK parent-child context"
  [ testCase "child span inherits parent's traceId" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      parentSpan <- startSpan tracer "parent" root defaultSpanConfig
      parentCtx <- getSpanContext parentSpan
      let childContext = setSpanInContext parentSpan root
      childSpan <- startSpan tracer "child" childContext defaultSpanConfig
      childCtx <- getSpanContext childSpan
      traceId childCtx @?= traceId parentCtx
      assertBool "child has different spanId" (spanId childCtx /= spanId parentCtx)

  , testCase "span with spanNoParent=True has a unique traceId (root span)" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      parentSpan <- startSpan tracer "parent" root defaultSpanConfig
      parentCtx <- getSpanContext parentSpan
      let parentContext = setSpanInContext parentSpan root
          noParentConfig = defaultSpanConfig { spanNoParent = True }
      rootSpan <- startSpan tracer "root-child" parentContext noParentConfig
      rootCtx <- getSpanContext rootSpan
      assertBool "root span should have different traceId"
        (traceId rootCtx /= traceId parentCtx)
  ]


-------------------------------------------------------------------------------
-- Processor notification
-------------------------------------------------------------------------------

sdkProcessorNotificationTests :: TestTree
sdkProcessorNotificationTests = testGroup "SDK processor notification"
  [ testCase "onStart is called on all processors when span starts" $ do
      rp1 <- newRecordingProcessor
      rp2 <- newRecordingProcessor
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors =
            [ SomeSpanProcessor rp1
            , SomeSpanProcessor rp2
            ]
        }
      tracer <- getTracer provider testScope
      _span <- startSpan tracer "notify-start" root defaultSpanConfig
      starts1 <- readTVarIO (rpOnStartCalls rp1)
      starts2 <- readTVarIO (rpOnStartCalls rp2)
      length starts1 @?= 1
      length starts2 @?= 1

  , testCase "onEnd is called on all processors when span ends" $ do
      rp1 <- newRecordingProcessor
      rp2 <- newRecordingProcessor
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors =
            [ SomeSpanProcessor rp1
            , SomeSpanProcessor rp2
            ]
        }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "notify-end" root defaultSpanConfig
      end span_ Nothing
      ends1 <- readTVarIO (rpOnEndCalls rp1)
      ends2 <- readTVarIO (rpOnEndCalls rp2)
      length ends1 @?= 1
      length ends2 @?= 1

  , testCase "processors receive span data (via RecordingExporter)" $ do
      (provider, recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "processor-data" root defaultSpanConfig
      setAttribute span_ "pkey" (StringValue "pval")
      end span_ Nothing
      exported <- getExportedSpan recorder
      readName exported @?= "processor-data"
      readAttributes exported @?= fromList [("pkey", StringValue "pval")]
  ]


-------------------------------------------------------------------------------
-- Provider lifecycle
-------------------------------------------------------------------------------

sdkProviderLifecycleTests :: TestTree
sdkProviderLifecycleTests = testGroup "SDK provider lifecycle"
  [ testCase "after shutdown, startSpan returns a NoOp span" $ do
      (provider, _recorder) <- newTestProvider
      tracer <- getTracer provider testScope
      _ <- shutdown provider
      span_ <- startSpan tracer "after-shutdown" root defaultSpanConfig
      recording <- isRecording span_
      assertBool "span after shutdown should not be recording" (not recording)
      ctx <- getSpanContext span_
      -- NoOp span returns invalidSpanContext
      ctx @?= invalidSpanContext

  , testCase "shutdown is idempotent" $ do
      (provider, _recorder) <- newTestProvider
      result1 <- shutdown provider
      assertRight result1
      result2 <- shutdown provider
      assertRight result2

  , testCase "forceFlush delegates to processors" $ do
      rp <- newRecordingProcessor
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor rp] }
      result <- forceFlush provider Nothing
      assertRight result
      flushed <- readTVarIO (rpFlushCalled rp)
      assertBool "forceFlush should have been called on processor" flushed
  ]


-------------------------------------------------------------------------------
-- Sampling (AlwaysOnSampler)
-------------------------------------------------------------------------------

sdkSamplingTests :: TestTree
sdkSamplingTests = testGroup "SDK sampling"
  [ testCase "AlwaysOnSampler: all spans are recorded and sampled" $ do
      (provider, recorder) <- newTestProvider  -- uses AlwaysOnSampler by default
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "sampled" root defaultSpanConfig
      ctx <- getSpanContext span_
      assertBool "sampled flag should be set" (isSampled (traceFlags ctx))
      recording <- isRecording span_
      assertBool "span should be recording" recording
      end span_ Nothing
      exported <- getExportedSpan recorder
      let exportedCtx = readSpanContext exported
      assertBool "exported span should have sampled flag"
        (isSampled (traceFlags exportedCtx))

  , testCase "defaultSdkTracerProviderConfig uses parentbased_always_on: root span records, non-sampled-parent child does not" $ do
      -- Use defaultSdkTracerProviderConfig with NO explicit sampler override.
      -- The spec mandates parentbased_always_on as the default sampler.
      recorder <- newRecordingExporter
      proc <- newSimpleSpanProcessor (SomeSpanExporter recorder)
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor proc] }
      tracer <- getTracer provider testScope

      -- 1) Root span (no parent context): should record, because the root
      --    delegate is AlwaysOnSampler.
      rootSpan <- startSpan tracer "root-span" root defaultSpanConfig
      rootRecording <- isRecording rootSpan
      assertBool "root span should be recording (parentbased delegates to AlwaysOn for roots)"
        rootRecording
      end rootSpan Nothing

      -- 2) Child span whose parent is a non-sampled remote span: should NOT
      --    record, because remoteParentNotSampled defaults to AlwaysOffSampler.
      --    This is the key difference from a pure AlwaysOnSampler which would
      --    record everything regardless of parent sampling state.
      let nonSampledParentSc = invalidSpanContext
            { traceId = mkTraceId 99
            , spanId = mkSpanId_ 7
            , traceFlags = emptyTraceFlags  -- not sampled
            , _isRemote = True
            }
          parentCtx = setSpanInContext (createNonRecordingSpan nonSampledParentSc) root
      childSpan <- startSpan tracer "child-of-unsampled" parentCtx defaultSpanConfig
      childRecording <- isRecording childSpan
      assertBool "child of non-sampled remote parent should NOT be recording"
        (not childRecording)
      end childSpan Nothing

      -- Cleanup
      _ <- shutdown provider
      pure ()
  ]


-------------------------------------------------------------------------------
-- Sampler property-based tests
-------------------------------------------------------------------------------

-- | Convert a Word64 to 8 big-endian bytes.
word64ToBytes :: Word64 -> [Word8]
word64ToBytes w =
  [ fromIntegral (w `div` (256^i) `mod` 256)
  | i <- [7 :: Int, 6, 5, 4, 3, 2, 1, 0]
  ]

-- | Build a TraceId from a Word64 (zero-padded in the upper 8 bytes).
mkTraceId :: Word64 -> TraceId
mkTraceId w = traceIdFromBytes (BS.pack (replicate 8 0 <> word64ToBytes w))

-- | Build a SpanId from a Word64.
mkSpanId_ :: Word64 -> SpanId
mkSpanId_ w = spanIdFromBytes (BS.pack (word64ToBytes w))

-- | Generator for arbitrary TraceIds.
genTraceId :: Gen TraceId
genTraceId = traceIdFromBytes . BS.pack <$> vectorOf 16 (arbitrary :: Gen Word8)

-- | Generator for non-zero TraceId byte strings (16 bytes, not all zero).
genNonZeroTraceIdBytes :: Gen BS.ByteString
genNonZeroTraceIdBytes = do
  bytes <- vectorOf 16 (arbitrary :: Gen Word8)
  if all (== 0) bytes
    then pure (BS.pack (0 : replicate 14 0 <> [1]))
    else pure (BS.pack bytes)


samplerTests :: TestTree
samplerTests = testGroup "Sampler"
  [ alwaysOnSamplerTests
  , alwaysOffSamplerTests
  , traceIdRatioBasedSamplerTests
  , parentBasedSamplerTests
  ]


-------------------------------------------------------------------------------
-- AlwaysOnSampler
-------------------------------------------------------------------------------

alwaysOnSamplerTests :: TestTree
alwaysOnSamplerTests = testGroup "AlwaysOnSampler"
  [ testProperty "always returns RecordAndSample" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample AlwaysOnSampler root tid "op" Internal emptyAttributes []
          pure (samplingDecision result === RecordAndSample)

  , testProperty "always returns emptyAttributes" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample AlwaysOnSampler root tid "op" Internal emptyAttributes []
          pure (samplingAttributes result === emptyAttributes)

  , testCase "description is \"AlwaysOnSampler\"" $
      samplerDescription AlwaysOnSampler @?= "AlwaysOnSampler"
  ]


-------------------------------------------------------------------------------
-- AlwaysOffSampler
-------------------------------------------------------------------------------

alwaysOffSamplerTests :: TestTree
alwaysOffSamplerTests = testGroup "AlwaysOffSampler"
  [ testProperty "always returns Drop" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample AlwaysOffSampler root tid "op" Internal emptyAttributes []
          pure (samplingDecision result === Drop)

  , testProperty "always returns emptyAttributes" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample AlwaysOffSampler root tid "op" Internal emptyAttributes []
          pure (samplingAttributes result === emptyAttributes)

  , testCase "description is \"AlwaysOffSampler\"" $
      samplerDescription AlwaysOffSampler @?= "AlwaysOffSampler"
  ]


-------------------------------------------------------------------------------
-- TraceIdRatioBasedSampler
-------------------------------------------------------------------------------

traceIdRatioBasedSamplerTests :: TestTree
traceIdRatioBasedSamplerTests = testGroup "TraceIdRatioBasedSampler"
  [ testProperty "ratio 0.0 always Drop" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample (TraceIdRatioBasedSampler 0.0) root tid "op" Internal emptyAttributes []
          pure (samplingDecision result === Drop)

  , testProperty "ratio 1.0 always RecordAndSample" $
      forAll genTraceId $ \tid ->
        ioProperty $ do
          result <- shouldSample (TraceIdRatioBasedSampler 1.0) root tid "op" Internal emptyAttributes []
          pure (samplingDecision result === RecordAndSample)

  , testProperty "deterministic: same traceId gives same decision" $
      forAll (chooseInt (1, 99)) $ \pct ->
        forAll genNonZeroTraceIdBytes $ \tidBytes ->
          let ratio = fromIntegral pct / 100.0 :: Double
              tid = traceIdFromBytes tidBytes
          in ioProperty $ do
               r1 <- shouldSample (TraceIdRatioBasedSampler ratio) root tid "op" Internal emptyAttributes []
               r2 <- shouldSample (TraceIdRatioBasedSampler ratio) root tid "op" Internal emptyAttributes []
               pure (samplingDecision r1 === samplingDecision r2)

  , testProperty "monotonicity: higher ratio samples at least as much" $
      forAll genNonZeroTraceIdBytes $ \tidBytes ->
        forAll (chooseInt (1, 98)) $ \pct ->
          let lo = fromIntegral pct / 100.0 :: Double
              hi = fromIntegral (pct + 1) / 100.0 :: Double
              tid = traceIdFromBytes tidBytes
          in ioProperty $ do
               rLo <- shouldSample (TraceIdRatioBasedSampler lo) root tid "op" Internal emptyAttributes []
               rHi <- shouldSample (TraceIdRatioBasedSampler hi) root tid "op" Internal emptyAttributes []
               -- If sampled at lower ratio, must be sampled at higher ratio
               pure $ case samplingDecision rLo of
                 RecordAndSample -> samplingDecision rHi === RecordAndSample
                 _ -> property True

  , testCase "ratio 0.5 samples approximately half (statistical)" $ do
      -- Spread trace IDs evenly across the Word64 space so ~half fall
      -- below the 50% bound. stride = maxBound / n distributes uniformly.
      let n = 1000 :: Int
          stride = maxBound `div` fromIntegral n :: Word64
          tids = [ traceIdFromBytes (BS.pack (replicate 8 0 <> word64ToBytes (fromIntegral i * stride)))
                 | i <- [1..n]
                 ]
      results <- mapM (\tid -> samplingDecision <$> shouldSample (TraceIdRatioBasedSampler 0.5) root tid "op" Internal emptyAttributes []) tids
      let sampledCount = length (filter (== RecordAndSample) results)
      assertBool ("Expected ~500 sampled, got " <> show sampledCount)
        (sampledCount >= 300 && sampledCount <= 700)
  ]


-------------------------------------------------------------------------------
-- ParentBasedSampler
-------------------------------------------------------------------------------

parentBasedSamplerTests :: TestTree
parentBasedSamplerTests = testGroup "ParentBasedSampler"
  [ testCase "no parent -> uses root delegate (AlwaysOff)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOffSampler)
      result <- shouldSample sampler root (mkTraceId 1) "op" Internal emptyAttributes []
      samplingDecision result @?= Drop

  , testCase "no parent -> uses root delegate (AlwaysOn)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOnSampler)
      result <- shouldSample sampler root (mkTraceId 1) "op" Internal emptyAttributes []
      samplingDecision result @?= RecordAndSample

  , testCase "remote sampled parent -> remoteParentSampled delegate (default: AlwaysOn)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOffSampler)
          parentSc = invalidSpanContext
            { traceId = mkTraceId 42
            , spanId = mkSpanId_ 1
            , traceFlags = sampledFlag
            , _isRemote = True
            }
          parentCtx = setSpanInContext (createNonRecordingSpan parentSc) root
      result <- shouldSample sampler parentCtx (mkTraceId 42) "op" Internal emptyAttributes []
      samplingDecision result @?= RecordAndSample

  , testCase "remote unsampled parent -> remoteParentNotSampled delegate (default: AlwaysOff)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOnSampler)
          parentSc = invalidSpanContext
            { traceId = mkTraceId 42
            , spanId = mkSpanId_ 1
            , traceFlags = emptyTraceFlags
            , _isRemote = True
            }
          parentCtx = setSpanInContext (createNonRecordingSpan parentSc) root
      result <- shouldSample sampler parentCtx (mkTraceId 42) "op" Internal emptyAttributes []
      samplingDecision result @?= Drop

  , testCase "local sampled parent -> localParentSampled delegate (default: AlwaysOn)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOffSampler)
          parentSc = invalidSpanContext
            { traceId = mkTraceId 42
            , spanId = mkSpanId_ 1
            , traceFlags = sampledFlag
            , _isRemote = False
            }
          parentCtx = setSpanInContext (createNonRecordingSpan parentSc) root
      result <- shouldSample sampler parentCtx (mkTraceId 42) "op" Internal emptyAttributes []
      samplingDecision result @?= RecordAndSample

  , testCase "local unsampled parent -> localParentNotSampled delegate (default: AlwaysOff)" $ do
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOnSampler)
          parentSc = invalidSpanContext
            { traceId = mkTraceId 42
            , spanId = mkSpanId_ 1
            , traceFlags = emptyTraceFlags
            , _isRemote = False
            }
          parentCtx = setSpanInContext (createNonRecordingSpan parentSc) root
      result <- shouldSample sampler parentCtx (mkTraceId 42) "op" Internal emptyAttributes []
      samplingDecision result @?= Drop

  , testProperty "no parent in context -> always delegates to root" $
      forAll genTraceId $ \tid ->
        forAll (elements [True, False]) $ \useAlwaysOn ->
          let rootSampler = if useAlwaysOn
                then SomeSampler AlwaysOnSampler
                else SomeSampler AlwaysOffSampler
              sampler = defaultParentBasedSampler rootSampler
              expected = if useAlwaysOn then RecordAndSample else Drop
          in ioProperty $ do
               result <- shouldSample sampler root tid "op" Internal emptyAttributes []
               pure (samplingDecision result === expected)

  , testCase "description includes root sampler" $
      let sampler = defaultParentBasedSampler (SomeSampler AlwaysOnSampler)
      in samplerDescription sampler @?= "ParentBased{root=AlwaysOnSampler}"
  ]


-------------------------------------------------------------------------------
-- BatchSpanProcessor
-------------------------------------------------------------------------------

-- | Fast configuration for batch processor tests. Short delays, small queues.
fastBatchConfig :: BatchSpanProcessorConfig
fastBatchConfig = defaultBatchSpanProcessorConfig
  { bspScheduledDelay     = milliseconds 10
  , bspMaxQueueSize       = 20
  , bspMaxExportBatchSize = 5
  }


-- | Create a provider wired to a RecordingExporter via a BatchSpanProcessor.
-- Returns the provider, the batch processor, and the recorder.
newBatchTestProvider
  :: BatchSpanProcessorConfig
  -> IO (SdkTracerProvider, BatchSpanProcessor, RecordingExporter)
newBatchTestProvider cfg = do
  recorder <- newRecordingExporter
  bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
  provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
    { providerProcessors = [SomeSpanProcessor bsp] }
  pure (provider, bsp, recorder)


-- | Run @n@ spans through a provider (start + end each).
runSpansThrough :: SdkTracerProvider -> Int -> IO ()
runSpansThrough provider n = do
  tracer <- getTracer provider testScope
  replicateM_ n $ do
    span_ <- startSpan tracer "batch-test" root defaultSpanConfig
    end span_ Nothing


-- | Count all exported spans across all batches in a RecordingExporter.
countExported :: RecordingExporter -> IO Int
countExported recorder = do
  batches <- readTVarIO (exportedBatches recorder)
  pure (sum (map length batches))


batchSpanProcessorTests :: TestTree
batchSpanProcessorTests = testGroup "BatchSpanProcessor"
  [ batchSpanProcessorDefaultTests
  , batchSpanProcessorBehaviourTests
  , batchSpanProcessorFlushTimeoutTests
  , batchSpanProcessorExportTimeoutTests
  ]


batchSpanProcessorDefaultTests :: TestTree
batchSpanProcessorDefaultTests = testGroup "BatchSpanProcessor defaults"
  [ testCase "defaultBatchSpanProcessorConfig values match spec" $ do
      defaultBatchSpanProcessorConfig.bspScheduledDelay     @?= milliseconds 5000
      defaultBatchSpanProcessorConfig.bspExportTimeout      @?= milliseconds 30000
      defaultBatchSpanProcessorConfig.bspMaxQueueSize       @?= 2048
      defaultBatchSpanProcessorConfig.bspMaxExportBatchSize @?= 512
  ]


batchSpanProcessorBehaviourTests :: TestTree
batchSpanProcessorBehaviourTests = localOption (mkTimeout 5_000_000) $
  testGroup "BatchSpanProcessor behaviour"
  [ testCase "shutdown drains all queued spans" $ do
      -- Use onEnd directly with TestSpan to avoid provider complexity
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig { bspScheduledDelay = milliseconds 5000 }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      replicateM_ 5 $ onEnd bsp (SomeReadableSpan TestSpan)
      _ <- shutdownProcessor bsp
      exported <- countExported recorder
      exported @?= 5

  , testCase "forceFlush triggers immediate export" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig { bspScheduledDelay = milliseconds 5000 }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      replicateM_ 3 $ onEnd bsp (SomeReadableSpan TestSpan)
      _ <- forceFlushProcessor bsp Nothing
      exported <- countExported recorder
      exported @?= 3
      _ <- shutdownProcessor bsp
      pure ()

  , testCase "exports up to maxExportBatchSize per cycle" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig
            { bspMaxExportBatchSize = 3
            , bspScheduledDelay = milliseconds 50
            }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      replicateM_ 3 $ onEnd bsp (SomeReadableSpan TestSpan)
      -- Wait long enough for the scheduled export to fire
      threadDelay 200_000
      _ <- shutdownProcessor bsp
      exported <- countExported recorder
      exported @?= 3

  , testCase "periodic export fires after scheduledDelay" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig { bspScheduledDelay = milliseconds 20 }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      onEnd bsp (SomeReadableSpan TestSpan)
      -- Wait 200ms, well past the 20ms delay
      threadDelay 200_000
      exported <- countExported recorder
      assertBool ("expected at least 1 span exported by timer, got " <> show exported)
        (exported >= 1)
      _ <- shutdownProcessor bsp
      pure ()

  , testCase "queue-full spans are dropped silently" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig
            { bspMaxQueueSize = 3
            , bspScheduledDelay = milliseconds 5000
            }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      replicateM_ 5 $ onEnd bsp (SomeReadableSpan TestSpan)
      -- Use shutdown to drain — avoids forceFlush timing issues
      _ <- shutdownProcessor bsp
      exported <- countExported recorder
      assertBool ("expected <= 3 spans, got " <> show exported)
        (exported <= 3)

  , testCase "concurrent onEnd does not corrupt queue" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig
            { bspMaxQueueSize       = 100
            , bspMaxExportBatchSize = 100
            , bspScheduledDelay     = milliseconds 5000
            }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      let nThreads = 10 :: Int
          spansPerThread = 5 :: Int
      done <- newEmptyMVar
      replicateM_ nThreads $ forkIO $ do
        replicateM_ spansPerThread $ onEnd bsp (SomeReadableSpan TestSpan)
        putMVar done ()
      replicateM_ nThreads (takeMVar done)
      -- Use shutdown to drain all spans
      _ <- shutdownProcessor bsp
      exported <- countExported recorder
      assertBool ("expected 50 spans, got " <> show exported)
        (exported == nThreads * spansPerThread)

  , testCase "shutdownProcessor is idempotent" $ do
      recorder <- newRecordingExporter
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) fastBatchConfig
      r1 <- shutdownProcessor bsp
      r2 <- shutdownProcessor bsp
      assertRight r1
      assertRight r2

  , testCase "end-to-end: spans flow through provider to batch exporter" $ do
      (provider, bsp, recorder) <- newBatchTestProvider
        fastBatchConfig { bspScheduledDelay = milliseconds 5000 }
      runSpansThrough provider 3
      _ <- shutdownProcessor bsp
      exported <- countExported recorder
      exported @?= 3
  ]


-------------------------------------------------------------------------------
-- BatchSpanProcessor forceFlush timeout
-------------------------------------------------------------------------------

batchSpanProcessorFlushTimeoutTests :: TestTree
batchSpanProcessorFlushTimeoutTests = localOption (mkTimeout 10_000_000) $
  testGroup "forceFlush timeout"
  [ testProperty "forceFlush Nothing completes successfully" $
      once $ ioProperty $ do
        recorder <- newRecordingExporter
        let cfg = fastBatchConfig { bspScheduledDelay = milliseconds 5000 }
        bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
        result <- forceFlushProcessor bsp Nothing
        _ <- shutdownProcessor bsp
        pure $ case result of
          Right () -> property True
          Left fe  -> counterexample ("unexpected FlushError: " ++ show fe) False

  , testCase "forceFlush with generous timeout completes" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig { bspScheduledDelay = milliseconds 5000 }
      bsp <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      result <- forceFlushProcessor bsp (Just (milliseconds 500))
      assertRight result
      _ <- shutdownProcessor bsp
      pure ()

  , testCase "forceFlush times out when exporter is slow" $ do
      let slowExporter = SomeSpanExporter SlowExporter
      bsp <- newBatchSpanProcessor slowExporter
               (fastBatchConfig { bspScheduledDelay = milliseconds 5 })
      -- Enqueue a span so export is actually invoked
      onEnd bsp (SomeReadableSpan TestSpan)
      threadDelay 20_000  -- let worker start and pick up the span
      result <- forceFlushProcessor bsp (Just (milliseconds 1))
      case result of
        Left fe -> flushTimedOut fe @?= True
        Right () -> assertFailure "expected timeout, got success"
      _ <- shutdownProcessor bsp
      pure ()

  , testProperty "timedOut FlushError has flushTimedOut = True" $
      once $ ioProperty $ do
        let slowExporter = SomeSpanExporter SlowExporter
        bsp <- newBatchSpanProcessor slowExporter
                 (fastBatchConfig { bspScheduledDelay = milliseconds 5 })
        onEnd bsp (SomeReadableSpan TestSpan)
        threadDelay 20_000
        result <- forceFlushProcessor bsp (Just (milliseconds 1))
        _ <- shutdownProcessor bsp
        pure $ case result of
          Left fe -> flushTimedOut fe === True
          Right () -> counterexample "expected timeout but got Right ()" False

  , testCase "forceFlush Nothing and Just both succeed with fast worker" $ do
      recorder <- newRecordingExporter
      let cfg = fastBatchConfig
      bsp1 <- newBatchSpanProcessor (SomeSpanExporter recorder) cfg
      r1 <- forceFlushProcessor bsp1 Nothing
      _ <- shutdownProcessor bsp1

      recorder2 <- newRecordingExporter
      bsp2 <- newBatchSpanProcessor (SomeSpanExporter recorder2) cfg
      r2 <- forceFlushProcessor bsp2 (Just (milliseconds 500))
      _ <- shutdownProcessor bsp2

      assertRight r1
      assertRight r2
  ]


-------------------------------------------------------------------------------
-- BatchSpanProcessor export timeout
-------------------------------------------------------------------------------

batchSpanProcessorExportTimeoutTests :: TestTree
batchSpanProcessorExportTimeoutTests = localOption (mkTimeout 5_000_000) $
  testGroup "export timeout"
  [ testCase "BatchSpanProcessor respects bspExportTimeout: slow exporter does not block indefinitely" $ do
      (slowExp, completedRef) <- newVerySlowExporter
      let cfg = fastBatchConfig
            { bspExportTimeout  = milliseconds 100
            , bspScheduledDelay = milliseconds 50
            }
      bsp <- newBatchSpanProcessor (SomeSpanExporter slowExp) cfg
      -- Wire through a real provider so the span flows through the full pipeline
      provider <- newSdkTracerProvider defaultSdkTracerProviderConfig
        { providerProcessors = [SomeSpanProcessor bsp] }
      tracer <- getTracer provider testScope
      span_ <- startSpan tracer "timeout-test" root defaultSpanConfig
      end span_ Nothing
      -- Wait 300ms: the scheduler should have fired (50ms) and the
      -- export timeout (100ms) should have cancelled the 10s export.
      threadDelay 300_000
      completed <- readIORef completedRef
      assertBool
        "slow export should have been cancelled by bspExportTimeout (IORef should be False)"
        (not completed)
      _ <- shutdownProcessor bsp
      pure ()
  ]
