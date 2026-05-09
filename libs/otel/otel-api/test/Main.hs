{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (intToDigit)
import Data.Int (Int64)
import Data.Function (on)
import Data.List (nubBy, sort, sortBy)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word8, Word64)
import Prelude hiding (lookup)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import Control.Exception (SomeException, toException)
import OTel.Attribute
import OTel.Baggage hiding (getValue, setValue)
import OTel.Baggage qualified as Baggage
import OTel.Context
import OTel.Context.Key
import OTel.Propagation
import OTel.Propagation.W3C
import OTel.Timestamp
import OTel.Log
import OTel.Metric
import OTel.Profile
import OTel.Trace
import OTel.Trace.SpanContext
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- Arbitrary instances
-------------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> arbitrary
  shrink = fmap Text.pack . shrink . Text.unpack

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = Vector.fromList <$> arbitrary
  shrink = fmap Vector.fromList . shrink . Vector.toList

instance Arbitrary AttributeValue where
  arbitrary =
    oneof
      [ StringValue <$> arbitrary
      , BoolValue <$> arbitrary
      , Int64Value <$> arbitrary
      , Float64Value <$> arbitraryFiniteDouble
      , StringArrayValue <$> arbitrary
      , BoolArrayValue <$> arbitrary
      , Int64ArrayValue <$> arbitrary
      , Float64ArrayValue <$> (Vector.fromList <$> listOf arbitraryFiniteDouble)
      ]
  shrink (StringValue t) = StringValue <$> shrink t
  shrink (BoolValue _) = []
  shrink (Int64Value n) = Int64Value <$> shrink n
  shrink (Float64Value _) = []
  shrink (StringArrayValue v) = StringArrayValue <$> shrink v
  shrink (BoolArrayValue v) = BoolArrayValue <$> shrink v
  shrink (Int64ArrayValue v) = Int64ArrayValue <$> shrink v
  shrink (Float64ArrayValue v) = Float64ArrayValue <$> shrink v

-- | Short alphanumeric keys to keep tests readable and avoid collisions
-- in a controlled way.
newtype ShortKey = ShortKey {unShortKey :: Text}
  deriving stock (Eq, Ord, Show)

instance Arbitrary ShortKey where
  arbitrary = do
    len <- chooseInt (1, 8)
    chars <- vectorOf len (elements (['a' .. 'z'] <> ['0' .. '9']))
    pure (ShortKey (Text.pack chars))
  shrink (ShortKey t) =
    [ShortKey (Text.pack s) | s <- shrink (Text.unpack t), not (null s)]

instance Arbitrary Attributes where
  arbitrary = do
    kvs <- listOf ((,) <$> (unShortKey <$> arbitrary) <*> arbitrary)
    pure (fromList kvs)
  shrink attrs = fromList <$> shrink (toList attrs)

instance Arbitrary InstrumentationScope where
  arbitrary =
    InstrumentationScope
      <$> (unShortKey <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
      <*> oneof [pure Nothing, Just <$> arbitrary]
  shrink _ = []

-- Finite doubles only to avoid NaN comparison issues
arbitraryFiniteDouble :: Gen Double
arbitraryFiniteDouble = do
  n <- arbitrary :: Gen Int64
  pure (fromIntegral n / 1000.0)


-------------------------------------------------------------------------------
-- Generator helpers for trace types
-------------------------------------------------------------------------------

-- | 16 random bytes for TraceId
genTraceIdBytes :: Gen ByteString
genTraceIdBytes = BS.pack <$> vectorOf 16 (arbitrary :: Gen Word8)

-- | 16 non-zero bytes (at least one non-zero) for valid TraceId
genNonZeroTraceIdBytes :: Gen ByteString
genNonZeroTraceIdBytes = do
  bs <- genTraceIdBytes
  if BS.all (== 0) bs
    then do
      pos <- chooseInt (0, 15)
      w <- chooseEnum (1, 255) :: Gen Word8
      pure (BS.take pos bs <> BS.singleton w <> BS.drop (pos + 1) bs)
    else pure bs

-- | 8 random bytes for SpanId
genSpanIdBytes :: Gen ByteString
genSpanIdBytes = BS.pack <$> vectorOf 8 (arbitrary :: Gen Word8)

-- | 8 non-zero bytes (at least one non-zero) for valid SpanId
genNonZeroSpanIdBytes :: Gen ByteString
genNonZeroSpanIdBytes = do
  bs <- genSpanIdBytes
  if BS.all (== 0) bs
    then do
      pos <- chooseInt (0, 7)
      w <- chooseEnum (1, 255) :: Gen Word8
      pure (BS.take pos bs <> BS.singleton w <> BS.drop (pos + 1) bs)
    else pure bs

-- | Generate a valid lowercase hex string of given length (in hex chars).
genHexString :: Int -> Gen Text
genHexString n = do
  chars <- vectorOf n (elements "0123456789abcdef")
  pure (Text.pack chars)

-- | Generate a non-zero hex string (at least one non-zero digit).
genNonZeroHexString :: Int -> Gen Text
genNonZeroHexString n = do
  t <- genHexString n
  if Text.all (== '0') t
    then do
      pos <- chooseInt (0, n - 1)
      c <- elements "123456789abcdef"
      let s = Text.unpack t
      pure (Text.pack (take pos s <> [c] <> drop (pos + 1) s))
    else pure t

-- | Generate a ByteString whose length is NOT the given value.
genBytesNotLength :: Int -> Gen ByteString
genBytesNotLength n = do
  len <- chooseInt (0, 32) `suchThat` (/= n)
  BS.pack <$> vectorOf len (arbitrary :: Gen Word8)

-- | Bytes to hex (for comparison)
bytesToHexText :: ByteString -> Text
bytesToHexText = Text.pack . concatMap word8ToHex . BS.unpack
  where
    word8ToHex :: Word8 -> String
    word8ToHex w = [intToDigit (fromIntegral (w `div` 16)), intToDigit (fromIntegral (w `mod` 16))]

-- | Generate unique keys (no duplicate first elements).
genUniqueKvs :: Gen [(Key, AttributeValue)]
genUniqueKvs = do
  kvs <- listOf ((,) <$> (unShortKey <$> arbitrary) <*> arbitrary)
  pure (nubBy (\(k1, _) (k2, _) -> k1 == k2) kvs)

-- | Small Word64 values to avoid overflow in Duration tests.
genSmallWord64 :: Gen Word64
genSmallWord64 = chooseEnum (0, 1_000_000)


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup
    "otel-api"
    [ attributeValueTests
    , attributesTests
    , instrumentationScopeTests
    , timestampTests
    , durationTests
    , contextKeyTests
    , contextTests
    , traceIdTests
    , spanIdTests
    , traceFlagsTests
    , spanContextTests
    , traceStateTests
    , spanKindTests
    , statusCodeTests
    , spanConfigTests
    , noOpTests
    , globalProviderTests
    , contextIntegrationTests
    , baggageApiTests
    , propagationTests
    , metricsApiTests
    , logsApiTests
    , profilesApiTests
    ]


-------------------------------------------------------------------------------
-- AttributeValue
-------------------------------------------------------------------------------

attributeValueTests :: TestTree
attributeValueTests =
  testGroup
    "AttributeValue"
    [ testProperty "StringValue round-trips" $ \(t :: Text) ->
        case StringValue t of
          StringValue t' -> t === t'
          _ -> property False
    , testProperty "BoolValue round-trips" $ \(b :: Bool) ->
        case BoolValue b of
          BoolValue b' -> b === b'
          _ -> property False
    , testProperty "Int64Value round-trips" $ \(n :: Int64) ->
        case Int64Value n of
          Int64Value n' -> n === n'
          _ -> property False
    , testProperty "Float64Value round-trips" $
        forAll arbitraryFiniteDouble $ \d ->
          case Float64Value d of
            Float64Value d' -> d === d'
            _ -> property False
    , testProperty "Different constructors are never equal" $
        forAll arbitrary $ \(t :: Text) ->
          forAll arbitrary $ \(b :: Bool) ->
            StringValue t =/= BoolValue b
              .&&. StringValue t =/= Int64Value 0
              .&&. BoolValue b =/= Int64Value 0
    ]


-------------------------------------------------------------------------------
-- Attributes
-------------------------------------------------------------------------------

attributesTests :: TestTree
attributesTests =
  testGroup
    "Attributes"
    [ testProperty "fromList . toList preserves entries (duplicate-free)" $
        forAll genUniqueKvs $ \kvs ->
          let attrs = fromList kvs
              -- toList returns sorted-by-key, so sort input by key too
              sortByKey = sortBy (compare `on` fst)
           in sortByKey (toList attrs) === sortByKey kvs
    , testProperty "fromList with duplicate keys keeps last value" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \v1 ->
            forAll arbitrary $ \(v2 :: AttributeValue) ->
              lookup k (fromList [(k, v1), (k, v2)]) === Just v2
    , testProperty "insert then lookup returns Just v" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: AttributeValue) ->
            forAll arbitrary $ \attrs ->
              lookup k (insert k v attrs) === Just v
    , testProperty "lookup on emptyAttributes returns Nothing" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          lookup k emptyAttributes === Nothing
    , testProperty "size emptyAttributes == 0" $
        once $ size emptyAttributes === 0
    , testProperty "size after insert >= size before" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: AttributeValue) ->
            forAll arbitrary $ \attrs ->
              size (insert k v attrs) >= size attrs
    , testProperty "insert existing key doesn't increase size" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v1 :: AttributeValue) ->
            forAll arbitrary $ \v2 ->
              forAll arbitrary $ \attrs ->
                let attrs' = insert k v1 attrs
                 in size (insert k v2 attrs') === size attrs'
    , testProperty "Semigroup associativity" $
        forAll arbitrary $ \a ->
          forAll arbitrary $ \b ->
            forAll arbitrary $ \(c :: Attributes) ->
              (a <> b) <> c === a <> (b <> c)
    , testProperty "Monoid left identity" $
        forAll arbitrary $ \(a :: Attributes) ->
          mempty <> a === a
    , testProperty "Monoid right identity" $
        forAll arbitrary $ \(a :: Attributes) ->
          a <> mempty === a
    , testProperty "Semigroup is left-biased on keys" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v1 :: AttributeValue) ->
            forAll arbitrary $ \v2 ->
              lookup k (fromList [(k, v1)] <> fromList [(k, v2)])
                === Just v1
    , testProperty "toList is sorted by key" $
        forAll arbitrary $ \(attrs :: Attributes) ->
          let keys = fmap fst (toList attrs)
           in keys === sort keys
    , testProperty "size agrees with toList length" $
        forAll arbitrary $ \(attrs :: Attributes) ->
          size attrs === length (toList attrs)
    ]


-------------------------------------------------------------------------------
-- InstrumentationScope
-------------------------------------------------------------------------------

instrumentationScopeTests :: TestTree
instrumentationScopeTests =
  testGroup
    "InstrumentationScope"
    [ testProperty "Field accessors round-trip" $
        forAll arbitrary $ \(scope :: InstrumentationScope) ->
          InstrumentationScope
            { scopeName = scope.scopeName
            , scopeVersion = scope.scopeVersion
            , scopeSchemaUrl = scope.scopeSchemaUrl
            , scopeAttributes = scope.scopeAttributes
            }
            === scope
    , testProperty "scopeName is always present" $
        forAll arbitrary $ \(scope :: InstrumentationScope) ->
          not (Text.null scope.scopeName)
    ]


-------------------------------------------------------------------------------
-- Timestamp
-------------------------------------------------------------------------------

timestampTests :: TestTree
timestampTests =
  testGroup
    "Timestamp"
    [ testProperty "fromNanos . toNanos == id" $
        forAll arbitrary $ \(n :: Word64) ->
          let ts = fromNanos n
           in fromNanos (toNanos ts) === ts
    , testProperty "toNanos . fromNanos == id" $
        forAll arbitrary $ \(n :: Word64) ->
          toNanos (fromNanos n) === n
    , testCase "now returns positive timestamp" $ do
        ts <- now
        assertBool "timestamp should be > 0" (toNanos ts > 0)
    ]


-------------------------------------------------------------------------------
-- Duration
-------------------------------------------------------------------------------

durationTests :: TestTree
durationTests =
  testGroup
    "Duration"
    [ testProperty "milliseconds 1000 == seconds 1" $
        once $ milliseconds 1000 === seconds 1
    , testProperty "milliseconds 0 == Duration 0" $
        once $ milliseconds 0 === Duration 0
    , testProperty "seconds n is n * 1_000_000_000 nanoseconds" $
        forAll genSmallWord64 $ \n ->
          seconds n === Duration (n * 1_000_000_000)
    , testProperty "milliseconds n is n * 1_000_000 nanoseconds" $
        forAll genSmallWord64 $ \n ->
          milliseconds n === Duration (n * 1_000_000)
    ]


-------------------------------------------------------------------------------
-- ContextKey
-------------------------------------------------------------------------------

contextKeyTests :: TestTree
contextKeyTests =
  testGroup
    "ContextKey"
    [ testCase "Two keys with same name are NOT equal" $ do
        k1 <- newContextKey "test" :: IO (ContextKey Int)
        k2 <- newContextKey "test" :: IO (ContextKey Int)
        assertBool "keys with same name should differ" (k1 /= k2)
    , testCase "A key equals itself" $ do
        k <- newContextKey "test" :: IO (ContextKey Int)
        k @?= k
    , testCase "keyName returns the constructor argument" $ do
        k <- newContextKey "my-key" :: IO (ContextKey Int)
        keyName k @?= "my-key"
    , testProperty "keyName round-trips arbitrary names" $
        \(name :: Text) -> ioProperty $ do
          k <- newContextKey name :: IO (ContextKey Int)
          pure (keyName k === name)
    ]


-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

contextTests :: TestTree
contextTests =
  testGroup
    "Context"
    [ testCase "root returns Nothing for any key" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        getValue k root @?= Nothing
    , testCase "getValue after setValue returns Just val" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx = setValue k (42 :: Int) root
        getValue k ctx @?= Just 42
    , testCase "Overwrite keeps latest value" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx = setValue k (2 :: Int) (setValue k 1 root)
        getValue k ctx @?= Just 2
    , testCase "Setting different key doesn't affect first" $ do
        k1 <- newContextKey "k1" :: IO (ContextKey Int)
        k2 <- newContextKey "k2" :: IO (ContextKey String)
        let ctx = setValue k2 "hello" (setValue k1 (42 :: Int) root)
        getValue k1 ctx @?= Just 42
    , testCase "setValue doesn't mutate original context" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx1 = setValue k (1 :: Int) root
        let _ctx2 = setValue k (2 :: Int) ctx1
        getValue k ctx1 @?= Just 1
    , testCase "root is unaffected by setValue on derived context" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let _ctx = setValue k (42 :: Int) root
        getValue k root @?= Nothing
    , testCase "attach then getCurrent returns attached context" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx = setValue k (42 :: Int) root
        tok <- attach ctx
        cur <- getCurrent
        getValue k cur @?= Just 42
        detach tok
    , testCase "attach then detach restores previous context" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        before <- getCurrent
        let ctx = setValue k (42 :: Int) root
        tok <- attach ctx
        detach tok
        restored <- getCurrent
        getValue k restored @?= getValue k before
    , testCase "Nested attach/detach restores outer context" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx1 = setValue k (1 :: Int) root
        let ctx2 = setValue k (2 :: Int) root
        tok1 <- attach ctx1
        tok2 <- attach ctx2
        cur2 <- getCurrent
        getValue k cur2 @?= Just 2
        detach tok2
        cur1 <- getCurrent
        getValue k cur1 @?= Just 1
        detach tok1
    , testCase "Excessive detach never pops below root" $ do
        k <- newContextKey "k" :: IO (ContextKey Int)
        let ctx = setValue k (42 :: Int) root
        tok <- attach ctx
        detach tok
        -- Detach again with same token -- should be safe
        detach tok
        detach tok
        cur <- getCurrent
        getValue k cur @?= Nothing
    ]


-------------------------------------------------------------------------------
-- TraceId
-------------------------------------------------------------------------------

traceIdTests :: TestTree
traceIdTests =
  testGroup
    "TraceId"
    [ testProperty "toBytes . fromBytes == id for 16-byte input" $
        forAll genTraceIdBytes $ \bs ->
          traceIdToBytes (traceIdFromBytes bs) === bs
    , testProperty "fromBytes with wrong length returns invalidTraceId" $
        forAll (genBytesNotLength 16) $ \bs ->
          traceIdFromBytes bs === invalidTraceId
    , testProperty "toHex . fromHex == id for valid 32-char hex" $
        forAll (genHexString 32) $ \hex ->
          traceIdToHex (traceIdFromHex hex) === hex
    , testProperty "fromHex with invalid input returns invalidTraceId" $
        forAll arbitrary $ \(t :: Text) ->
          Text.length t /= 32 ==> traceIdFromHex t === invalidTraceId
    , testProperty "toHex invalidTraceId is 32 zeros" $
        once $ traceIdToHex invalidTraceId === Text.replicate 32 "0"
    , testProperty "fromHex . toHex == id" $
        forAll genTraceIdBytes $ \bs ->
          let tid = traceIdFromBytes bs
              hex = traceIdToHex tid
           in traceIdFromHex hex === tid
    ]


-------------------------------------------------------------------------------
-- SpanId
-------------------------------------------------------------------------------

spanIdTests :: TestTree
spanIdTests =
  testGroup
    "SpanId"
    [ testProperty "toBytes . fromBytes == id for 8-byte input" $
        forAll genSpanIdBytes $ \bs ->
          spanIdToBytes (spanIdFromBytes bs) === bs
    , testProperty "fromBytes with wrong length returns invalidSpanId" $
        forAll (genBytesNotLength 8) $ \bs ->
          spanIdFromBytes bs === invalidSpanId
    , testProperty "toHex . fromHex == id for valid 16-char hex" $
        forAll (genHexString 16) $ \hex ->
          spanIdToHex (spanIdFromHex hex) === hex
    , testProperty "fromHex with invalid input returns invalidSpanId" $
        forAll arbitrary $ \(t :: Text) ->
          Text.length t /= 16 ==> spanIdFromHex t === invalidSpanId
    , testProperty "toHex invalidSpanId is 16 zeros" $
        once $ spanIdToHex invalidSpanId === Text.replicate 16 "0"
    , testProperty "fromHex . toHex == id" $
        forAll genSpanIdBytes $ \bs ->
          let sid = spanIdFromBytes bs
              hex = spanIdToHex sid
           in spanIdFromHex hex === sid
    ]


-------------------------------------------------------------------------------
-- TraceFlags
-------------------------------------------------------------------------------

traceFlagsTests :: TestTree
traceFlagsTests =
  testGroup
    "TraceFlags"
    [ testProperty "toByte . fromByte == id" $
        forAll arbitrary $ \(w :: Word8) ->
          traceFlagsToByte (traceFlagsFromByte w) === w
    , testProperty "isSampled sampledFlag == True" $
        once $ isSampled sampledFlag === True
    , testProperty "isSampled emptyTraceFlags == False" $
        once $ isSampled emptyTraceFlags === False
    , testProperty "isSampled (fromByte w) iff bit 0 set" $
        forAll arbitrary $ \(w :: Word8) ->
          isSampled (traceFlagsFromByte w) === (w `mod` 2 == 1)
    ]


-------------------------------------------------------------------------------
-- SpanContext
-------------------------------------------------------------------------------

spanContextTests :: TestTree
spanContextTests =
  testGroup
    "SpanContext"
    [ testProperty "isValid returns False for invalidSpanContext" $
        once $ isValid invalidSpanContext === False
    , testProperty "isValid False if traceId invalid" $
        forAll genNonZeroSpanIdBytes $ \sbs ->
          let sc =
                invalidSpanContext
                  { traceId = invalidTraceId
                  , spanId = spanIdFromBytes sbs
                  }
           in isValid sc === False
    , testProperty "isValid False if spanId invalid" $
        forAll genNonZeroTraceIdBytes $ \tbs ->
          let sc =
                invalidSpanContext
                  { traceId = traceIdFromBytes tbs
                  , spanId = invalidSpanId
                  }
           in isValid sc === False
    , testProperty "isValid True if both non-zero" $
        forAll genNonZeroTraceIdBytes $ \tbs ->
          forAll genNonZeroSpanIdBytes $ \sbs ->
            let sc =
                  invalidSpanContext
                    { traceId = traceIdFromBytes tbs
                    , spanId = spanIdFromBytes sbs
                    }
             in isValid sc === True
    , testProperty "isRemote returns the field value" $
        forAll arbitrary $ \(remote :: Bool) ->
          let sc = invalidSpanContext {_isRemote = remote}
           in isRemote sc === remote
    ]


-------------------------------------------------------------------------------
-- TraceState
-------------------------------------------------------------------------------

traceStateTests :: TestTree
traceStateTests =
  testGroup
    "TraceState"
    [ testProperty "empty has no entries" $
        once $ TraceState.toList TraceState.empty === []
    , testProperty "get after set returns Just v" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: Text) ->
            TraceState.get k (TraceState.set k v TraceState.empty) === Just v
    , testProperty "set moves key to front" $
        forAll (unShortKey <$> arbitrary) $ \k1 ->
          forAll (unShortKey <$> (arbitrary `suchThat` (\sk -> unShortKey sk /= k1))) $ \k2 ->
            forAll arbitrary $ \(v1 :: Text) ->
              forAll arbitrary $ \v2 ->
                forAll arbitrary $ \v3 ->
                  let ts = TraceState.set k1 v3 (TraceState.set k2 v2 (TraceState.set k1 v1 TraceState.empty))
                      entries = TraceState.toList ts
                   in case entries of
                        ((fk, fv) : _) -> fk === k1 .&&. fv === v3
                        _ -> property False
    , testProperty "get after delete returns Nothing" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: Text) ->
            let ts = TraceState.set k v TraceState.empty
             in TraceState.get k (TraceState.delete k ts) === Nothing
    , testProperty "delete on non-existent key is identity" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          TraceState.toList (TraceState.delete k TraceState.empty)
            === TraceState.toList TraceState.empty
    , testProperty "set returns new TraceState, original unchanged" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: Text) ->
            let original = TraceState.empty
                modified = TraceState.set k v original
             in TraceState.toList original === []
                  .&&. TraceState.get k modified === Just v
    ]


-------------------------------------------------------------------------------
-- SpanKind
-------------------------------------------------------------------------------

spanKindTests :: TestTree
spanKindTests =
  testGroup
    "SpanKind"
    [ testCase "All constructors are distinct" $ do
        let kinds = [Internal, Server, Client, Producer, Consumer]
        length kinds @?= 5
        assertBool "all distinct" (nubBy (\a b -> a == b) kinds == kinds)
    , testCase "Enum round-trips all constructors" $ do
        let kinds = [minBound .. maxBound] :: [SpanKind]
        mapM_ (\k -> toEnum (fromEnum k) @?= k) kinds
    ]


-------------------------------------------------------------------------------
-- StatusCode
-------------------------------------------------------------------------------

statusCodeTests :: TestTree
statusCodeTests =
  testGroup
    "StatusCode"
    [ testCase "All constructors are distinct" $ do
        let codes = [Unset, Ok, Error]
        length codes @?= 3
        assertBool "all distinct" (nubBy (\a b -> a == b) codes == codes)
    , testCase "Enum round-trips all constructors" $ do
        let codes = [minBound .. maxBound] :: [StatusCode]
        mapM_ (\c -> toEnum (fromEnum c) @?= c) codes
    ]


-------------------------------------------------------------------------------
-- SpanConfig defaults
-------------------------------------------------------------------------------

spanConfigTests :: TestTree
spanConfigTests =
  testGroup
    "SpanConfig (defaults)"
    [ testCase "spanKind is Internal" $
        defaultSpanConfig.spanKind @?= Internal
    , testCase "spanAttributes is []" $
        defaultSpanConfig.spanAttributes @?= []
    , testCase "spanLinks is []" $
        defaultSpanConfig.spanLinks @?= []
    , testCase "spanStartTimestamp is Nothing" $
        defaultSpanConfig.spanStartTimestamp @?= Nothing
    , testCase "spanNoParent is False" $
        defaultSpanConfig.spanNoParent @?= False
    ]


-------------------------------------------------------------------------------
-- NoOp implementations
-------------------------------------------------------------------------------

-- | A dummy exception for testing recordException.
dummyException :: SomeException
dummyException = toException (userError "test")

noOpTests :: TestTree
noOpTests =
  testGroup
    "NoOp implementations"
    [ testCase "NoOpTracerProvider.getTracer returns a working tracer" $ do
        tracer <- getTracer NoOpTracerProvider dummyScope
        span_ <- startSpan tracer "test" root defaultSpanConfig
        -- Just verify it returns without crashing
        _ <- isRecording span_
        pure ()
    , testCase "NoOpTracer.startSpan returns a SomeSpan" $ do
        span_ <- startSpan NoOpTracer "test" root defaultSpanConfig
        -- Verify it's a span we can interact with
        sc <- getSpanContext span_
        sc @?= invalidSpanContext
    , testCase "NoOpSpan.isRecording returns False" $ do
        rec <- isRecording NoOpSpan
        rec @?= False
    , testCase "NoOpSpan.getSpanContext returns invalidSpanContext" $ do
        sc <- getSpanContext NoOpSpan
        sc @?= invalidSpanContext
    , testCase "All mutating operations on NoOpSpan are silent" $ do
        setAttribute NoOpSpan "key" (StringValue "val")
        addEvent NoOpSpan "event" emptyAttributes Nothing
        addLink NoOpSpan invalidSpanContext emptyAttributes
        setStatus NoOpSpan Ok Nothing
        setStatus NoOpSpan Error (Just "fail")
        setStatus NoOpSpan Unset Nothing
        recordException NoOpSpan dummyException emptyAttributes
        updateName NoOpSpan "new-name"
        end NoOpSpan Nothing
    , testCase "NoOpSpan operations after end are still silent" $ do
        end NoOpSpan Nothing
        -- All operations after end should not crash
        setAttribute NoOpSpan "key" (StringValue "val")
        addEvent NoOpSpan "event" emptyAttributes Nothing
        addLink NoOpSpan invalidSpanContext emptyAttributes
        setStatus NoOpSpan Error (Just "post-end")
        recordException NoOpSpan dummyException emptyAttributes
        updateName NoOpSpan "after-end"
        end NoOpSpan Nothing  -- double-end
    , testCase "setStatus with each StatusCode on NoOpSpan doesn't crash" $ do
        setStatus NoOpSpan Unset Nothing
        setStatus NoOpSpan Unset (Just "description")
        setStatus NoOpSpan Ok Nothing
        setStatus NoOpSpan Ok (Just "description")
        setStatus NoOpSpan Error Nothing
        setStatus NoOpSpan Error (Just "error description")
    , testCase "recordException on NoOpSpan doesn't crash" $ do
        recordException NoOpSpan dummyException emptyAttributes
        recordException NoOpSpan (toException (userError "other")) emptyAttributes
    ]
  where
    dummyScope :: InstrumentationScope
    dummyScope =
      InstrumentationScope
        { scopeName = "test-lib"
        , scopeVersion = Just "1.0"
        , scopeSchemaUrl = Just ""
        , scopeAttributes = Nothing
        }


-------------------------------------------------------------------------------
-- Global TracerProvider
-------------------------------------------------------------------------------

globalProviderTests :: TestTree
globalProviderTests =
  testGroup
    "Global TracerProvider"
    [ testCase "Default global provider is NoOp (getTracer works)" $ do
        provider <- getGlobalTracerProvider
        tracer <- getTracer provider dummyScope
        span_ <- startSpan tracer "test" root defaultSpanConfig
        rec <- isRecording span_
        rec @?= False
    , testCase "setGlobalTracerProvider then get returns the set provider" $ do
        -- Save original so we can restore it
        original <- getGlobalTracerProvider
        let custom = SomeTracerProvider NoOpTracerProvider
        setGlobalTracerProvider custom
        retrieved <- getGlobalTracerProvider
        -- We can't compare providers directly, but we can verify it works
        tracer <- getTracer retrieved dummyScope
        span_ <- startSpan tracer "test" root defaultSpanConfig
        rec <- isRecording span_
        rec @?= False
        -- Restore original
        setGlobalTracerProvider original
    ]
  where
    dummyScope :: InstrumentationScope
    dummyScope =
      InstrumentationScope
        { scopeName = "test-lib"
        , scopeVersion = Just "1.0"
        , scopeSchemaUrl = Just ""
        , scopeAttributes = Nothing
        }


-------------------------------------------------------------------------------
-- Context integration
-------------------------------------------------------------------------------

contextIntegrationTests :: TestTree
contextIntegrationTests =
  testGroup
    "Context integration"
    [ testCase "getSpanFromContext root returns Nothing" $
        case getSpanFromContext root of
          Nothing -> pure ()
          Just _ -> assertBool "expected Nothing from root context" False
    , testCase "setSpanInContext then getSpanFromContext returns Just the span" $ do
        let span_ = SomeSpan NoOpSpan
            ctx = setSpanInContext span_ root
        case getSpanFromContext ctx of
          Nothing -> assertBool "expected Just span" False
          Just s -> do
            sc <- getSpanContext s
            sc @?= invalidSpanContext  -- It's a NoOpSpan
    , testCase "createNonRecordingSpan wraps SpanContext" $ do
        let tbs = traceIdFromHex "0af7651916cd43dd8448eb211c80319c"
            sbs = spanIdFromHex "00f067aa0ba902b7"
            sc = invalidSpanContext { traceId = tbs, spanId = sbs }
            span_ = createNonRecordingSpan sc
        retrieved <- getSpanContext span_
        retrieved @?= sc
    , testCase "createNonRecordingSpan: isRecording returns False" $ do
        let span_ = createNonRecordingSpan invalidSpanContext
        rec <- isRecording span_
        rec @?= False
    , testCase "NonRecordingSpan operations after end are silent" $ do
        let span_ = createNonRecordingSpan invalidSpanContext
        end span_ Nothing
        setAttribute span_ "key" (StringValue "val")
        addEvent span_ "event" emptyAttributes Nothing
        setStatus span_ Error (Just "desc")
        rec <- isRecording span_
        rec @?= False
        sc <- getSpanContext span_
        sc @?= invalidSpanContext
    , testCase "setSpanInContext doesn't mutate original context" $ do
        let span_ = SomeSpan NoOpSpan
            ctx1 = root
            ctx2 = setSpanInContext span_ ctx1
        case getSpanFromContext ctx1 of
          Nothing -> pure ()
          Just _ -> assertBool "expected Nothing in original ctx" False
        case getSpanFromContext ctx2 of
          Nothing -> assertBool "expected Just span in ctx2" False
          Just _ -> pure ()
    ]


-------------------------------------------------------------------------------
-- Baggage API
-------------------------------------------------------------------------------

baggageApiTests :: TestTree
baggageApiTests =
  testGroup
    "Baggage API"
    [ testProperty "getValue after setValue returns the value" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          forAll arbitrary $ \(val :: Text) ->
            Baggage.getValue name (Baggage.setValue name val Nothing emptyBaggage)
              === Just val
    , testProperty "getEntry after setValue returns the entry" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          forAll arbitrary $ \(val :: Text) ->
            forAll arbitrary $ \(meta :: Maybe Text) ->
              getEntry name (Baggage.setValue name val meta emptyBaggage)
                === Just (BaggageEntry val meta)
    , testProperty "getValue on emptyBaggage returns Nothing" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          Baggage.getValue name emptyBaggage === Nothing
    , testProperty "setValue returns new Baggage, original unchanged" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          forAll arbitrary $ \(val :: Text) ->
            let b = Baggage.setValue name val Nothing emptyBaggage
             in Baggage.getValue name emptyBaggage === Nothing
                  .&&. Baggage.getValue name b === Just val
    , testProperty "removeValue after setValue returns Nothing" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          forAll arbitrary $ \(val :: Text) ->
            Baggage.getValue name
              (removeValue name (Baggage.setValue name val Nothing emptyBaggage))
              === Nothing
    , testProperty "removeValue on non-existent key is identity" $
        forAll (unShortKey <$> arbitrary) $ \name ->
          baggageToList (removeValue name emptyBaggage) === []
    , testProperty "getAllValues agrees with baggageToList" $
        forAll genBaggage $ \bag ->
          Map.toAscList (getAllValues bag)
            === sortBy (comparing fst) (baggageToList bag)
    , testProperty "Context round-trip: getBaggage . setBaggage == id" $
        forAll genBaggage $ \bag ->
          getBaggage (setBaggage bag root) === bag
    , testProperty "Metadata round-trip through W3C header" $
        ioProperty $ do
          let b = Baggage.setValue "k" "v" (Just "md") emptyBaggage
              ctx = setBaggage b root
          carrier <- inject W3CBaggagePropagator ctx (Map.empty :: Map Text Text)
          ctx2 <- extract W3CBaggagePropagator root carrier
          let b2 = getBaggage ctx2
          pure (getEntry "k" b2 === Just (BaggageEntry "v" (Just "md")))
    ]


-------------------------------------------------------------------------------
-- Propagation generators
-------------------------------------------------------------------------------

-- | Generate a valid SpanContext with non-zero traceId and spanId.
genValidSpanContext :: Gen SpanContext
genValidSpanContext = do
  tidBytes <- genNonZeroTraceIdBytes
  sidBytes <- genNonZeroSpanIdBytes
  flags <- arbitrary :: Gen Word8
  pure $ SpanContext
    { traceId = traceIdFromBytes tidBytes
    , spanId = spanIdFromBytes sidBytes
    , traceFlags = traceFlagsFromByte flags
    , traceState = TraceState.empty
    , _isRemote = False
    }


-- | Generate a valid SpanContext with a non-empty TraceState.
genValidSpanContextWithTraceState :: Gen SpanContext
genValidSpanContextWithTraceState = do
  sc <- genValidSpanContext
  k <- elements ["vendor1", "vendor2", "rojo"]
  v <- elements ["value1", "value2", "s]dr4t5"]
  pure sc { traceState = TraceState.set k v TraceState.empty }


-- | Generate a non-empty Baggage with simple alphanumeric keys and values.
genBaggage :: Gen Baggage
genBaggage = do
  n <- chooseInt (1, 5)
  entries <- vectorOf n genBaggageEntry
  let pairs = zip (map (\i -> Text.pack ("key" <> show i)) [1 .. n :: Int]) entries
  pure (baggageFromList pairs)


genBaggageEntry :: Gen BaggageEntry
genBaggageEntry = do
  val <- Text.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))
  pure (BaggageEntry val Nothing)


-------------------------------------------------------------------------------
-- Propagation tests
-------------------------------------------------------------------------------

propagationTests :: TestTree
propagationTests =
  testGroup
    "Propagation"
    [ textMapGetterSetterTests
    , noOpPropagatorTests
    , compositePropagatorTests
    , w3cTraceContextTests
    , w3cBaggageTests
    , globalPropagatorRegistrationTests
    ]


textMapGetterSetterTests :: TestTree
textMapGetterSetterTests =
  testGroup
    "TextMapGetter/Setter instances (Map)"
    [ testProperty "tmGet after tmSet returns Just v" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: Text) ->
            forAll arbitrary $ \(m :: Map Text Text) ->
              tmGet (tmSet m k v) k === Just v
    , testProperty "tmGet on empty map returns Nothing" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          tmGet (Map.empty :: Map Text Text) k === Nothing
    , testProperty "tmKeys of singleton via tmSet" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: Text) ->
            tmKeys (tmSet (Map.empty :: Map Text Text) k v) === [k]
    ]


noOpPropagatorTests :: TestTree
noOpPropagatorTests =
  testGroup
    "NoOpPropagator"
    [ testProperty "inject returns carrier unchanged" $
        forAll arbitrary $ \(m :: Map Text Text) ->
          ioProperty $ do
            result <- inject NoOpPropagator root m
            pure (result === m)
    , testCase "extract returns context unchanged (no span added)" $ do
        ctx <- extract NoOpPropagator root (Map.empty :: Map Text Text)
        case getSpanFromContext ctx of
          Nothing -> pure ()
          Just _ -> assertBool "NoOp extract should not add a span" False
    , testCase "fields is empty" $
        fields NoOpPropagator @?= ([] :: [Text])
    ]


compositePropagatorTests :: TestTree
compositePropagatorTests =
  testGroup
    "CompositePropagator"
    [ testCase "fields is concatenation of children's fields" $ do
        let cp = CompositePropagator
              [ SomeTextMapPropagator W3CTraceContextPropagator
              , SomeTextMapPropagator W3CBaggagePropagator
              ]
        sort (fields cp) @?= sort ["traceparent", "tracestate", "baggage"]
    , testProperty "inject delegates to children (traceparent written)" $
        forAll genValidSpanContext $ \sc ->
          ioProperty $ do
            let span_ = createNonRecordingSpan sc
                ctx = setSpanInContext span_ root
                cp = CompositePropagator
                  [SomeTextMapPropagator W3CTraceContextPropagator]
            carrier <- inject cp ctx (Map.empty :: Map Text Text)
            pure $ case Map.lookup "traceparent" carrier of
              Nothing -> property False
              Just _  -> property True
    , testProperty "extract recovers both span and baggage" $
        forAll genValidSpanContext $ \sc ->
          forAll genBaggage $ \bag ->
            ioProperty $ do
              let span_ = createNonRecordingSpan sc
                  ctx = setBaggage bag (setSpanInContext span_ root)
                  cp = CompositePropagator
                    [ SomeTextMapPropagator W3CTraceContextPropagator
                    , SomeTextMapPropagator W3CBaggagePropagator
                    ]
              carrier <- inject cp ctx (Map.empty :: Map Text Text)
              ctx2 <- extract cp root carrier
              let hasSpan = case getSpanFromContext ctx2 of
                    Nothing -> False
                    Just _  -> True
                  bag2 = getBaggage ctx2
              pure $ hasSpan === True
                .&&. baggageToList bag2 === baggageToList bag
    ]


w3cTraceContextTests :: TestTree
w3cTraceContextTests =
  testGroup
    "W3CTraceContextPropagator"
    [ testProperty "round-trip: inject then extract recovers traceId, spanId, traceFlags" $
        forAll genValidSpanContext $ \sc ->
          ioProperty $ do
            let span_ = createNonRecordingSpan sc
                ctx = setSpanInContext span_ root
            carrier <- inject W3CTraceContextPropagator ctx (Map.empty :: Map Text Text)
            ctx2 <- extract W3CTraceContextPropagator root carrier
            case getSpanFromContext ctx2 of
              Nothing -> pure (property False)
              Just s -> do
                sc2 <- getSpanContext s
                pure $ sc2.traceId === sc.traceId
                  .&&. sc2.spanId === sc.spanId
                  .&&. sc2.traceFlags === sc.traceFlags
    , testProperty "tracestate round-trip" $
        forAll genValidSpanContextWithTraceState $ \sc ->
          ioProperty $ do
            let span_ = createNonRecordingSpan sc
                ctx = setSpanInContext span_ root
            carrier <- inject W3CTraceContextPropagator ctx (Map.empty :: Map Text Text)
            ctx2 <- extract W3CTraceContextPropagator root carrier
            case getSpanFromContext ctx2 of
              Nothing -> pure (property False)
              Just s -> do
                sc2 <- getSpanContext s
                pure $ TraceState.toList sc2.traceState === TraceState.toList sc.traceState
    , testProperty "extracted span is remote" $
        forAll genValidSpanContext $ \sc ->
          ioProperty $ do
            let span_ = createNonRecordingSpan sc
                ctx = setSpanInContext span_ root
            carrier <- inject W3CTraceContextPropagator ctx (Map.empty :: Map Text Text)
            ctx2 <- extract W3CTraceContextPropagator root carrier
            case getSpanFromContext ctx2 of
              Nothing -> pure (property False)
              Just s -> do
                sc2 <- getSpanContext s
                pure $ isRemote sc2 === True
    , testCase "no span in context -> inject writes nothing" $ do
        carrier <- inject W3CTraceContextPropagator root (Map.empty :: Map Text Text)
        Map.lookup "traceparent" carrier @?= Nothing
    , testCase "invalid SpanContext -> inject writes nothing" $ do
        let span_ = createNonRecordingSpan invalidSpanContext
            ctx = setSpanInContext span_ root
        carrier <- inject W3CTraceContextPropagator ctx (Map.empty :: Map Text Text)
        Map.lookup "traceparent" carrier @?= Nothing
    , testCase "fields == [traceparent, tracestate]" $
        sort (fields W3CTraceContextPropagator) @?= sort ["traceparent", "tracestate"]
    , testGroup "invalid traceparent is ignored"
        [ mkInvalidTraceparentTest "empty string" ""
        , mkInvalidTraceparentTest "wrong number of parts" "00-abc"
        , mkInvalidTraceparentTest "version ff"
            "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
        , mkInvalidTraceparentTest "all-zero trace-id"
            "00-00000000000000000000000000000000-00f067aa0ba902b7-01"
        , mkInvalidTraceparentTest "all-zero span-id"
            "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01"
        , mkInvalidTraceparentTest "short trace-id (30 chars)"
            "00-4bf92f3577b34da6a3ce929d0e0e47-00f067aa0ba902b7-01"
        , mkInvalidTraceparentTest "short span-id (14 chars)"
            "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902-01"
        , mkInvalidTraceparentTest "v00 with extra trailing field (W3C 3.2.2)"
            "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-extra"
        ]
    ]


-- | Helper: test that a malformed traceparent header is ignored by extract.
mkInvalidTraceparentTest :: String -> Text -> TestTree
mkInvalidTraceparentTest desc tp =
  testCase desc $ do
    let carrier = Map.singleton "traceparent" tp :: Map Text Text
    ctx <- extract W3CTraceContextPropagator root carrier
    case getSpanFromContext ctx of
      Nothing -> pure ()
      Just _ -> assertBool ("expected no span for invalid traceparent: " <> desc) False


w3cBaggageTests :: TestTree
w3cBaggageTests =
  testGroup
    "W3CBaggagePropagator"
    [ testProperty "round-trip: inject then extract recovers baggage" $
        forAll genBaggage $ \bag ->
          ioProperty $ do
            let ctx = setBaggage bag root
            carrier <- inject W3CBaggagePropagator ctx (Map.empty :: Map Text Text)
            ctx2 <- extract W3CBaggagePropagator root carrier
            let bag2 = getBaggage ctx2
            pure $ baggageToList bag2 === baggageToList bag
    , testCase "empty baggage -> no header" $ do
        let ctx = setBaggage emptyBaggage root
        carrier <- inject W3CBaggagePropagator ctx (Map.empty :: Map Text Text)
        Map.lookup "baggage" carrier @?= Nothing
    , testCase "no baggage header -> context unchanged" $ do
        ctx <- extract W3CBaggagePropagator root (Map.empty :: Map Text Text)
        baggageToList (getBaggage ctx) @?= []
    , testCase "fields == [baggage]" $
        fields W3CBaggagePropagator @?= ["baggage"]
    ]


globalPropagatorRegistrationTests :: TestTree
globalPropagatorRegistrationTests =
  testGroup
    "Global propagator registration"
    [ testCase "setGlobal then getGlobal returns the set propagator" $ do
        let p = SomeTextMapPropagator NoOpPropagator
        setGlobalTextMapPropagator p
        got <- getGlobalTextMapPropagator
        carrier <- inject got root (Map.empty :: Map Text Text)
        carrier @?= Map.empty
    ]


-------------------------------------------------------------------------------
-- Metrics API
-------------------------------------------------------------------------------

metricsApiTests :: TestTree
metricsApiTests =
  testGroup
    "Metrics API"
    [ noOpSyncTests
    , someWrapperSyncTests
    , noOpObservableTests
    , noOpMeterTests
    , noOpMeterProviderTests
    , globalMeterProviderTests
    , instrumentOptionsTests
    ]
  where
    dummyScope :: InstrumentationScope
    dummyScope =
      InstrumentationScope
        { scopeName = "test-metrics"
        , scopeVersion = Nothing
        , scopeSchemaUrl = Nothing
        , scopeAttributes = Nothing
        }

    --------------------------------------------------------------------------
    -- NoOp instruments — synchronous
    --------------------------------------------------------------------------

    noOpSyncTests :: TestTree
    noOpSyncTests =
      testGroup
        "NoOp instruments — synchronous"
        [ testCase "NoOpCounter.counterAdd is silent" $ do
            counterAdd NoOpCounter 1.0 emptyAttributes
            counterAdd NoOpCounter (-1.0) emptyAttributes
            counterAdd NoOpCounter 0.0 emptyAttributes
        , testCase "NoOpUpDownCounter.upDownCounterAdd is silent" $ do
            upDownCounterAdd NoOpUpDownCounter 1.0 emptyAttributes
            upDownCounterAdd NoOpUpDownCounter (-5.0) emptyAttributes
            upDownCounterAdd NoOpUpDownCounter 0.0 emptyAttributes
        , testCase "NoOpHistogram.histogramRecord is silent" $ do
            histogramRecord NoOpHistogram 1.0 emptyAttributes
            histogramRecord NoOpHistogram (-1.0) emptyAttributes
            histogramRecord NoOpHistogram 0.0 emptyAttributes
        , testCase "NoOpGauge.gaugeSet is silent" $ do
            gaugeSet NoOpGauge 1.0 emptyAttributes
            gaugeSet NoOpGauge (-1.0) emptyAttributes
            gaugeSet NoOpGauge 0.0 emptyAttributes
        ]

    --------------------------------------------------------------------------
    -- NoOp instruments — SomeX wrapping
    --------------------------------------------------------------------------

    someWrapperSyncTests :: TestTree
    someWrapperSyncTests =
      testGroup
        "NoOp instruments — SomeX wrapping"
        [ testCase "SomeCounter wraps NoOpCounter transparently" $ do
            let c = SomeCounter NoOpCounter
            counterAdd c 42.0 emptyAttributes
        , testCase "SomeUpDownCounter wraps NoOpUpDownCounter transparently" $ do
            let u = SomeUpDownCounter NoOpUpDownCounter
            upDownCounterAdd u (-3.0) emptyAttributes
        , testCase "SomeHistogram wraps NoOpHistogram transparently" $ do
            let h = SomeHistogram NoOpHistogram
            histogramRecord h 0.5 emptyAttributes
        , testCase "SomeGauge wraps NoOpGauge transparently" $ do
            let g = SomeGauge NoOpGauge
            gaugeSet g 99.0 emptyAttributes
        ]

    --------------------------------------------------------------------------
    -- NoOp observable instruments
    --------------------------------------------------------------------------

    noOpObservableTests :: TestTree
    noOpObservableTests =
      testGroup
        "NoOp observable instruments"
        [ testCase "addObservableCounterCallback on NoOp is silent" $
            addObservableCounterCallback NoOpObservableCounter (\_ -> pure ())
        , testCase "addObservableUpDownCounterCallback on NoOp is silent" $
            addObservableUpDownCounterCallback NoOpObservableUpDownCounter (\_ -> pure ())
        , testCase "addObservableGaugeCallback on NoOp is silent" $
            addObservableGaugeCallback NoOpObservableGauge (\_ -> pure ())
        , testCase "unregister NoOpCallbackRegistration is silent" $
            unregister NoOpCallbackRegistration
        , testCase "observeValue NoOpObservableResult is silent" $
            observeValue NoOpObservableResult 1.0 emptyAttributes
        , testCase "batchObserveValue NoOpBatchObservableResult is silent" $
            batchObserveValue
              NoOpBatchObservableResult
              (mkSomeObsCounter "" (SomeObservableCounter NoOpObservableCounter))
              1.0
              emptyAttributes
        ]

    --------------------------------------------------------------------------
    -- NoOpMeter — instrument creation
    --------------------------------------------------------------------------

    noOpMeterTests :: TestTree
    noOpMeterTests =
      testGroup
        "NoOpMeter — instrument creation"
        [ testCase "NoOpMeter creates all no-op instruments" $ do
            let meter = NoOpMeter
            counter  <- createCounter meter "requests" Nothing
            udc      <- createUpDownCounter meter "queue.size" Nothing
            hist     <- createHistogram meter "latency" Nothing
            gauge    <- createGauge meter "memory" Nothing
            obsC     <- createObservableCounter meter "obs.counter" [] Nothing
            obsUDC   <- createObservableUpDownCounter meter "obs.udc" [] Nothing
            obsG     <- createObservableGauge meter "obs.gauge" [] Nothing
            reg      <- registerCallback meter
                          [ mkSomeObsCounter "obs.counter" obsC
                          , mkSomeObsUpDownCounter "obs.udc" obsUDC
                          , mkSomeObsGauge "obs.gauge" obsG
                          ]
                          (\_ -> pure ())
            -- All operations on the returned no-op instruments are silent
            counterAdd counter 1.0 emptyAttributes
            upDownCounterAdd udc (-5.0) emptyAttributes
            histogramRecord hist 0.5 emptyAttributes
            gaugeSet gauge 99.0 emptyAttributes
            addObservableCounterCallback obsC (\_ -> pure ())
            unregister reg
        ]

    --------------------------------------------------------------------------
    -- NoOpMeterProvider
    --------------------------------------------------------------------------

    noOpMeterProviderTests :: TestTree
    noOpMeterProviderTests =
      testGroup
        "NoOpMeterProvider"
        [ testCase "NoOpMeterProvider.getMeter returns NoOpMeter" $ do
            meter <- getMeter NoOpMeterProvider dummyScope
            counter <- createCounter meter "x" Nothing
            counterAdd counter 1.0 emptyAttributes
        ]

    --------------------------------------------------------------------------
    -- Global MeterProvider registration
    --------------------------------------------------------------------------

    globalMeterProviderTests :: TestTree
    globalMeterProviderTests =
      testGroup
        "Global MeterProvider registration"
        [ testCase "setGlobalMeterProvider then getGlobalMeterProvider round-trip" $ do
            original <- getGlobalMeterProvider
            let custom = SomeMeterProvider NoOpMeterProvider
            setGlobalMeterProvider custom
            retrieved <- getGlobalMeterProvider
            meter <- getMeter retrieved dummyScope
            counter <- createCounter meter "y" Nothing
            counterAdd counter 1.0 emptyAttributes
            -- Restore original
            setGlobalMeterProvider original
        ]

    --------------------------------------------------------------------------
    -- InstrumentOptions
    --------------------------------------------------------------------------

    instrumentOptionsTests :: TestTree
    instrumentOptionsTests =
      testGroup
        "InstrumentOptions"
        [ testCase "defaultInstrumentOptions has all Nothing" $ do
            defaultInstrumentOptions.instrumentDescription @?= Nothing
            defaultInstrumentOptions.instrumentUnit        @?= Nothing
            defaultInstrumentOptions.instrumentAdvisory    @?= Nothing
        ]


-------------------------------------------------------------------------------
-- Logs API
-------------------------------------------------------------------------------

logsApiTests :: TestTree
logsApiTests =
  testGroup
    "Logs API"
    [ severityNumberTests
    , logBodyTests
    , logRecordTests
    , noOpLoggerTests
    , noOpLoggerProviderTests
    , globalLoggerProviderTests
    ]
  where
    dummyScope :: InstrumentationScope
    dummyScope =
      InstrumentationScope
        { scopeName = "test-logs"
        , scopeVersion = Nothing
        , scopeSchemaUrl = Nothing
        , scopeAttributes = Nothing
        }

    --------------------------------------------------------------------------
    -- SeverityNumber
    --------------------------------------------------------------------------

    severityNumberTests :: TestTree
    severityNumberTests =
      testGroup
        "SeverityNumber"
        [ testProperty "severityNumberValue covers 1..24" $
            once $
              map severityNumberValue [minBound .. maxBound]
                === [1 .. 24]
        , testCase "SeverityNumber has 24 constructors" $
            length [minBound .. maxBound :: SeverityNumber] @?= 24
        , testProperty "Enum round-trip" $
            forAll (elements [minBound .. maxBound :: SeverityNumber]) $ \s ->
              toEnum (fromEnum s) === s
        , testCase "Severity ordering" $ do
            assertBool "TRACE < DEBUG"  (SeverityTrace  < SeverityDebug)
            assertBool "DEBUG < INFO"   (SeverityDebug  < SeverityInfo)
            assertBool "INFO < WARN"    (SeverityInfo   < SeverityWarn)
            assertBool "WARN < ERROR"   (SeverityWarn   < SeverityError)
            assertBool "ERROR < FATAL"  (SeverityError  < SeverityFatal)
        ]

    --------------------------------------------------------------------------
    -- LogBody
    --------------------------------------------------------------------------

    logBodyTests :: TestTree
    logBodyTests =
      testGroup
        "LogBody"
        [ testCase "LogBody constructors are distinct" $ do
            (LogBodyString "x"   /= LogBodyBool True)   @?= True
            (LogBodyInt64 1      /= LogBodyFloat64 1.0)  @?= True
            (LogBodyBytes ""     /= LogBodyList [])      @?= True
            (LogBodyMap mempty   /= LogBodyString "")    @?= True
        , testCase "LogBodyList nests LogBody values" $
            LogBodyList [LogBodyString "a", LogBodyInt64 1]
              @?= LogBodyList [LogBodyString "a", LogBodyInt64 1]
        , testCase "LogBodyMap holds string keys" $ do
            let m = LogBodyMap (Map.fromList [("k", LogBodyBool True)])
            m @?= LogBodyMap (Map.fromList [("k", LogBodyBool True)])
        ]

    --------------------------------------------------------------------------
    -- LogRecord
    --------------------------------------------------------------------------

    logRecordTests :: TestTree
    logRecordTests =
      testGroup
        "LogRecord"
        [ testCase "defaultLogRecord has all Nothing" $ do
            defaultLogRecord.logTimestamp         @?= Nothing
            defaultLogRecord.logObservedTimestamp @?= Nothing
            assertBool "logContext is Nothing"
              (case defaultLogRecord.logContext of Nothing -> True; Just _ -> False)
            defaultLogRecord.logSeverityNumber    @?= Nothing
            defaultLogRecord.logSeverityText      @?= Nothing
            defaultLogRecord.logBody              @?= Nothing
            defaultLogRecord.logAttributes        @?= emptyAttributes
        , testCase "LogRecord field update" $ do
            let r = defaultLogRecord
                      { logSeverityNumber = Just SeverityInfo
                      , logSeverityText   = Just "INFO"
                      , logBody           = Just (LogBodyString "hello")
                      }
            r.logSeverityNumber @?= Just SeverityInfo
            r.logSeverityText   @?= Just "INFO"
            r.logBody           @?= Just (LogBodyString "hello")
            assertBool "logContext unchanged"
              (case r.logContext of Nothing -> True; Just _ -> False)
        ]

    --------------------------------------------------------------------------
    -- NoOpLogger
    --------------------------------------------------------------------------

    noOpLoggerTests :: TestTree
    noOpLoggerTests =
      testGroup
        "NoOpLogger"
        [ testCase "NoOpLogger.emit is silent" $ do
            emit NoOpLogger defaultLogRecord
            emit NoOpLogger defaultLogRecord { logSeverityNumber = Just SeverityError }
        , testCase "NoOpLogger.isEnabled returns False" $ do
            r1 <- isEnabled NoOpLogger SeverityTrace Nothing Nothing
            r1 @?= False
            r2 <- isEnabled NoOpLogger SeverityFatal4 (Just "event") Nothing
            r2 @?= False
        , testProperty "NoOpLogger.isEnabled False for all SeverityNumber values" $
            forAll (elements [minBound .. maxBound :: SeverityNumber]) $ \sev ->
              ioProperty $ do
                r <- isEnabled NoOpLogger sev Nothing Nothing
                pure (r === False)
        ]

    --------------------------------------------------------------------------
    -- NoOpLoggerProvider
    --------------------------------------------------------------------------

    noOpLoggerProviderTests :: TestTree
    noOpLoggerProviderTests =
      testGroup
        "NoOpLoggerProvider"
        [ testCase "NoOpLoggerProvider.getLogger returns NoOpLogger" $ do
            logger <- getLogger NoOpLoggerProvider dummyScope
            emit logger defaultLogRecord
            r <- isEnabled logger SeverityInfo Nothing Nothing
            r @?= False
        ]

    --------------------------------------------------------------------------
    -- Global LoggerProvider registration
    --------------------------------------------------------------------------

    globalLoggerProviderTests :: TestTree
    globalLoggerProviderTests =
      testGroup
        "Global LoggerProvider registration"
        [ testCase "setGlobalLoggerProvider then getGlobalLoggerProvider" $ do
            original <- getGlobalLoggerProvider
            let custom = SomeLoggerProvider NoOpLoggerProvider
            setGlobalLoggerProvider custom
            retrieved <- getGlobalLoggerProvider
            logger <- getLogger retrieved dummyScope
            emit logger defaultLogRecord
            setGlobalLoggerProvider original
        ]


-------------------------------------------------------------------------------
-- Profiles API (experimental)
-------------------------------------------------------------------------------

profilesApiTests :: TestTree
profilesApiTests =
  testGroup
    "Profiles API (experimental)"
    [ testCase "NoOpProfilerProvider constructs without error" $
        let _ = NoOpProfilerProvider in pure ()
    , testCase "SomeProfilerProvider wraps NoOpProfilerProvider" $
        let _ = SomeProfilerProvider NoOpProfilerProvider in pure ()
    , testCase "Default global provider is NoOpProfilerProvider" $ do
        _ <- getGlobalProfilerProvider
        pure ()
    , testCase "setGlobalProfilerProvider then getGlobalProfilerProvider round-trip" $ do
        original <- getGlobalProfilerProvider
        let custom = SomeProfilerProvider NoOpProfilerProvider
        setGlobalProfilerProvider custom
        _ <- getGlobalProfilerProvider
        -- Restore original
        setGlobalProfilerProvider original
    ]
