{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Exception (throwIO)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prelude hiding (lookup)
import System.Environment (setEnv, unsetEnv)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import OTel.Attribute
import OTel.SDK.Export
import OTel.SDK.Resource
import OTel.SDK.Resource.Detectors


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

-- | Finite doubles only to avoid NaN comparison issues.
arbitraryFiniteDouble :: Gen Double
arbitraryFiniteDouble = do
  n <- arbitrary :: Gen Int64
  pure (fromIntegral n / 1000.0)

-- | Short alphanumeric keys to keep tests readable and improve collision
-- control for merge property coverage.
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


-------------------------------------------------------------------------------
-- Generator helpers
-------------------------------------------------------------------------------

-- | Generate a short schema URL (non-empty text).
genSchemaUrl :: Gen Text
genSchemaUrl = do
  suffix <- listOf1 (elements (['a' .. 'z'] <> ['0' .. '9'] <> ['.', '/', '-']))
  pure (Text.pack ("https://schema.example.com/" <> suffix))

-- | Generate a list of attributes with unique keys.
genAttributes :: Gen [Attribute]
genAttributes = do
  kvs <- listOf ((,) <$> (unShortKey <$> arbitrary) <*> arbitrary)
  -- Use fromList/toList to deduplicate by key (Map keeps last)
  pure (toList (fromList kvs))

-- | Generate a Resource with optional schema URL.
genResource :: Gen Resource
genResource = do
  attrs <- genAttributes
  schemaUrl <- oneof [pure Nothing, Just <$> genSchemaUrl]
  pure (create attrs schemaUrl)

-- | Generate two distinct schema URLs.
genDistinctSchemaUrls :: Gen (Text, Text)
genDistinctSchemaUrls = do
  u1 <- genSchemaUrl
  u2 <- genSchemaUrl `suchThat` (/= u1)
  pure (u1, u2)


-------------------------------------------------------------------------------
-- Test detectors
-------------------------------------------------------------------------------

-- | A detector that returns a fixed resource.
data FixedDetector = FixedDetector Resource

instance ResourceDetector FixedDetector where
  detectResource (FixedDetector r) = pure r

-- | A detector that always throws.
data ThrowingDetector = ThrowingDetector

instance ResourceDetector ThrowingDetector where
  detectResource _ = throwIO (userError "detector failed")


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "otel-sdk-common"
    [ resourceTests
    , resourceMergeTests
    , resourceDetectionTests
    , resourceDetectorTests
    , exportResultTests
    ]


-------------------------------------------------------------------------------
-- Resource: construction and accessors
-------------------------------------------------------------------------------

resourceTests :: TestTree
resourceTests =
  testGroup
    "Resource"
    [ -- Property 1: empty has no attributes
      testProperty "empty has no attributes" $
        once $ getAttributes empty === emptyAttributes

      -- Property 2: empty has no schema URL
    , testProperty "empty has no schema URL" $
        once $ getSchemaUrl empty === Nothing

      -- Property 3: create stores all attributes, lookup retrieves them
    , testProperty "create stores attributes (lookup retrieves)" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v :: AttributeValue) ->
            lookup k (getAttributes (create [(k, v)] Nothing)) === Just v

      -- Stronger variant: all attributes survive round-trip through create
    , testProperty "create preserves all unique-key attributes" $
        forAll genAttributes $ \attrs ->
          toList (getAttributes (create attrs Nothing)) === toList (fromList attrs)

      -- Property 4: create stores schema URL
    , testProperty "create stores schema URL" $
        forAll genSchemaUrl $ \url ->
          getSchemaUrl (create [] (Just url)) === Just url

    , testProperty "create with Nothing schema URL" $
        forAll genAttributes $ \attrs ->
          getSchemaUrl (create attrs Nothing) === Nothing

      -- Property: duplicate keys in the input list are resolved last-wins
      -- This underpins the env-var-wins precedence rule in applyResourceEnv.
    , testProperty "create with duplicate keys keeps last value" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v1 :: AttributeValue) ->
            forAll (arbitrary `suchThat` (/= v1)) $ \v2 ->
              lookup k (getAttributes (create [(k, v1), (k, v2)] Nothing)) === Just v2
    ]


-------------------------------------------------------------------------------
-- Resource: merge properties
-------------------------------------------------------------------------------

resourceMergeTests :: TestTree
resourceMergeTests =
  testGroup
    "Resource merge"
    [ -- Property 5: updating resource attributes take precedence on key conflict
      testProperty "updating resource wins on key conflict" $
        forAll (unShortKey <$> arbitrary) $ \k ->
          forAll arbitrary $ \(v1 :: AttributeValue) ->
            forAll (arbitrary `suchThat` (/= v1)) $ \v2 ->
              case merge (create [(k, v1)] Nothing) (create [(k, v2)] Nothing) of
                Right merged -> lookup k (getAttributes merged) === Just v2
                Left _ -> property False

      -- Property 6: non-conflicting attributes preserved from both
    , testProperty "non-conflicting attributes preserved from both" $
        forAll (unShortKey <$> arbitrary) $ \k1 ->
          forAll (arbitrary `suchThat` (\sk -> unShortKey sk /= k1)) $ \sk2 ->
            let k2 = unShortKey sk2
             in forAll arbitrary $ \(v1 :: AttributeValue) ->
                  forAll arbitrary $ \(v2 :: AttributeValue) ->
                    case merge (create [(k1, v1)] Nothing) (create [(k2, v2)] Nothing) of
                      Right merged ->
                        lookup k1 (getAttributes merged) === Just v1
                          .&&. lookup k2 (getAttributes merged) === Just v2
                      Left _ -> property False

      -- Property 7: schema URL conflict returns Left
    , testProperty "schema URL conflict returns Left ResourceMergeError" $
        forAll genDistinctSchemaUrls $ \(u1, u2) ->
          case merge (create [] (Just u1)) (create [] (Just u2)) of
            Left (ResourceMergeError s1 s2) -> s1 === u1 .&&. s2 === u2
            Right _ -> property False

      -- Property 8: same schema URL succeeds
    , testProperty "same schema URL succeeds" $
        forAll genSchemaUrl $ \url ->
          case merge (create [] (Just url)) (create [] (Just url)) of
            Right merged -> getSchemaUrl merged === Just url
            Left _ -> property False

      -- Property 9: one null schema URL succeeds, picks the non-null one
    , testProperty "one null schema URL: picks non-null (updating has URL)" $
        forAll genSchemaUrl $ \url ->
          case merge (create [] Nothing) (create [] (Just url)) of
            Right merged -> getSchemaUrl merged === Just url
            Left _ -> property False

    , testProperty "one null schema URL: picks non-null (old has URL)" $
        forAll genSchemaUrl $ \url ->
          case merge (create [] (Just url)) (create [] Nothing) of
            Right merged -> getSchemaUrl merged === Just url
            Left _ -> property False

      -- Property 10: empty is left identity
    , testProperty "merge empty r == Right r (left identity)" $
        forAll genResource $ \r ->
          merge empty r === Right r

      -- Property 11: empty is right identity
    , testProperty "merge r empty == Right r (right identity)" $
        forAll genResource $ \r ->
          merge r empty === Right r

      -- Bonus: merge two no-schema-URL resources always succeeds
    , testProperty "merge two no-schema-URL resources always succeeds" $
        forAll genAttributes $ \a1 ->
          forAll genAttributes $ \a2 ->
            case merge (create a1 Nothing) (create a2 Nothing) of
              Right _ -> property True
              Left _ -> property False

      -- Coverage check: verify generator distribution
    , testProperty "merge covers interesting schema URL combinations" $
        forAll genResource $ \r1 ->
          forAll genResource $ \r2 ->
            let tag = case (getSchemaUrl r1, getSchemaUrl r2) of
                  (Nothing, Nothing) -> "both-none" :: String
                  (Just _, Nothing) -> "old-only"
                  (Nothing, Just _) -> "new-only"
                  (Just a, Just b)
                    | a == b -> "same"
                    | otherwise -> "conflict"
             in cover 10 (tag == "both-none") "both-none" $
                  cover 10 (tag == "old-only") "old-only" $
                    cover 10 (tag == "new-only") "new-only" $
                      merge r1 r2 === merge r1 r2 -- tautology; we're checking coverage
    ]


-------------------------------------------------------------------------------
-- Resource detection
-------------------------------------------------------------------------------

resourceDetectionTests :: TestTree
resourceDetectionTests =
  testGroup
    "Resource detection"
    [ -- Property 13: detect [] returns empty
      testCase "detect [] returns empty" $ do
        r <- detect []
        r @?= empty

      -- Property 14: detect with a single detector returns that detector's resource
    , testProperty "detect [fixed] returns that resource" $
        forAll genResource $ \r ->
          ioProperty $ do
            result <- detect [SomeResourceDetector (FixedDetector r)]
            -- Merge with empty should produce the same resource
            pure (result === r)

      -- Property 15: throwing detector is suppressed (returns empty for that detector)
    , testCase "throwing detector is suppressed" $ do
        r <- detect [SomeResourceDetector ThrowingDetector]
        r @?= empty

    , testCase "throwing detector among others is suppressed" $ do
        let r1 = create [("k1", StringValue "v1")] Nothing
        result <- detect
          [ SomeResourceDetector (FixedDetector r1)
          , SomeResourceDetector ThrowingDetector
          ]
        -- r1 should be present, throwing detector contributes nothing
        assertBool "k1 present" (lookup "k1" (getAttributes result) == Just (StringValue "v1"))

      -- Property 16: detect merges multiple detectors' results
    , testCase "detect merges multiple detectors left-to-right" $ do
        let r1 = create [("k1", StringValue "v1"), ("shared", StringValue "from-r1")] Nothing
            r2 = create [("k2", StringValue "v2"), ("shared", StringValue "from-r2")] Nothing
        result <- detect
          [ SomeResourceDetector (FixedDetector r1)
          , SomeResourceDetector (FixedDetector r2)
          ]
        -- k1 from first detector
        lookup "k1" (getAttributes result) @?= Just (StringValue "v1")
        -- k2 from second detector
        lookup "k2" (getAttributes result) @?= Just (StringValue "v2")
        -- shared key: second detector (updating) wins
        lookup "shared" (getAttributes result) @?= Just (StringValue "from-r2")

    , testCase "detect drops detector on schema URL conflict" $ do
        let r1 = create [("k1", StringValue "v1")] (Just "https://schema.a")
            r2 = create [("k2", StringValue "v2")] (Just "https://schema.b")
        result <- detect
          [ SomeResourceDetector (FixedDetector r1)
          , SomeResourceDetector (FixedDetector r2)
          ]
        -- r2 is dropped due to schema conflict, r1's attributes and schema remain
        lookup "k1" (getAttributes result) @?= Just (StringValue "v1")
        lookup "k2" (getAttributes result) @?= Nothing
        getSchemaUrl result @?= Just "https://schema.a"
    ]


-------------------------------------------------------------------------------
-- Resource detectors (built-in)
-------------------------------------------------------------------------------

resourceDetectorTests :: TestTree
resourceDetectorTests =
  testGroup
    "Resource detectors"
    [ sdkDetectorTests
    , environmentDetectorTests
    , processDetectorTests
    , hostDetectorTests
    , osDetectorTests
    , detectorFailureTests
    , detectorMergingTests
    ]


sdkDetectorTests :: TestTree
sdkDetectorTests =
  testGroup
    "SdkResourceDetector"
    [ testCase "produces telemetry.sdk.name" $ do
        r <- detectResource SdkResourceDetector
        lookup "telemetry.sdk.name" (getAttributes r) @?= Just (StringValue "opentelemetry-haskell")

    , testCase "produces telemetry.sdk.version" $ do
        r <- detectResource SdkResourceDetector
        let v = lookup "telemetry.sdk.version" (getAttributes r)
        assertBool "telemetry.sdk.version should be present" (v /= Nothing)

    , testCase "produces telemetry.sdk.language = haskell" $ do
        r <- detectResource SdkResourceDetector
        lookup "telemetry.sdk.language" (getAttributes r) @?= Just (StringValue "haskell")

    , testProperty "SdkResourceDetector is deterministic (same result every call)" $
        once $ ioProperty $ do
          r1 <- detectResource SdkResourceDetector
          r2 <- detectResource SdkResourceDetector
          pure (r1 === r2)
    ]


environmentDetectorTests :: TestTree
environmentDetectorTests =
  testGroup
    "EnvironmentResourceDetector"
    [ testCase "reads OTEL_SERVICE_NAME" $ do
        setEnv "OTEL_SERVICE_NAME" "my-service"
        r <- detectResource EnvironmentResourceDetector
        unsetEnv "OTEL_SERVICE_NAME"
        lookup "service.name" (getAttributes r) @?= Just (StringValue "my-service")

    , testCase "reads OTEL_RESOURCE_ATTRIBUTES" $ do
        setEnv "OTEL_RESOURCE_ATTRIBUTES" "deployment.environment=prod,region=us-east"
        r <- detectResource EnvironmentResourceDetector
        unsetEnv "OTEL_RESOURCE_ATTRIBUTES"
        lookup "deployment.environment" (getAttributes r) @?= Just (StringValue "prod")
        lookup "region" (getAttributes r) @?= Just (StringValue "us-east")

    , testCase "empty env produces empty resource" $ do
        unsetEnv "OTEL_SERVICE_NAME"
        unsetEnv "OTEL_RESOURCE_ATTRIBUTES"
        r <- detectResource EnvironmentResourceDetector
        getAttributes r @?= emptyAttributes

    , testCase "OTEL_RESOURCE_ATTRIBUTES with whitespace is trimmed" $ do
        setEnv "OTEL_RESOURCE_ATTRIBUTES" " key1 = val1 , key2 = val2 "
        r <- detectResource EnvironmentResourceDetector
        unsetEnv "OTEL_RESOURCE_ATTRIBUTES"
        lookup "key1" (getAttributes r) @?= Just (StringValue "val1")
        lookup "key2" (getAttributes r) @?= Just (StringValue "val2")

    , testCase "OTEL_RESOURCE_ATTRIBUTES ignores malformed entries" $ do
        setEnv "OTEL_RESOURCE_ATTRIBUTES" "good=value,malformed,also.good=ok"
        r <- detectResource EnvironmentResourceDetector
        unsetEnv "OTEL_RESOURCE_ATTRIBUTES"
        lookup "good" (getAttributes r) @?= Just (StringValue "value")
        lookup "also.good" (getAttributes r) @?= Just (StringValue "ok")
        -- "malformed" has no = sign, should be skipped
        assertBool "malformed entry should not appear" (lookup "malformed" (getAttributes r) == Nothing)
    ]


processDetectorTests :: TestTree
processDetectorTests =
  testGroup
    "ProcessResourceDetector"
    [ testCase "produces process.executable.name" $ do
        r <- detectResource ProcessResourceDetector
        let v = lookup "process.executable.name" (getAttributes r)
        assertBool "process.executable.name should be present" (v /= Nothing)

    , testCase "produces process.executable.path" $ do
        r <- detectResource ProcessResourceDetector
        let v = lookup "process.executable.path" (getAttributes r)
        assertBool "process.executable.path should be present" (v /= Nothing)

    , testProperty "ProcessResourceDetector always returns two attributes" $
        once $ ioProperty $ do
          r <- detectResource ProcessResourceDetector
          pure (size (getAttributes r) === 2)
    ]


hostDetectorTests :: TestTree
hostDetectorTests =
  testGroup
    "HostResourceDetector"
    [ testCase "produces host.arch" $ do
        r <- detectResource HostResourceDetector
        let v = lookup "host.arch" (getAttributes r)
        assertBool "host.arch should be present" (v /= Nothing)

    , testCase "host.arch is a non-empty string" $ do
        r <- detectResource HostResourceDetector
        case lookup "host.arch" (getAttributes r) of
          Just (StringValue t) -> assertBool "host.arch should be non-empty" (not (Text.null t))
          other -> assertBool ("expected StringValue, got: " <> show other) False
    ]


osDetectorTests :: TestTree
osDetectorTests =
  testGroup
    "OsResourceDetector"
    [ testCase "produces os.type" $ do
        r <- detectResource OsResourceDetector
        let v = lookup "os.type" (getAttributes r)
        assertBool "os.type should be present" (v /= Nothing)

    , testCase "os.type is a non-empty string" $ do
        r <- detectResource OsResourceDetector
        case lookup "os.type" (getAttributes r) of
          Just (StringValue t) -> assertBool "os.type should be non-empty" (not (Text.null t))
          other -> assertBool ("expected StringValue, got: " <> show other) False
    ]


detectorFailureTests :: TestTree
detectorFailureTests =
  testGroup
    "Detector failure handling"
    [ testCase "throwing detector does not prevent resource creation" $ do
        r <- detect [SomeResourceDetector ThrowingDetector, SomeResourceDetector SdkResourceDetector]
        lookup "telemetry.sdk.name" (getAttributes r) @?= Just (StringValue "opentelemetry-haskell")

    , testCase "throwing detector returns empty resource contribution" $ do
        r <- detect [SomeResourceDetector ThrowingDetector]
        getAttributes r @?= emptyAttributes
    ]


detectorMergingTests :: TestTree
detectorMergingTests =
  testGroup
    "Detector merging"
    [ testCase "later detectors win on key conflict" $ do
        let d1 = SomeResourceDetector (FixedDetector (create [("service.name", StringValue "first")] Nothing))
            d2 = SomeResourceDetector (FixedDetector (create [("service.name", StringValue "second")] Nothing))
        r <- detect [d1, d2]
        lookup "service.name" (getAttributes r) @?= Just (StringValue "second")

    , testCase "detect with empty list returns empty resource" $ do
        r <- detect []
        getAttributes r @?= emptyAttributes

    , testCase "defaultResource always includes SDK attributes" $ do
        r <- defaultResource []
        lookup "telemetry.sdk.language" (getAttributes r) @?= Just (StringValue "haskell")

    , testCase "defaultResource extra detector attributes present" $ do
        let extra = SomeResourceDetector (FixedDetector (create [("custom.key", StringValue "custom-value")] Nothing))
        r <- defaultResource [extra]
        lookup "custom.key" (getAttributes r) @?= Just (StringValue "custom-value")
        lookup "telemetry.sdk.language" (getAttributes r) @?= Just (StringValue "haskell")
    ]


-------------------------------------------------------------------------------
-- ExportResult
-------------------------------------------------------------------------------

exportResultTests :: TestTree
exportResultTests =
  testGroup
    "ExportResult"
    [ -- Property 17: ExportSuccess /= ExportFailure
      testCase "ExportSuccess /= ExportFailure" $
        assertBool "constructors should differ" (ExportSuccess /= ExportFailure)

      -- Property 18: both constructors are constructible
    , testCase "ExportSuccess is constructible" $
        ExportSuccess @?= ExportSuccess
    , testCase "ExportFailure is constructible" $
        ExportFailure @?= ExportFailure

      -- Eq reflexivity
    , testCase "ExportSuccess == ExportSuccess" $
        assertBool "reflexive" (ExportSuccess == ExportSuccess)
    , testCase "ExportFailure == ExportFailure" $
        assertBool "reflexive" (ExportFailure == ExportFailure)

      -- Show is defined
    , testCase "show ExportSuccess" $
        show ExportSuccess @?= "ExportSuccess"
    , testCase "show ExportFailure" $
        show ExportFailure @?= "ExportFailure"
    ]
