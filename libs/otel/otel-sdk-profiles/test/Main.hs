module Main where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Either (isRight)
import OTel.Profile
  ( SomeProfilerProvider (..)
  , getGlobalProfilerProvider
  , setGlobalProfilerProvider
  )
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Profile
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (ioProperty, testProperty)


-- --------------------------------------------------------------------------
-- Recording exporter for observability in tests
-- --------------------------------------------------------------------------

data RecordingProfileExporter = RecordingProfileExporter
  { rpfExports :: !(TVar Int)
  , rpfShutdowns :: !(TVar Int)
  , rpfFlushes :: !(TVar Int)
  }

newRecordingProfileExporter :: IO RecordingProfileExporter
newRecordingProfileExporter =
  RecordingProfileExporter
    <$> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0

instance ProfileExporter RecordingProfileExporter where
  exportProfiles e = do
    atomically $ modifyTVar' (rpfExports e) (+ 1)
    pure ExportSuccess
  shutdownProfileExporter e = do
    atomically $ modifyTVar' (rpfShutdowns e) (+ 1)
    pure (Right ())
  forceFlushProfileExporter e _ = do
    atomically $ modifyTVar' (rpfFlushes e) (+ 1)
    pure (Right ())


-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

-- | Assert that an Either value is Right ().
assertRight :: (Show e) => Either e () -> Assertion
assertRight (Right ()) = pure ()
assertRight (Left e) = assertFailure $ "Expected Right (), got Left: " <> show e


-- --------------------------------------------------------------------------
-- Test tree
-- --------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "otel-sdk-profiles"
    [ noOpExporterTests
    , configTests
    , lifecycleTests
    , recordingExporterTests
    , profilerProviderClassTests
    , propertyTests
    ]


-- --------------------------------------------------------------------------
-- 1. NoOpProfileExporter
-- --------------------------------------------------------------------------

noOpExporterTests :: TestTree
noOpExporterTests =
  testGroup
    "NoOpProfileExporter"
    [ testCase "exportProfiles returns ExportSuccess" $ do
        r <- exportProfiles NoOpProfileExporter
        r @?= ExportSuccess

    , testCase "shutdownProfileExporter returns Right ()" $ do
        r <- shutdownProfileExporter NoOpProfileExporter
        assertRight r

    , testCase "forceFlushProfileExporter returns Right ()" $ do
        r <- forceFlushProfileExporter NoOpProfileExporter Nothing
        assertRight r

    , testCase "SomeProfileExporter delegates exportProfiles" $ do
        let wrapped = SomeProfileExporter NoOpProfileExporter
        r <- exportProfiles wrapped
        r @?= ExportSuccess

    , testCase "SomeProfileExporter delegates shutdownProfileExporter" $ do
        let wrapped = SomeProfileExporter NoOpProfileExporter
        r <- shutdownProfileExporter wrapped
        assertRight r

    , testCase "SomeProfileExporter delegates forceFlushProfileExporter" $ do
        let wrapped = SomeProfileExporter NoOpProfileExporter
        r <- forceFlushProfileExporter wrapped Nothing
        assertRight r
    ]


-- --------------------------------------------------------------------------
-- 2. SdkProfilerProviderConfig defaults
-- --------------------------------------------------------------------------

configTests :: TestTree
configTests =
  testGroup
    "SdkProfilerProviderConfig"
    [ testCase "default exporters list is empty" $
        assertBool "Expected empty exporter list" $
          null (sppExporters defaultSdkProfilerProviderConfig)

    , testCase "Show instance produces non-empty string" $
        assertBool "Show output should not be empty" $
          not (null (show defaultSdkProfilerProviderConfig))
    ]


-- --------------------------------------------------------------------------
-- 3. SdkProfilerProvider lifecycle (no exporters)
-- --------------------------------------------------------------------------

lifecycleTests :: TestTree
lifecycleTests =
  testGroup
    "SdkProfilerProvider lifecycle"
    [ testCase "newSdkProfilerProvider succeeds" $ do
        _p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        pure ()

    , testCase "shutdown returns Right () with no exporters" $ do
        p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        r <- sdkProfilerProviderShutdown p
        assertRight r

    , testCase "forceFlush returns Right () with no exporters" $ do
        p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        r <- sdkProfilerProviderForceFlush p Nothing
        assertRight r

    , testCase "second shutdown is safe (idempotent)" $ do
        p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        r1 <- sdkProfilerProviderShutdown p
        r2 <- sdkProfilerProviderShutdown p
        assertRight r1
        assertRight r2
    ]


-- --------------------------------------------------------------------------
-- 4. SdkProfilerProvider with recording exporter
-- --------------------------------------------------------------------------

recordingExporterTests :: TestTree
recordingExporterTests =
  testGroup
    "SdkProfilerProvider with RecordingProfileExporter"
    [ testCase "shutdown calls through to exporter's shutdown" $ do
        rec' <- newRecordingProfileExporter
        p <- newSdkProfilerProvider
          defaultSdkProfilerProviderConfig
            { sppExporters = [SomeProfileExporter rec'] }
        _ <- sdkProfilerProviderShutdown p
        count <- readTVarIO (rpfShutdowns rec')
        count @?= 1

    , testCase "forceFlush calls through to exporter's flush" $ do
        rec' <- newRecordingProfileExporter
        p <- newSdkProfilerProvider
          defaultSdkProfilerProviderConfig
            { sppExporters = [SomeProfileExporter rec'] }
        _ <- sdkProfilerProviderForceFlush p Nothing
        count <- readTVarIO (rpfFlushes rec')
        count @?= 1

    , testCase "multiple exporters: all are shut down on provider shutdown" $ do
        rec1 <- newRecordingProfileExporter
        rec2 <- newRecordingProfileExporter
        rec3 <- newRecordingProfileExporter
        p <- newSdkProfilerProvider
          defaultSdkProfilerProviderConfig
            { sppExporters =
                [ SomeProfileExporter rec1
                , SomeProfileExporter rec2
                , SomeProfileExporter rec3
                ]
            }
        r <- sdkProfilerProviderShutdown p
        assertRight r
        c1 <- readTVarIO (rpfShutdowns rec1)
        c2 <- readTVarIO (rpfShutdowns rec2)
        c3 <- readTVarIO (rpfShutdowns rec3)
        c1 @?= 1
        c2 @?= 1
        c3 @?= 1
    ]


-- --------------------------------------------------------------------------
-- 5. ProfilerProvider type class
-- --------------------------------------------------------------------------

profilerProviderClassTests :: TestTree
profilerProviderClassTests =
  testGroup
    "ProfilerProvider type class"
    [ testCase "SdkProfilerProvider wraps in SomeProfilerProvider" $ do
        p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        let _wrapped = SomeProfilerProvider p
        pure ()

    , testCase "setGlobalProfilerProvider / getGlobalProfilerProvider round-trip" $ do
        p <- newSdkProfilerProvider defaultSdkProfilerProviderConfig
        let sp = SomeProfilerProvider p
        setGlobalProfilerProvider sp
        _got <- getGlobalProfilerProvider
        -- The global ref stores a SomeProfilerProvider; we cannot
        -- test equality (no Eq instance on existential), but the
        -- round-trip itself must not throw.
        pure ()
    ]


-- --------------------------------------------------------------------------
-- 6. Property: shutdown idempotence
-- --------------------------------------------------------------------------

propertyTests :: TestTree
propertyTests =
  testGroup
    "Properties"
    [ testProperty "shutdown is idempotent for NoOpProfileExporter" $
        -- The property is trivially total over its (empty) input space:
        -- two consecutive shutdowns must both succeed.
        \() -> ioProperty $ do
          r1 <- shutdownProfileExporter NoOpProfileExporter
          r2 <- shutdownProfileExporter NoOpProfileExporter
          pure (isRight r1 && isRight r2)
    ]
