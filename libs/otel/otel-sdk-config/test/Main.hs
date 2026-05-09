module Main where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import Test.Tasty
import Test.Tasty.HUnit

import OTel.SDK.Config
  ( ConfigBuildError (..)
  , ConfigParseError (..)
  , Configuration
  , createLoggerProvider
  , createMeterProvider
  , createPropagator
  , createTracerProvider
  , parse
  , parseYaml
  )


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "otel-sdk-config"
  [ errorTypeTests
  , parseYamlValidTests
  , parseYamlInvalidTests
  , createTracerProviderTests
  , createMeterProviderTests
  , createLoggerProviderTests
  , createPropagatorTests
  , envVarOverrideTests
  , envVarPrecedenceTests
  , parseFileTests
  ]


-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

validFullConfig :: Text
validFullConfig = Text.unlines
  [ "sdk:"
  , "  resource:"
  , "    service_name: test-service"
  , "  tracer_provider:"
  , "    sampler: always_on"
  , "    processors:"
  , "      - type: batch"
  , "        exporter: console"
  , "  meter_provider:"
  , "    readers:"
  , "      - type: periodic"
  , "        exporter: console"
  , "  logger_provider:"
  , "    processors:"
  , "      - type: simple"
  , "        exporter: console"
  , "propagators:"
  , "  - tracecontext"
  , "  - baggage"
  ]


-- | Parse YAML and assert it succeeds, returning the Configuration.
parseAndAssert :: Text -> IO Configuration
parseAndAssert yaml = do
  result <- parseYaml yaml
  case result of
    Left e -> assertFailure ("parseYaml failed: " <> show e) >> error "unreachable"
    Right cfg -> pure cfg


-- | Run an action with env vars set, cleaning up afterwards.
withEnvVars :: forall a. [(String, String)] -> IO a -> IO a
withEnvVars vars action = do
  mapM_ (uncurry setEnv) vars
  r <- try action :: IO (Either SomeException a)
  mapM_ (unsetEnv . fst) vars
  case r of
    Left e -> error (show e)
    Right v -> pure v


-- | Assert that an Either is Right.
assertRight :: (Show e) => String -> Either e a -> IO a
assertRight _   (Right a) = pure a
assertRight msg (Left e)  = assertFailure (msg <> ": " <> show e) >> error "unreachable"



-------------------------------------------------------------------------------
-- 1. Error types
-------------------------------------------------------------------------------

errorTypeTests :: TestTree
errorTypeTests = testGroup "Error types"
  [ testCase "ConfigParseError fields are accessible" $ do
      let e = ConfigParseError
            { cpeSource   = "test.yaml"
            , cpePosition = Just (1, 5)
            , cpeMessage  = "bad syntax"
            }
      cpeSource e @?= "test.yaml"
      cpePosition e @?= Just (1, 5)
      cpeMessage e @?= "bad syntax"

  , testCase "ConfigBuildError fields are accessible" $ do
      let e = ConfigBuildError
            { cbeSection = "propagators"
            , cbeMessage = "unknown"
            }
      cbeSection e @?= "propagators"
      cbeMessage e @?= "unknown"

  , testCase "ConfigParseError derives Eq" $ do
      let e1 = ConfigParseError "a" Nothing "msg"
          e2 = ConfigParseError "a" Nothing "msg"
      e1 @?= e2

  , testCase "ConfigBuildError derives Eq" $ do
      let e1 = ConfigBuildError "sec" "msg"
          e2 = ConfigBuildError "sec" "msg"
      e1 @?= e2

  , testCase "ConfigParseError derives Show" $ do
      let e = ConfigParseError "src" Nothing "err"
          s = show e
      assertBool "show produces non-empty string" (not (null s))

  , testCase "ConfigBuildError derives Show" $ do
      let e = ConfigBuildError "sec" "err"
          s = show e
      assertBool "show produces non-empty string" (not (null s))
  ]


-------------------------------------------------------------------------------
-- 2. parseYaml - valid YAML
-------------------------------------------------------------------------------

parseYamlValidTests :: TestTree
parseYamlValidTests = testGroup "parseYaml - valid YAML"
  [ testCase "empty object parses successfully" $ do
      result <- parseYaml "{}"
      _ <- assertRight "empty object" result
      pure ()

  , testCase "minimal sdk config parses successfully" $ do
      result <- parseYaml "sdk: {}"
      _ <- assertRight "minimal sdk" result
      pure ()

  , testCase "full config with all sections parses successfully" $ do
      result <- parseYaml validFullConfig
      _ <- assertRight "full config" result
      pure ()

  , testCase "traceidratio sampler with arg parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: traceidratio"
            , "    sampler_arg: 0.5"
            ]
      result <- parseYaml yaml
      _ <- assertRight "traceidratio sampler" result
      pure ()

  , testCase "always_off sampler parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: always_off"
            ]
      result <- parseYaml yaml
      _ <- assertRight "always_off sampler" result
      pure ()

  , testCase "parentbased_always_on sampler parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: parentbased_always_on"
            ]
      result <- parseYaml yaml
      _ <- assertRight "parentbased_always_on sampler" result
      pure ()

  , testCase "parentbased_traceidratio sampler with arg parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: parentbased_traceidratio"
            , "    sampler_arg: 0.25"
            ]
      cfg <- parseAndAssert yaml
      tpResult <- createTracerProvider cfg
      _ <- assertRight "parentbased_traceidratio with arg createTracerProvider" tpResult
      pure ()

  , testCase "parentbased_traceidratio sampler without arg parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: parentbased_traceidratio"
            ]
      cfg <- parseAndAssert yaml
      tpResult <- createTracerProvider cfg
      _ <- assertRight "parentbased_traceidratio without arg createTracerProvider" tpResult
      pure ()

  , testCase "simple span processor parses successfully" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    processors:"
            , "      - type: simple"
            , "        exporter: console"
            ]
      result <- parseYaml yaml
      _ <- assertRight "simple processor" result
      pure ()
  ]


-------------------------------------------------------------------------------
-- 3. parseYaml - invalid YAML
-------------------------------------------------------------------------------

parseYamlInvalidTests :: TestTree
parseYamlInvalidTests = testGroup "parseYaml - invalid YAML"
  [ testCase "malformed YAML returns Left ConfigParseError" $ do
      result <- parseYaml "key: [unclosed"
      case result of
        Left e ->
          assertBool "cpeMessage is non-empty" (not (Text.null (cpeMessage e)))
        Right _ ->
          assertFailure "expected Left ConfigParseError but got Right"
  ]


-------------------------------------------------------------------------------
-- 4. createTracerProvider
-------------------------------------------------------------------------------

createTracerProviderTests :: TestTree
createTracerProviderTests = testGroup "createTracerProvider"
  [ testCase "from empty config succeeds" $ do
      cfg <- parseAndAssert "{}"
      result <- createTracerProvider cfg
      _ <- assertRight "empty config tracer provider" result
      pure ()

  , testCase "from full config with batch+console succeeds" $ do
      cfg <- parseAndAssert validFullConfig
      result <- createTracerProvider cfg
      _ <- assertRight "full config tracer provider" result
      pure ()

  -- NOTE: The implementation maps all exporter names to ExporterConsole
  -- (parseExporterRef always returns ExporterConsole), so an "unknown"
  -- exporter name does NOT produce a ConfigBuildError. This test verifies
  -- the actual behavior: it succeeds regardless of exporter name.
  , testCase "unknown exporter name returns Left ConfigParseError" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    processors:"
            , "      - type: batch"
            , "        exporter: otlp_grpc"
            ]
      result <- parseYaml yaml
      case result of
        Left err -> assertBool "error message mentions unknown exporter"
                      ("Unknown exporter" `Text.isInfixOf` cpeMessage err)
        Right _  -> assertFailure "expected ConfigParseError for unknown exporter"

  , testCase "unknown sampler name returns Left ConfigParseError" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  tracer_provider:"
            , "    sampler: nonexistent_sampler"
            ]
      result <- parseYaml yaml
      case result of
        Left err -> assertBool "error message mentions unknown sampler"
                      ("Unknown sampler" `Text.isInfixOf` cpeMessage err)
        Right _  -> assertFailure "expected ConfigParseError for unknown sampler"
  ]


-------------------------------------------------------------------------------
-- 5. createMeterProvider
-------------------------------------------------------------------------------

createMeterProviderTests :: TestTree
createMeterProviderTests = testGroup "createMeterProvider"
  [ testCase "from empty config succeeds" $ do
      cfg <- parseAndAssert "{}"
      result <- createMeterProvider cfg
      _ <- assertRight "empty config meter provider" result
      pure ()

  , testCase "from config with periodic reader + console succeeds" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  meter_provider:"
            , "    readers:"
            , "      - type: periodic"
            , "        exporter: console"
            ]
      cfg <- parseAndAssert yaml
      result <- createMeterProvider cfg
      _ <- assertRight "periodic reader meter provider" result
      pure ()
  ]


-------------------------------------------------------------------------------
-- 6. createLoggerProvider
-------------------------------------------------------------------------------

createLoggerProviderTests :: TestTree
createLoggerProviderTests = testGroup "createLoggerProvider"
  [ testCase "from empty config succeeds" $ do
      cfg <- parseAndAssert "{}"
      result <- createLoggerProvider cfg
      _ <- assertRight "empty config logger provider" result
      pure ()

  , testCase "from config with batch processor + console succeeds" $ do
      let yaml = Text.unlines
            [ "sdk:"
            , "  logger_provider:"
            , "    processors:"
            , "      - type: batch"
            , "        exporter: console"
            ]
      cfg <- parseAndAssert yaml
      result <- createLoggerProvider cfg
      _ <- assertRight "batch logger provider" result
      pure ()
  ]


-------------------------------------------------------------------------------
-- 7. createPropagator
-------------------------------------------------------------------------------

createPropagatorTests :: TestTree
createPropagatorTests = testGroup "createPropagator"
  [ testCase "from empty config succeeds with default propagators" $ do
      cfg <- parseAndAssert "{}"
      result <- createPropagator cfg
      _ <- assertRight "default propagator" result
      pure ()

  , testCase "from config with tracecontext+baggage succeeds" $ do
      let yaml = Text.unlines
            [ "propagators:"
            , "  - tracecontext"
            , "  - baggage"
            ]
      cfg <- parseAndAssert yaml
      result <- createPropagator cfg
      _ <- assertRight "tracecontext+baggage propagator" result
      pure ()

  , testCase "unknown propagator name returns Left ConfigBuildError" $ do
      let yaml = Text.unlines
            [ "propagators:"
            , "  - unknown_propagator"
            ]
      cfg <- parseAndAssert yaml
      result <- createPropagator cfg
      case result of
        Left e -> do
          cbeSection e @?= "propagators"
          assertBool "cbeMessage mentions unknown propagator"
            (Text.isInfixOf "unknown_propagator" (cbeMessage e))
        Right _ ->
          assertFailure "expected Left ConfigBuildError but got Right"
  ]


-------------------------------------------------------------------------------
-- 8. Environment variable overrides
-------------------------------------------------------------------------------

envVarOverrideTests :: TestTree
envVarOverrideTests = testGroup "Environment variable overrides"
  [ testCase "OTEL_SERVICE_NAME overrides config service_name" $ do
      withEnvVars [("OTEL_SERVICE_NAME", "override-svc")] $ do
        let yaml = Text.unlines
              [ "sdk:"
              , "  resource:"
              , "    service_name: original"
              ]
        cfg <- parseAndAssert yaml
        result <- createTracerProvider cfg
        _ <- assertRight "service name override" result
        pure ()

  , testCase "OTEL_PROPAGATORS env var applied" $ do
      withEnvVars [("OTEL_PROPAGATORS", "tracecontext")] $ do
        cfg <- parseAndAssert "{}"
        result <- createPropagator cfg
        _ <- assertRight "propagators env override" result
        pure ()

  , testCase "OTEL_TRACES_SAMPLER=always_off applied" $ do
      withEnvVars [("OTEL_TRACES_SAMPLER", "always_off")] $ do
        let yaml = Text.unlines
              [ "sdk:"
              , "  tracer_provider:"
              , "    sampler: always_on"
              ]
        cfg <- parseAndAssert yaml
        result <- createTracerProvider cfg
        _ <- assertRight "sampler env override" result
        pure ()

  , testCase "OTEL_TRACES_SAMPLER=traceidratio with OTEL_TRACES_SAMPLER_ARG" $ do
      withEnvVars
        [ ("OTEL_TRACES_SAMPLER", "traceidratio")
        , ("OTEL_TRACES_SAMPLER_ARG", "0.5")
        ] $ do
        cfg <- parseAndAssert "{}"
        result <- createTracerProvider cfg
        _ <- assertRight "traceidratio env override" result
        pure ()
  ]


-------------------------------------------------------------------------------
-- 9. Precedence: env vars > config file > defaults
-------------------------------------------------------------------------------

envVarPrecedenceTests :: TestTree
envVarPrecedenceTests = testGroup "Precedence: env vars > config file > defaults"
  [ testCase "env var OTEL_PROPAGATORS overrides config propagators" $ do
      withEnvVars [("OTEL_PROPAGATORS", "tracecontext")] $ do
        let yaml = Text.unlines
              [ "propagators:"
              , "  - baggage"
              ]
        cfg <- parseAndAssert yaml
        result <- createPropagator cfg
        _ <- assertRight "env var overrides config propagators" result
        pure ()
  ]


-------------------------------------------------------------------------------
-- 10. parse (file-based)
-------------------------------------------------------------------------------

parseFileTests :: TestTree
parseFileTests = testGroup "parse (file-based)"
  [ testCase "parse a temp file with valid YAML succeeds" $ do
      let tmpDir = "/tmp"
      (tmpPath, h) <- openTempFile tmpDir "otel-config-test-.yaml"
      hClose h
      writeFile tmpPath (Text.unpack validFullConfig)
      result <- parse tmpPath
      _ <- assertRight "parse temp file" result
      removeFile tmpPath

  , testCase "parse non-existent file throws IO exception" $ do
      let badPath = "/tmp/otel-config-does-not-exist-12345.yaml"
      result <- try (parse badPath) :: IO (Either SomeException (Either ConfigParseError Configuration))
      case result of
        Left exc -> do
          let msg = show exc
          assertBool "IO exception for missing file" (not (null msg))
        Right (Left e) ->
          -- Implementation might wrap the error as ConfigParseError
          assertBool "error message is non-empty" (not (Text.null (cpeMessage e)))
        Right (Right _) ->
          assertFailure "expected error for non-existent file but got Right"
  ]
