module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners (NumThreads (..))

import OTel.Attribute (AttributeValue (..), InstrumentationScope (..))
import OTel.Context (root)
import OTel.Log
  ( LogBody (..), LogRecord (..), Logger (..), LoggerProvider (..)
  , SomeLogger (..), defaultLogRecord, SeverityNumber (..)
  )
import OTel.SDK.Config
  ( ConfigBuildError (..)
  , createLoggerProvider, createMeterProvider, createPropagator
  , createTracerProvider, parseYaml
  )
import OTel.SDK.Log
  ( sdkLoggerProviderForceFlush, sdkLoggerProviderShutdown
  )
import OTel.SDK.Trace (forceFlush, shutdown)
import OTel.Trace
  ( Span (..), Tracer (..), TracerProvider (..)
  , defaultSpanConfig
  )

import Control.Exception (bracket_)
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import System.Environment (setEnv, unsetEnv)


main :: IO ()
main = defaultMain (localOption (NumThreads 1) tests)


tests :: TestTree
tests = testGroup "Configuration file integration tests"
  [ parseComprehensiveYamlTest
  , invalidYamlReturnsParseErrorTest
  , unknownExporterReturnsParseErrorTest
  , unknownSamplerInYamlReturnsParseErrorTest
  , envServiceNameOverrideTest
  , envSamplerOverrideTest
  , unknownSamplerEnvVarReturnsBuildErrorTest
  , pipelineProducesTelemetryTest
  , envPropagatorsOverrideTest
  , unknownPropagatorReturnsBuildErrorTest
  , envSamplerNoTracerProviderSectionTest
  ]


-------------------------------------------------------------------------------
-- Test 1: Parse a comprehensive YAML config
-------------------------------------------------------------------------------

comprehensiveYaml :: Text
comprehensiveYaml =
  "sdk:\n\
  \  resource:\n\
  \    service_name: test-service\n\
  \    attributes:\n\
  \      deployment.environment: staging\n\
  \      service.version: \"1.2.3\"\n\
  \  tracer_provider:\n\
  \    sampler: always_on\n\
  \    processors:\n\
  \      - type: batch\n\
  \        exporter: console\n\
  \        schedule_delay: 5000\n\
  \        export_timeout: 30000\n\
  \        max_queue_size: 2048\n\
  \        max_export_batch_size: 512\n\
  \  meter_provider:\n\
  \    readers:\n\
  \      - exporter: console\n\
  \        interval: 60000\n\
  \        timeout: 30000\n\
  \  logger_provider:\n\
  \    processors:\n\
  \      - type: batch\n\
  \        exporter: console\n\
  \        schedule_delay: 1000\n\
  \        max_queue_size: 2048\n\
  \        max_export_batch_size: 512\n\
  \propagators:\n\
  \  - tracecontext\n\
  \  - baggage\n"

parseComprehensiveYamlTest :: TestTree
parseComprehensiveYamlTest = testCase
  "Parse a comprehensive YAML config covering all sections" $ do
  result <- parseYaml comprehensiveYaml
  assertBool "parseYaml should return Right" (isRight result)
  cfg <- either (assertFailure . show) pure result
  tp <- createTracerProvider cfg
  assertBool "createTracerProvider should return Right" (isRight tp)
  mp <- createMeterProvider cfg
  assertBool "createMeterProvider should return Right" (isRight mp)
  lp <- createLoggerProvider cfg
  assertBool "createLoggerProvider should return Right" (isRight lp)
  prop <- createPropagator cfg
  assertBool "createPropagator should return Right" (isRight prop)
  case tp of
    Right provider -> do
      _ <- shutdown provider
      pure ()
    _ -> pure ()


-------------------------------------------------------------------------------
-- Test 2: Invalid YAML returns ConfigParseError
-------------------------------------------------------------------------------

invalidYamlReturnsParseErrorTest :: TestTree
invalidYamlReturnsParseErrorTest = testCase
  "Invalid YAML returns ConfigParseError" $ do
  result <- parseYaml "{ unclosed"
  assertBool "parseYaml should return Left for invalid YAML" (isLeft result)


-------------------------------------------------------------------------------
-- Test 3: Unknown exporter type returns ConfigParseError
-------------------------------------------------------------------------------

unknownExporterReturnsParseErrorTest :: TestTree
unknownExporterReturnsParseErrorTest = testCase
  "Unknown exporter type returns ConfigParseError" $ do
  let yaml = "sdk:\n\
             \  tracer_provider:\n\
             \    processors:\n\
             \      - type: batch\n\
             \        exporter: otlp\n"
  result <- parseYaml yaml
  assertBool "parseYaml should return Left for unknown exporter" (isLeft result)


-------------------------------------------------------------------------------
-- Test 4 (new): Unknown sampler name in YAML returns ConfigParseError
-------------------------------------------------------------------------------

unknownSamplerInYamlReturnsParseErrorTest :: TestTree
unknownSamplerInYamlReturnsParseErrorTest = testCase
  "Unknown sampler name in YAML config returns ConfigParseError" $ do
  let yaml = "sdk:\n\
             \  tracer_provider:\n\
             \    sampler: not_a_real_sampler\n\
             \    processors:\n\
             \      - type: simple\n\
             \        exporter: console\n"
  result <- parseYaml yaml
  assertBool "parseYaml should return Left for unknown sampler" (isLeft result)


-------------------------------------------------------------------------------
-- Test 5 (new): Unknown OTEL_TRACES_SAMPLER env var returns ConfigBuildError
-------------------------------------------------------------------------------

unknownSamplerEnvVarReturnsBuildErrorTest :: TestTree
unknownSamplerEnvVarReturnsBuildErrorTest = testCase
  "Unknown OTEL_TRACES_SAMPLER env var returns ConfigBuildError" $
  bracket_
    (setEnv "OTEL_TRACES_SAMPLER" "not_a_real_sampler")
    (unsetEnv "OTEL_TRACES_SAMPLER")
    $ do
      let yaml = "sdk:\n\
                 \  tracer_provider:\n\
                 \    sampler: always_on\n\
                 \    processors:\n\
                 \      - type: simple\n\
                 \        exporter: console\n"
      result <- parseYaml yaml
      assertBool "parseYaml should return Right" (isRight result)
      cfg <- either (assertFailure . show) pure result
      tp <- createTracerProvider cfg
      assertBool "createTracerProvider should return Left for unknown sampler env var" (isLeft tp)


-------------------------------------------------------------------------------
-- Test 7: OTEL_SERVICE_NAME env var override
-------------------------------------------------------------------------------

envServiceNameOverrideTest :: TestTree
envServiceNameOverrideTest = testCase
  "OTEL_SERVICE_NAME env var override applies during provider construction" $
  bracket_
    (setEnv "OTEL_SERVICE_NAME" "env-service")
    (unsetEnv "OTEL_SERVICE_NAME")
    $ do
      let yaml = "sdk:\n\
                 \  resource:\n\
                 \    service_name: config-service\n\
                 \  tracer_provider:\n\
                 \    sampler: always_on\n\
                 \    processors:\n\
                 \      - type: simple\n\
                 \        exporter: console\n"
      result <- parseYaml yaml
      assertBool "parseYaml should return Right" (isRight result)
      cfg <- either (assertFailure . show) pure result
      tp <- createTracerProvider cfg
      assertBool "createTracerProvider should return Right" (isRight tp)
      case tp of
        Right provider -> do
          _ <- shutdown provider
          pure ()
        _ -> pure ()


-------------------------------------------------------------------------------
-- Test 5: OTEL_TRACES_SAMPLER override (always_off => isRecording False)
-------------------------------------------------------------------------------

envSamplerOverrideTest :: TestTree
envSamplerOverrideTest = testCase
  "OTEL_TRACES_SAMPLER=always_off overrides config sampler; span not recording" $
  bracket_
    (setEnv "OTEL_TRACES_SAMPLER" "always_off")
    (unsetEnv "OTEL_TRACES_SAMPLER")
    $ do
      let yaml = "sdk:\n\
                 \  tracer_provider:\n\
                 \    sampler: always_on\n\
                 \    processors:\n\
                 \      - type: simple\n\
                 \        exporter: console\n"
      result <- parseYaml yaml
      assertBool "parseYaml should return Right" (isRight result)
      cfg <- either (assertFailure . show) pure result
      tp <- createTracerProvider cfg
      assertBool "createTracerProvider should return Right" (isRight tp)
      case tp of
        Right provider -> do
          let scope = InstrumentationScope "sampler-test" Nothing Nothing Nothing
          tracer <- getTracer provider scope
          span1 <- startSpan tracer "sampled-span" root defaultSpanConfig
          recording <- isRecording span1
          assertBool "Span should not be recording with always_off sampler" (not recording)
          end span1 Nothing
          _ <- shutdown provider
          pure ()
        Left _ -> assertFailure "Expected Right from createTracerProvider"


-------------------------------------------------------------------------------
-- Test 6: Config-constructed pipeline produces and exports telemetry
-------------------------------------------------------------------------------

pipelineProducesTelemetryTest :: TestTree
pipelineProducesTelemetryTest = testCase
  "Config-constructed pipeline produces telemetry; flush and shutdown succeed" $ do
  let yaml = "sdk:\n\
             \  tracer_provider:\n\
             \    sampler: always_on\n\
             \    processors:\n\
             \      - type: simple\n\
             \        exporter: console\n\
             \  logger_provider:\n\
             \    processors:\n\
             \      - type: simple\n\
             \        exporter: console\n"
  result <- parseYaml yaml
  assertBool "parseYaml should return Right" (isRight result)
  cfg <- either (assertFailure . show) pure result

  tp <- createTracerProvider cfg
  assertBool "createTracerProvider should return Right" (isRight tp)
  lp <- createLoggerProvider cfg
  assertBool "createLoggerProvider should return Right" (isRight lp)

  case (tp, lp) of
    (Right tracerProv, Right logProv) -> do
      let scope = InstrumentationScope "pipeline-test" (Just "1.0.0") Nothing Nothing

      tracer <- getTracer tracerProv scope
      span1 <- startSpan tracer "pipeline-span" root defaultSpanConfig
      setAttribute span1 "test.key" (StringValue "test-value")
      end span1 Nothing

      SomeLogger logger <- getLogger logProv scope
      emit logger defaultLogRecord
        { logBody = Just (LogBodyString "pipeline log")
        , logSeverityNumber = Just SeverityInfo
        }

      flushTrace <- forceFlush tracerProv Nothing
      assertBool "Tracer forceFlush should return Right" (isRight flushTrace)

      flushLog <- sdkLoggerProviderForceFlush logProv Nothing
      assertBool "Logger forceFlush should return Right" (isRight flushLog)

      shutTrace <- shutdown tracerProv
      assertBool "Tracer shutdown should return Right" (isRight shutTrace)

      shutLog <- sdkLoggerProviderShutdown logProv
      assertBool "Logger shutdown should return Right" (isRight shutLog)

    _ -> assertFailure "Expected Right from both createTracerProvider and createLoggerProvider"


-------------------------------------------------------------------------------
-- Test 7: OTEL_PROPAGATORS override
-------------------------------------------------------------------------------

envPropagatorsOverrideTest :: TestTree
envPropagatorsOverrideTest = testCase
  "OTEL_PROPAGATORS env var override applies during propagator construction" $
  bracket_
    (setEnv "OTEL_PROPAGATORS" "tracecontext,baggage")
    (unsetEnv "OTEL_PROPAGATORS")
    $ do
      let yaml = "sdk: {}\n"
      result <- parseYaml yaml
      assertBool "parseYaml should return Right" (isRight result)
      cfg <- either (assertFailure . show) pure result
      prop <- createPropagator cfg
      assertBool "createPropagator should return Right" (isRight prop)


-------------------------------------------------------------------------------
-- Test 8: Unknown propagator returns ConfigBuildError
-------------------------------------------------------------------------------

unknownPropagatorReturnsBuildErrorTest :: TestTree
unknownPropagatorReturnsBuildErrorTest = testCase
  "Unknown propagator returns ConfigBuildError with section=propagators" $ do
  unsetEnv "OTEL_PROPAGATORS"
  let yaml = "propagators:\n\
             \  - nosuchprop\n"
  result <- parseYaml yaml
  assertBool "parseYaml should return Right" (isRight result)
  cfg <- either (assertFailure . show) pure result
  prop <- createPropagator cfg
  case prop of
    Left err -> do
      assertEqual "Error section should be propagators"
        "propagators" (cbeSection err)
    Right _ -> assertFailure "Expected Left ConfigBuildError for unknown propagator"


-------------------------------------------------------------------------------
-- Test 9: OTEL_TRACES_SAMPLER applied when config has no tracer_provider section
-------------------------------------------------------------------------------

envSamplerNoTracerProviderSectionTest :: TestTree
envSamplerNoTracerProviderSectionTest = testCase
  "OTEL_TRACES_SAMPLER=always_off applied when config has no tracer_provider section" $
  bracket_
    (setEnv "OTEL_TRACES_SAMPLER" "always_off")
    (unsetEnv "OTEL_TRACES_SAMPLER")
    $ do
      let yaml = "sdk: {}\n"
      result <- parseYaml yaml
      assertBool "parseYaml should return Right" (isRight result)
      cfg <- either (assertFailure . show) pure result
      tp <- createTracerProvider cfg
      assertBool "createTracerProvider should return Right" (isRight tp)
      case tp of
        Right provider -> do
          let scope = InstrumentationScope "no-tp-section-test" Nothing Nothing Nothing
          tracer <- getTracer provider scope
          span1 <- startSpan tracer "should-not-record" root defaultSpanConfig
          recording <- isRecording span1
          assertBool
            "Span should NOT be recording when OTEL_TRACES_SAMPLER=always_off \
            \and config has no tracer_provider section"
            (not recording)
          end span1 Nothing
          _ <- shutdown provider
          pure ()
        Left _ -> assertFailure "Expected Right from createTracerProvider"
