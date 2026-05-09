module Main (main) where

import Prelude hiding (log)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H

import Telemetry.Core

main :: IO ()
main = defaultMain $ testGroup "telemetry-core"
  [ testGroup "Severity ordering"
      [ testProperty "DEBUG < INFO < WARN < ERROR" prop_severityOrdering
      ]
  , testGroup "No-op backend tracing"
      [ testProperty "withSpan runs action and returns result" prop_withSpanNoOp
      , testProperty "getCurrentSpanContext returns Nothing" prop_noSpanContext
      , testProperty "startSpan/endSpan succeed" prop_startEndNoOp
      , testProperty "addAttribute on NoOpSpan succeeds" prop_addAttrNoOp
      ]
  , testGroup "No-op backend logging"
      [ testProperty "log succeeds silently" prop_logNoOp
      ]
  , testGroup "No-op backend metrics"
      [ testProperty "incrementCounter succeeds" prop_counterNoOp
      , testProperty "recordGauge succeeds" prop_gaugeNoOp
      , testProperty "recordHistogram succeeds" prop_histogramNoOp
      ]
  , testGroup "Initialization"
      [ testProperty "initTelemetry NoOpConfig succeeds" prop_initNoOp
      , testProperty "shutdownTelemetry succeeds" prop_shutdown
      ]
  ]

prop_severityOrdering :: H.Property
prop_severityOrdering = H.withTests 1 $ H.property $ do
  H.assert (DEBUG < INFO)
  H.assert (INFO < WARN)
  H.assert (WARN < ERROR)
  H.assert (DEBUG < ERROR)

prop_withSpanNoOp :: H.Property
prop_withSpanNoOp = H.property $ do
  result <- H.evalIO $ withSpan "test.span" [] (pure (42 :: Int))
  result === 42

prop_noSpanContext :: H.Property
prop_noSpanContext = H.withTests 1 $ H.property $ do
  ctx <- H.evalIO getCurrentSpanContext
  ctx === Nothing

prop_startEndNoOp :: H.Property
prop_startEndNoOp = H.withTests 1 $ H.property $ do
  span' <- H.evalIO $ startSpan "test.span" []
  H.evalIO $ endSpan span'
  pure ()

prop_addAttrNoOp :: H.Property
prop_addAttrNoOp = H.withTests 1 $ H.property $ do
  span' <- H.evalIO $ startSpan "test.span" []
  H.evalIO $ addAttribute span' "key" (TextValue "value")
  pure ()

prop_logNoOp :: H.Property
prop_logNoOp = H.withTests 1 $ H.property $ do
  H.evalIO $ log INFO "test message" []
  H.evalIO $ log ERROR "error message" [("code", IntValue 500)]
  pure ()

prop_counterNoOp :: H.Property
prop_counterNoOp = H.withTests 1 $ H.property $ do
  H.evalIO $ incrementCounter "test.counter" 1 []
  H.evalIO $ addCounter "test.counter" 1.5 []
  pure ()

prop_gaugeNoOp :: H.Property
prop_gaugeNoOp = H.withTests 1 $ H.property $ do
  H.evalIO $ recordGauge "test.gauge" 42.0 []
  pure ()

prop_histogramNoOp :: H.Property
prop_histogramNoOp = H.withTests 1 $ H.property $ do
  H.evalIO $ recordHistogram "test.histogram" 0.123 []
  pure ()

prop_initNoOp :: H.Property
prop_initNoOp = H.withTests 1 $ H.property $ do
  handle <- H.evalIO $ initTelemetry NoOpConfig
  H.evalIO $ shutdownTelemetry handle
  pure ()

prop_shutdown :: H.Property
prop_shutdown = H.withTests 1 $ H.property $ do
  handle <- H.evalIO $ initTelemetry NoOpConfig
  H.evalIO $ shutdownTelemetry handle
  ctx <- H.evalIO getCurrentSpanContext
  ctx === Nothing
