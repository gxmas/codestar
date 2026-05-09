{-# LANGUAGE OverloadedStrings #-}

module CodeStar.TelemetrySpec (spec) where

import Control.Concurrent.Async (mapConcurrently_)
import Data.Text qualified as Text
import Data.Aeson (Value (Object), encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.List (isInfixOf)
import Test.Hspec

import CodeStar.Telemetry
  ( AgentEvent (..)
  , TelemetryRecorder (..)
  , jsonRecorder
  , noOpRecorder
  )
import CodeStar.Types (ControlSignal (..), ModelRole (..), TaskType (..))

spec :: Spec
spec = describe "CodeStar.Telemetry" $ do
  describe "AgentEvent JSON encoding" $ do
    it "yields an object payload" $ do
      let ev = EvToolEnd{toolName = "shell", success = True, durationMs = 12}
      (Aeson.decode (encode ev) :: Maybe Value) `shouldSatisfy` isObject

    it "EvToolEnd includes key fields" $ do
      let raw = BLC8.unpack (encode EvToolEnd{toolName = "shell", success = True, durationMs = 12})
      raw `shouldSatisfy` isInfixOf "EvToolEnd"
      raw `shouldSatisfy` isInfixOf "toolName"
      raw `shouldSatisfy` isInfixOf "shell"
      raw `shouldSatisfy` isInfixOf "success"
      raw `shouldSatisfy` isInfixOf "durationMs"

    it "EvCostUpdate includes token and cost fields" $ do
      let raw = BLC8.unpack (encode (EvCostUpdate 10 20 0.03))
      raw `shouldSatisfy` isInfixOf "EvCostUpdate"
      raw `shouldSatisfy` isInfixOf "totalInputTokens"
      raw `shouldSatisfy` isInfixOf "totalOutputTokens"
      raw `shouldSatisfy` isInfixOf "estimatedCostUsd"

    it "EvCompaction includes before/after lengths" $ do
      let raw = BLC8.unpack (encode (EvCompaction 40 8))
      raw `shouldSatisfy` isInfixOf "EvCompaction"
      raw `shouldSatisfy` isInfixOf "historyLenBefore"
      raw `shouldSatisfy` isInfixOf "historyLenAfter"

  describe "noOpRecorder" $ do
    it "all operations complete without error" $ do
      let r = noOpRecorder
      sp <- r.startSpan "test-span" [("k", "v")]
      r.recordEvent (EvControlSignal Continue)
      r.incrementCounter "count" [("kind", "noop")]
      r.endSpan sp

    it "handles nested spans correctly" $ do
      let r = noOpRecorder
      sp1 <- r.startSpan "outer" []
      sp2 <- r.startSpan "inner" []
      r.endSpan sp2
      r.endSpan sp1

    it "is safe under concurrent recordEvent calls" $ do
      let r = noOpRecorder
          events = replicate 200 (EvControlSignal Continue)
              ++ replicate 200 (EvCostUpdate 1 1 0.001)
      mapConcurrently_ r.recordEvent events

    it "is safe under concurrent span operations" $ do
      let r = noOpRecorder
      mapConcurrently_ (\(i :: Int) -> do
          sp <- r.startSpan ("span-" <> Text.pack (show i)) []
          r.incrementCounter "counter" []
          r.endSpan sp
        ) [1 .. 100]

  describe "jsonRecorder" $ do
    it "handles all event types without error" $ do
      let r = jsonRecorder
      sp <- r.startSpan "json-span" []
      r.recordEvent (EvPlanGenerated{stepCount = 3, taskType = Feature})
      r.recordEvent (EvLlmCall Architect 100 50 200)
      r.recordEvent (EvCostUpdate 10 20 0.03)
      r.recordEvent (EvToolEnd{toolName = "shell", success = False, durationMs = 5})
      r.recordEvent (EvCompaction 40 8)
      r.incrementCounter "json.counter" []
      r.endSpan sp

    it "is safe under concurrent recordEvent calls" $ do
      let r = jsonRecorder
          events = replicate 50 (EvControlSignal (Blocked "test"))
              ++ replicate 50 (EvCostUpdate 1 1 0.001)
      mapConcurrently_ r.recordEvent events

isObject :: Maybe Value -> Bool
isObject (Just (Object _)) = True
isObject _ = False
