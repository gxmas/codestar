module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Resilience.Core

main :: IO ()
main = defaultMain $ testGroup "resilience"
  [ testProperty "computeDelay Constant returns same delay" prop_constantDelay
  , testProperty "computeDelay Linear scales with attempt" prop_linearDelay
  , testProperty "withRecovery succeeds on first try" prop_successOnFirstTry
  , testProperty "getCircuitState returns Nothing before any calls" prop_circuitInitial
  ]

prop_constantDelay :: H.Property
prop_constantDelay = H.property $ do
  d <- H.forAll (fromIntegral <$> Gen.int (Range.linear 1 1000))
  attempt <- H.forAll (Gen.int (Range.linear 1 10))
  result <- H.evalIO (computeDelay (Constant d) attempt)
  result === d

prop_linearDelay :: H.Property
prop_linearDelay = H.property $ do
  base <- H.forAll (fromIntegral <$> Gen.int (Range.linear 1 100))
  attempt <- H.forAll (Gen.int (Range.linear 1 5))
  result <- H.evalIO (computeDelay (Linear base) attempt)
  result === base * fromIntegral attempt

prop_successOnFirstTry :: H.Property
prop_successOnFirstTry = H.property $ do
  engine <- H.evalIO (newRecoveryEngine defaultRecoveryPolicy)
  result <- H.evalIO (withRecovery engine "test" (pure (42 :: Int)))
  case result of
    RecoverySuccess v -> v === 42
    _                 -> H.failure

prop_circuitInitial :: H.Property
prop_circuitInitial = H.property $ do
  engine <- H.evalIO (newRecoveryEngine defaultRecoveryPolicy)
  state <- H.evalIO (getCircuitState engine "unregistered-op")
  state === Nothing
