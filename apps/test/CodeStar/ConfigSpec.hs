{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.ConfigSpec (spec) where

import Options.Applicative
  ( ParserResult
  , defaultPrefs
  , execParserPure
  , info
  )
import Options.Applicative qualified as OA
import Test.Hspec
import Test.QuickCheck

import CodeStar.Config (RunArgs (..), TelemetryMode (..), runArgsParser)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

-- | Run the RunArgs parser against a list of CLI tokens.
parseRunArgs :: [String] -> ParserResult RunArgs
parseRunArgs = execParserPure defaultPrefs (info runArgsParser mempty)

-- | Extract a successful parse, or Nothing.
successResult :: ParserResult a -> Maybe a
successResult (OA.Success a) = Just a
successResult _ = Nothing

-- | True when the parse failed.
isParseFailure :: ParserResult a -> Bool
isParseFailure (OA.Failure _) = True
isParseFailure _ = False

-- --------------------------------------------------------------------
-- Arbitrary instances
-- --------------------------------------------------------------------

instance Arbitrary TelemetryMode where
  arbitrary = elements [TelemetryOtlp, TelemetryStderr, TelemetryOff]

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Config telemetry flags" $ do
  describe "telemetry mode parsing" $ do
    it "--telemetry otlp parses to TelemetryOtlp" $
      fmap cliTelemetry (successResult (parseRunArgs ["--telemetry", "otlp"]))
        `shouldBe` Just TelemetryOtlp

    it "--telemetry stderr parses to TelemetryStderr" $
      fmap cliTelemetry (successResult (parseRunArgs ["--telemetry", "stderr"]))
        `shouldBe` Just TelemetryStderr

    it "--no-telemetry parses to TelemetryOff" $
      fmap cliTelemetry (successResult (parseRunArgs ["--no-telemetry"]))
        `shouldBe` Just TelemetryOff

    it "defaults to TelemetryOtlp when no flag is given" $
      fmap cliTelemetry (successResult (parseRunArgs []))
        `shouldBe` Just TelemetryOtlp

    it "rejects unknown telemetry modes" $
      parseRunArgs ["--telemetry", "datadog"] `shouldSatisfy` isParseFailure

  describe "telemetry flag properties" $ do
    it "every valid mode string round-trips through the parser" $
      property $ \mode ->
        let flag = modeToFlag mode
            result = fmap cliTelemetry (successResult (parseRunArgs flag))
         in counterexample (show flag) $
              result === Just mode

    it "telemetry flag does not interfere with other flags" $
      property $ \mode ->
        let flag = modeToFlag mode
            args = ["--headless", "--task", "test"] ++ flag
            result = successResult (parseRunArgs args)
         in counterexample (show args) $ case result of
              Nothing -> property False
              Just ra ->
                conjoin
                  [ ra.cliHeadless === True
                  , ra.cliTask === Just "test"
                  , ra.cliTelemetry === mode
                  ]

    it "arbitrary non-telemetry strings are rejected by --telemetry" $
      property $
        forAll arbitraryBadMode $ \bad ->
          counterexample bad $
            parseRunArgs ["--telemetry", bad] `shouldSatisfy` isParseFailure

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

-- | Convert a TelemetryMode to the CLI flags that produce it.
modeToFlag :: TelemetryMode -> [String]
modeToFlag TelemetryOtlp = ["--telemetry", "otlp"]
modeToFlag TelemetryStderr = ["--telemetry", "stderr"]
modeToFlag TelemetryOff = ["--no-telemetry"]

-- | Generate strings that are not valid telemetry mode names.
arbitraryBadMode :: Gen String
arbitraryBadMode = arbitrary `suchThat` (\s -> s `notElem` ["otlp", "stderr"])
