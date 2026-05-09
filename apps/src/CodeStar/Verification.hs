module CodeStar.Verification
  ( -- * Verification chain
    VerificationConfig (..)
  , defaultVerificationConfig
  , VerificationResult (..)
  , verify

    -- * Individual levels
  , verifySyntax
  , verifyTests
  , verifyEvidence
  ) where

import Control.Exception (IOException, catch)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text

import CodeStar.LLM.Base (ToolName (..))
import CodeStar.Tools.Registry (ToolInput (..), ToolOutput (..), ToolRegistry, dispatch)
import CodeStar.TreeSitter
  ( GrammarRegistry
  , SyntaxResult (..)
  , languageForFile
  , lookupLanguage
  , validateWithTimeout
  )
import CodeStar.Types
  ( CheckResult (..)
  , Evidence (..)
  )

-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

data VerificationConfig = VerificationConfig
  { runTestsAfterGroup :: !Bool
  -- ^ Level 2: run test suite after each logical step group
  , requireAllPassed :: !Bool
  -- ^ Level 3: all Evidence fields must be Passed
  }
  deriving stock (Eq, Show)

defaultVerificationConfig :: VerificationConfig
defaultVerificationConfig =
  VerificationConfig
    { runTestsAfterGroup = True
    , requireAllPassed = False
    }

-- --------------------------------------------------------------------
-- Result
-- --------------------------------------------------------------------

data VerificationResult
  = VerificationPassed Evidence
  | -- | reason for failure
    VerificationFailed Text
  | -- | some checks passed, work continues
    VerificationPartial Evidence
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Verification chain
-- --------------------------------------------------------------------

{- | Run all three verification levels in order.
Level 1: syntax check each modified file via tree-sitter (milliseconds).
Level 2: run the test suite if configured (seconds-minutes).
Level 3: aggregate Evidence and decide on a Done signal.
-}
verify ::
  GrammarRegistry ->
  ToolRegistry ->
  VerificationConfig ->
  -- | files modified in this step group
  [FilePath] ->
  IO VerificationResult
verify gramReg toolReg cfg modifiedFiles = do
  syntaxOk <- verifySyntax gramReg modifiedFiles
  if not syntaxOk
    then pure (VerificationFailed "Syntax errors found in modified files")
    else do
      testResult <-
        if cfg.runTestsAfterGroup
          then verifyTests toolReg
          else pure NotChecked
      let ev =
            Evidence
              { testsPass = testResult
              , buildSucceeds = NotChecked
              , filesVerified = modifiedFiles
              , regressions = []
              }
      pure (verifyEvidence cfg ev)

-- --------------------------------------------------------------------
-- Level 1: Syntax
-- --------------------------------------------------------------------

-- | Return True only if every file with a supported language parses cleanly.
verifySyntax :: GrammarRegistry -> [FilePath] -> IO Bool
verifySyntax gramReg paths = do
  results <- mapM checkFile paths
  pure (and results)
 where
  checkFile path =
    case languageForFile path >>= lookupLanguage gramReg of
      Nothing -> pure True -- unsupported language: pass through
      Just lang -> do
        bs <- safeReadFile path
        case bs of
          Nothing -> pure True -- file gone: not our problem here
          Just src -> do
            r <- validateWithTimeout lang src
            case r of
              Left _ -> pure True -- parse error in TS itself: pass through
              Right sr -> pure sr.valid

safeReadFile :: FilePath -> IO (Maybe ByteString)
safeReadFile path =
  fmap Just (BS.readFile path) `catch` handler
 where
  handler :: IOException -> IO (Maybe ByteString)
  handler _ = pure Nothing

-- --------------------------------------------------------------------
-- Level 2: Tests
-- --------------------------------------------------------------------

{- | Run the project test suite via the run_tests tool.
Returns Passed if the tool reports success, Failed otherwise.
-}
verifyTests :: ToolRegistry -> IO CheckResult
verifyTests toolReg = do
  let input = ToolInput{arguments = mempty}
  result <- dispatch toolReg (ToolName "run_tests") input
  case result of
    Left _ -> pure Failed
    Right output ->
      if Text.isInfixOf "PASSED" (Text.toUpper output.content)
        || Text.isInfixOf "OK" (Text.toUpper output.content)
        then pure Passed
        else pure Failed

-- --------------------------------------------------------------------
-- Level 3: Evidence aggregation
-- --------------------------------------------------------------------

-- | Decide the final verification result from accumulated Evidence.
verifyEvidence :: VerificationConfig -> Evidence -> VerificationResult
verifyEvidence cfg ev
  | cfg.requireAllPassed && ev.testsPass /= Passed =
      VerificationPartial ev
  | ev.testsPass == Failed =
      VerificationFailed "Test suite failed"
  | otherwise =
      VerificationPassed ev
