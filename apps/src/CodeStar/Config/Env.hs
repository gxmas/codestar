module CodeStar.Config.Env
  ( loadFromEnv
  ) where

import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import CodeStar.Config.Types

loadFromEnv :: IO PartialConfig
loadFromEnv = do
  key <- getApiKey
  sPort <- readEnvInt "CODESTAR_SERVER_PORT"
  telMode <- readEnvTelemetryMode
  telMetricsBindHost <- readEnvText "CODESTAR_TELEMETRY_METRICS_BIND_HOST"
  telMetricsPort <- readEnvInt "CODESTAR_TELEMETRY_METRICS_PORT"
  telEndpoint <- readEnvText "CODESTAR_TELEMETRY_ENDPOINT"
  ctxMax <- readEnvInt "CODESTAR_CONTEXT_MAX_TOKENS"
  shellTo <- readEnvInt "CODESTAR_SHELL_DEFAULT_TIMEOUT"
  budgetSt <- readEnvInt "CODESTAR_BUDGET_MAX_STEPS"

  let partial = PartialConfig
        (Last Nothing)
        (Last Nothing)
        (Last Nothing)
        (Last Nothing)
        (Last Nothing)
        (Last Nothing)
        (Last (ApiKey <$> key))
        (Last Nothing)
        (Last Nothing)
        (Last Nothing)
        (PartialServerSection (Last sPort) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing))
        (PartialTelemetrySection (Last telMode) (Last Nothing) (Last (Just <$> telEndpoint)) (Last Nothing) (Last Nothing) (Last telMetricsBindHost) (Last (Just <$> telMetricsPort)))
        (PartialContextSection (Last ctxMax) (Last Nothing) (Last Nothing) (Last Nothing) (Last Nothing))
        (PartialCompactionSection (Last Nothing) (Last Nothing))
        (PartialShellSection (Last shellTo) (Last Nothing) (Last Nothing))
        (PartialSessionSection (Last Nothing) (Last Nothing))
        (PartialGuardrailsSection (Last Nothing) (Last Nothing) (Last Nothing))
        (PartialBudgetSection (Last budgetSt) (Last Nothing) (Last Nothing))
        (PartialRepoMapSection (Last Nothing) (Last Nothing) (Last Nothing))
        (PartialMemorySection (Last Nothing) (Last Nothing) (Last Nothing))
  pure partial

getApiKey :: IO (Maybe Text)
getApiKey = do
  override <- lookupEnv "CODESTAR_API_KEY"
  case override of
    Just k  -> pure (Just (Text.pack k))
    Nothing -> fmap (fmap Text.pack) (lookupEnv "ANTHROPIC_API_KEY")

readEnvInt :: String -> IO (Maybe Int)
readEnvInt name = do
  mVal <- lookupEnv name
  pure (mVal >>= readMaybe)

readEnvText :: String -> IO (Maybe Text)
readEnvText name = fmap (fmap Text.pack) (lookupEnv name)

readEnvTelemetryMode :: IO (Maybe TelemetryMode)
readEnvTelemetryMode = do
  mVal <- lookupEnv "CODESTAR_TELEMETRY_MODE"
  pure $ mVal >>= \case
    "otlp"   -> Just TelemetryOtlp
    "stderr"  -> Just TelemetryStderr
    "off"     -> Just TelemetryOff
    _         -> Nothing
