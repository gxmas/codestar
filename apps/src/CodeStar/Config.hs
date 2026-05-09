module CodeStar.Config
  ( -- * Config (resolved)
    Config (..)
  , ApiKey (..)
  , ModelSpec (..)
  , ServerSection (..)
  , TelemetrySection (..)
  , ContextSection (..)
  , CompactionSection (..)
  , ShellSection (..)
  , SessionSection (..)
  , GuardrailsSection (..)
  , BudgetSection (..)
  , RepoMapSection (..)
  , MemorySection (..)

    -- * Partial config (for Config submodules only)
  , PartialConfig
  , PartialServerSection
  , PartialTelemetrySection
  , PartialContextSection
  , PartialCompactionSection
  , PartialShellSection
  , PartialSessionSection
  , PartialGuardrailsSection
  , PartialBudgetSection
  , PartialRepoMapSection
  , PartialMemorySection

    -- * Enums
  , TelemetryMode (..)
  , IndexStrategy (..)
  , SandboxMode (..)
  , AuthMode (..)
  , McpEndpoint (..)
  , McpTransport (..)
  , ConfigError (..)

    -- * CLI
  , CliArgs (..)
  , CliCommand (..)
  , CacheGcArgs (..)
  , RunArgs (..)

    -- * Defaults
  , module CodeStar.Config.Defaults

    -- * Loading
  , module CodeStar.Config.Load

    -- * Validation
  , resolve

    -- * Legacy aliases
  , AgentConfig
  , BudgetConfig

    -- * CLI parsing
  , parseCliArgs
  , runArgsParser

    -- * Legacy re-exports for compatibility
  , MemoryConfig
  ) where

import Options.Applicative hiding (command)
import Options.Applicative qualified as OA

import CodeStar.Config.Types
import CodeStar.Config.Defaults
import CodeStar.Config.Load
import CodeStar.Config.Validate (resolve)

-- | Legacy type alias — existing code that imports AgentConfig keeps working.
type AgentConfig = Config

-- | Legacy type alias
type BudgetConfig = BudgetSection

-- | Legacy type alias
type MemoryConfig = MemorySection

-- --------------------------------------------------------------------
-- CLI argument parsing
-- --------------------------------------------------------------------

parseCliArgs :: IO CliArgs
parseCliArgs = execParser opts
 where
  opts =
    info
      (cliArgsParser <**> helper)
      (fullDesc <> progDesc "CodeStar AI coding agent")

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs <$> commandParser

commandParser :: Parser CliCommand
commandParser =
  subparser
    ( OA.command
        "fetch-grammars"
        ( info
            fetchGrammarsParser
            (progDesc "Download and compile tree-sitter language grammars")
        )
    <> OA.command
        "migrate-config"
        ( info
            migrateConfigParser
            (progDesc "Convert settings.json to settings.toml")
        )
    <> OA.command
        "cache-gc"
        ( info
            cacheGcParser
            (progDesc "List or delete stale repo-map cache entries")
        )
    )
    <|> fmap RunAgent runArgsParser

runArgsParser :: Parser RunArgs
runArgsParser =
  RunArgs
    <$> optional
      ( strOption
          (long "model" <> metavar "MODEL" <> help "Model name override")
      )
    <*> optional
      ( option
          auto
          (long "port" <> metavar "PORT" <> help "Server port")
      )
    <*> switch
      (long "headless" <> help "Run in headless mode (no interactive REPL)")
    <*> optional
      ( strOption
          (long "task" <> metavar "TASK" <> help "Task to execute (headless mode)")
      )
    <*> optional
      ( strOption
          (long "workspace" <> short 'w' <> metavar "DIR" <> help "Workspace directory")
      )
    <*> telemetryParser

telemetryParser :: Parser TelemetryMode
telemetryParser =
  flag'
    TelemetryOff
    (long "no-telemetry" <> help "Disable telemetry")
    <|> option
      readTelemetryMode
      ( long "telemetry"
          <> metavar "otlp|stderr"
          <> value TelemetryOtlp
          <> help "Telemetry backend (default: otlp)"
      )

readTelemetryMode :: ReadM TelemetryMode
readTelemetryMode = eitherReader $ \case
  "otlp" -> Right TelemetryOtlp
  "stderr" -> Right TelemetryStderr
  other -> Left ("Unknown telemetry mode: " <> other <> " (expected otlp or stderr)")

fetchGrammarsParser :: Parser CliCommand
fetchGrammarsParser =
  FetchGrammars
    <$> optional
      ( strOption
          (long "lang" <> metavar "LANG" <> help "Fetch only this language")
      )

migrateConfigParser :: Parser CliCommand
migrateConfigParser =
  MigrateConfig
    <$> optional
      ( strOption
          (long "workspace" <> short 'w' <> metavar "DIR" <> help "Workspace directory")
      )

cacheGcParser :: Parser CliCommand
cacheGcParser =
  CacheGc
    <$> ( CacheGcArgs
            <$> switch
              (long "delete" <> help "Delete stale cache entries (default: list only)")
            <*> switch
              (long "json" <> help "Emit machine-readable JSON output")
            <*> optional
              ( strOption
                  (long "workspace" <> short 'w' <> metavar "DIR" <> help "Workspace directory")
              )
        <* switch
          (long "list" <> help "List stale cache entries (default behavior)")
        )
