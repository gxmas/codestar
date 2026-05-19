{- |
= CodeStar.Config — configuration facade

This module is the __single import__ that gives the rest of the codebase
access to all configuration types and loading logic.  It re-exports from
the sub-modules without exposing the internal split:

@
  Config.Types    — concrete record types (Config, ModelEntry, etc.)
  Config.Defaults — default values for each section
  Config.Load     — TOML file loading and environment-variable overlay
  Config.Validate — validation and resolution of partial config
@

== Config hierarchy

The configuration is loaded in layers and merged:

  1. __Compiled defaults__ ('Config.Defaults')
  2. __Global TOML__ (@~\/.codestar\/config.toml@)
  3. __Project TOML__ (@.codestar\/config.toml@ in the workspace)
  4. __Environment variables__ (e.g. @ANTHROPIC_API_KEY@)
  5. __CLI flags__ (@--model@, @--workspace@, …)

Later layers override earlier ones.  The result is a fully-resolved 'Config'.

== Key sections

  * 'ModelEntry' — one LLM model with provider, key, and parameter overrides.
  * 'ServerSection' — port, auth mode, host binding.
  * 'TelemetrySection' — backend selection and OTel endpoint.
  * 'BudgetSection' — per-session and daily token limits.
  * 'McpEndpoint' — external MCP server connections.

== Partial vs. resolved config

'PartialConfig' (and its section variants) use 'Maybe' for every field,
allowing safe merging of config layers.  'resolve' validates a merged
partial config and produces a concrete 'Config' or a list of 'ConfigError'.
-}
module CodeStar.Config
  ( -- * Config (resolved)
    Config (..)
  , ApiKey (..)
  , ModelEntry (..)
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
  , McpAuthConfig (..)
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
