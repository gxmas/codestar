{- |
= codestar-cli — entry point

The CLI application is the __local__ face of the coding-agent system.
It reads a TOML config file, wires up all subsystems (LLM client,
tool registry, repo-map worker, telemetry …), and then runs the agent
in one of two modes:

  * __Interactive (REPL)__: the user types tasks and slash commands at a
    @codestar>@ prompt.  Good for exploratory, conversational sessions.
  * __Headless__: a single task is supplied via @--task@ and the agent
    exits with a UNIX exit code when it finishes.  Good for CI pipelines
    and scripting.

Utility sub-commands (@fetch-grammars@, @cache-gc@, @migrate-config@) are
dispatched first; they do not start the agent loop.
-}
module Main where

import Control.Exception (finally)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import CLI.CacheGc (runCacheGcCommand)
import CLI.FetchGrammars (runFetchGrammars)
import CLI.Repl (ReplEnv (..), runInteractive)
import CLI.Setup (CliResources (..), buildCliResources, loadConfigOrDie)
import CodeStar.AgentLoop (AgentEnv (..), runAgent)
import CodeStar.Config
  ( CliArgs (..)
  , CliCommand (..)
  , RunArgs (..)
  , parseCliArgs
  )
import CodeStar.Config.Migrate (migrateJsonToToml)
import CodeStar.Types (ControlSignal (..))

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

-- | Parse CLI arguments and dispatch to the appropriate sub-command.
-- Line-buffering is forced on stdout so that streaming LLM tokens
-- appear immediately rather than being held in the OS buffer.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- parseCliArgs
  case args.cliCommand of
    FetchGrammars mLang -> runFetchGrammars mLang
    MigrateConfig mWorkspace -> migrateJsonToToml mWorkspace
    CacheGc gcArgs -> runCacheGcCommand gcArgs
    RunAgent runArgs -> runAgentCli runArgs

-- --------------------------------------------------------------------
-- run-agent command
-- --------------------------------------------------------------------

-- | Bootstrap all agent resources and choose interactive vs. headless mode.
--
-- 'buildCliResources' does the heavy lifting (LLM client, tool registry,
-- repo-map worker …).  This function simply decides which front-end to
-- attach: a full REPL or a single-shot headless run.
-- 'finally' guarantees telemetry and background workers are shut down
-- cleanly even if an exception propagates.
runAgentCli :: RunArgs -> IO ()
runAgentCli runArgs = do
  config <- loadConfigOrDie runArgs
  resources <- buildCliResources config runArgs
  let renv = ReplEnv
        { reEnv       = resources.crEnv
        , reSysPrompt = resources.crSysPrompt
        , reCostRef   = resources.crCostRef
        , reWorker    = resources.crWorker
        }
  ( if runArgs.cliHeadless
      then runHeadless resources.crEnv resources.crSysPrompt runArgs
      else runInteractive renv
    )
    `finally` resources.crShutdown

-- --------------------------------------------------------------------
-- Headless mode
-- --------------------------------------------------------------------

-- | Run the agent non-interactively with a single task string.
--
-- The exit code communicates the agent's terminal 'ControlSignal' to the
-- calling process: 0 for 'Done', non-zero for 'Blocked' or unexpected
-- signals.  This makes headless mode composable with shell scripts and
-- CI jobs that check @$?@.
runHeadless :: AgentEnv -> Text -> RunArgs -> IO ()
runHeadless env sysPrompt runArgs = do
  let task = case runArgs.cliTask of
        Just t -> t
        Nothing -> error "Headless mode requires --task"
  signal <- runAgent env sysPrompt task
  case signal of
    Done _ -> exitSuccess
    Blocked msg -> Text.IO.hPutStr stderr ("[blocked] " <> msg <> "\n") >> exitFailure
    _ -> exitFailure
