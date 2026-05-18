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
