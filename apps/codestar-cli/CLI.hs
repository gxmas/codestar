module Main where

import Control.Exception (finally)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import CLI.Repl (ReplEnv (..), runInteractive)
import CLI.Setup (CliResources (..), buildCliResources, loadConfigOrDie)
import CodeStar.AgentLoop (AgentEnv (..), runAgent)
import CodeStar.Config
  ( CacheGcArgs (..)
  , CliArgs (..)
  , CliCommand (..)
  , RunArgs (..)
  , parseCliArgs
  )
import CodeStar.Config.Migrate (migrateJsonToToml)
import CodeStar.Config.Paths qualified as Paths
import CodeStar.RepoMap.CacheGc (CacheGcReport (..), StaleEntry (..), StaleReason (..), runCacheGc)
import CodeStar.Storage (newBackend)
import CodeStar.TreeSitter.Grammars (GrammarSpec (..), fetchAllGrammars, fetchGrammar, grammarsDir, knownGrammars)
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

-- --------------------------------------------------------------------
-- fetch-grammars command
-- --------------------------------------------------------------------

runFetchGrammars :: Maybe Text -> IO ()
runFetchGrammars mLang = do
  dir <- grammarsDir
  Text.IO.putStrLn ("Fetching grammars to " <> Text.pack dir <> " ...")
  results <- case mLang of
    Nothing -> fetchAllGrammars dir report
    Just l -> case filter (\g -> g.language == l) knownGrammars of
      [] -> Text.IO.hPutStr stderr ("Unknown: " <> l <> "\n") >> exitFailure
      (g : _) -> report l >> fmap (\r -> [(g, r)]) (fetchGrammar dir g)
  let failures = [(g, e) | (g, Left e) <- results]
      successes = length [() | (_, Right _) <- results]
  Text.IO.putStrLn ("\nDone. " <> Text.pack (show successes) <> " grammars installed.")
  if null failures
    then exitSuccess
    else do
      Text.IO.hPutStr stderr "Failed:\n"
      mapM_ (\(g, e) -> Text.IO.hPutStr stderr ("  " <> g.language <> ": " <> e <> "\n")) failures
      exitFailure

report :: Text -> IO ()
report lang = Text.IO.putStrLn ("  [→] " <> lang)

-- --------------------------------------------------------------------
-- cache-gc command
-- --------------------------------------------------------------------

runCacheGcCommand :: CacheGcArgs -> IO ()
runCacheGcCommand args = do
  let workspace = maybe "." id args.cgWorkspace
      cacheRoot = Paths.projectDir workspace </> "cache"
  backend <- newBackend cacheRoot
  gcReport <- runCacheGc backend args.cgWorkspace args.cgDelete
  if args.cgJson
    then BL8.putStrLn (encode gcReport)
    else printCacheGcReport args.cgDelete gcReport

printCacheGcReport :: Bool -> CacheGcReport -> IO ()
printCacheGcReport deleted gcReport = do
  Text.IO.putStrLn ("Scanned entries: " <> tshow gcReport.scannedEntries)
  Text.IO.putStrLn ("Stale entries: " <> tshow gcReport.staleEntries)
  Text.IO.putStrLn ("Deleted entries: " <> tshow gcReport.deletedEntries)
  mapM_ printReason
    [StaleGlobal, StaleFile, StaleBoth]
  if null gcReport.entries
    then Text.IO.putStrLn "No stale entries found."
    else do
      Text.IO.putStrLn ""
      Text.IO.putStrLn
        (if deleted then "Processed stale entries:" else "Stale entries:")
      mapM_ printEntry gcReport.entries
 where
  printReason reason =
    Text.IO.putStrLn
      (reasonLabel reason <> ": " <> tshow (Map.findWithDefault 0 (reasonLabelInline reason) gcReport.staleByReason))

  reasonLabel StaleGlobal = "  stale-global"
  reasonLabel StaleFile = "  stale-file"
  reasonLabel StaleBoth = "  stale-both"

  printEntry entry =
    Text.IO.putStrLn
      ( "  - "
          <> maybe "<unknown-path>" Text.pack entry.stalePath
          <> " ["
          <> reasonLabelInline entry.staleReason
          <> "]"
      )

  reasonLabelInline StaleGlobal = "stale-global"
  reasonLabelInline StaleFile = "stale-file"
  reasonLabelInline StaleBoth = "stale-both"

tshow :: Show a => a -> Text
tshow = Text.pack . show
