module Main where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Word (Word32)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getModificationTime
  , listDirectory
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import CodeStar.RepoMap.Cache (RepoMapCache (..), newRepoMapCache)
import CodeStar.RepoMap.CacheGc (CacheGcReport (..), runCacheGc)
import CodeStar.RepoMap.Graph
  ( SymbolGraph (..)
  , Tag (..)
  , TagKind (..)
  , buildSymbolGraph
  , defaultWeights
  , extractTags
  , querySourceModeLabel
  , pageRank
  )
import CodeStar.RepoMap.Render (defaultRenderConfig, renderRepoMap)
import CodeStar.RepoMap.Render qualified as RepoRender
import CodeStar.Storage (StorageBackend, newBackend)
import CodeStar.TreeSitter
  ( GrammarRegistry
  , grammarCount
  , languageForFile
  , loadGrammarRegistry
  , loadedLanguages
  , lookupLanguage
  )
import CodeStar.TreeSitter.Grammars (grammarsDir, knownGrammars)
import TreeSitter qualified as TS
import TreeSitter.Language (languageVersion)
import TreeSitter.Node (Node (..))

-- --------------------------------------------------------------------
-- CLI args
-- --------------------------------------------------------------------

data Mode
  = IndexDir FilePath
  | InspectFile FilePath
  | DumpAst FilePath
  | CheckGrammar Text
  | CacheStats FilePath
  | PageRankMode FilePath
  | RenderMode FilePath

data Opts = Opts
  { mode :: Mode
  , verbose :: Bool
  , langFilter :: Maybe Text
  , listGrammars :: Bool
  , focusFiles :: [FilePath]
  , tokenBudget :: Int
  }

usage :: String
usage =
  unlines
    [ "Usage: repo-map [OPTIONS] [DIR]"
    , ""
    , "Options:"
    , "  --list-grammars        List each loaded grammar by name"
    , "  -v, --verbose          Show per-file tag counts / all PageRank scores"
    , "  --file FILE            Extract and display tags for a single file"
    , "  --dump-ast FILE        Print the full AST node tree for a file"
    , "  --check-grammar LANG   Test grammar loading and ABI compatibility"
    , "  --cache-stats [DIR]    Show cache hit/miss stats for a directory"
    , "  --pagerank [DIR]       Show per-file PageRank scores and graph stats"
    , "  --render [DIR]         Show the rendered repo map text (as sent to the LLM)"
    , "  --focus FILE           Personalise PageRank as if FILE is open (repeatable)"
    , "  --max-tokens N         Token budget for --render (default: 4096)"
    , "  --lang LANG            Filter to files of this language only (e.g. haskell)"
    , "  DIR                    Workspace root to index (default: current directory)"
    ]

parseArgs :: [String] -> Either String Opts
parseArgs args = go args (Opts (IndexDir ".") False Nothing False [] 4096)
 where
  go [] opts = Right opts
  go ("--list-grammars" : rest) opts = go rest opts{listGrammars = True}
  go ("-v" : rest) opts = go rest opts{verbose = True}
  go ("--verbose" : rest) opts = go rest opts{verbose = True}
  go ("--file" : f : rest) opts = go rest opts{mode = InspectFile f}
  go ("--file" : []) _ = Left "--file requires a FILE argument"
  go ("--dump-ast" : f : rest) opts = go rest opts{mode = DumpAst f}
  go ("--dump-ast" : []) _ = Left "--dump-ast requires a FILE argument"
  go ("--check-grammar" : l : rest) opts = go rest opts{mode = CheckGrammar (Text.pack l)}
  go ("--check-grammar" : []) _ = Left "--check-grammar requires a LANG argument"
  go ("--cache-stats" : rest) opts =
    case rest of
      (d : rest') | not ("-" `isPrefixOf` d) -> go rest' opts{mode = CacheStats d}
      _ -> go rest opts{mode = CacheStats "."}
  go ("--pagerank" : rest) opts =
    case rest of
      (d : rest') | not ("-" `isPrefixOf` d) -> go rest' opts{mode = PageRankMode d}
      _ -> go rest opts{mode = PageRankMode "."}
  go ("--render" : rest) opts =
    case rest of
      (d : rest') | not ("-" `isPrefixOf` d) -> go rest' opts{mode = RenderMode d}
      _ -> go rest opts{mode = RenderMode "."}
  go ("--focus" : f : rest) opts = go rest opts{focusFiles = opts.focusFiles <> [f]}
  go ("--focus" : []) _ = Left "--focus requires a FILE argument"
  go ("--max-tokens" : n : rest) opts =
    case reads n of
      [(v, "")] -> go rest opts{tokenBudget = v}
      _ -> Left $ "--max-tokens requires an integer, got: " <> n
  go ("--max-tokens" : []) _ = Left "--max-tokens requires an integer argument"
  go ("--lang" : l : rest) opts = go rest opts{langFilter = Just (Text.pack l)}
  go ("--lang" : []) _ = Left "--lang requires a LANG argument"
  go ("--help" : _) _ = Left usage
  go (('-' : _) : _) _ = Left usage
  go (dir : rest) opts = go rest opts{mode = IndexDir dir}

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      putStrLn msg
      exitFailure
    Right opts -> do
      dir <- grammarsDir
      reg <- loadGrammarRegistry
      printGrammarDiagnostics dir (grammarCount reg)
      queryMode <- querySourceModeLabel
      putStrLn $ "Query mode: " <> Text.unpack queryMode
      printStaleFingerprintSafetyRail opts
      when opts.listGrammars $
        mapM_ (\l -> putStrLn $ "  " <> Text.unpack l) (loadedLanguages reg)
      putStrLn ""
      case opts.mode of
        IndexDir root -> runIndex reg opts root
        InspectFile f -> runInspect reg f
        DumpAst f -> runDumpAst reg f
        CheckGrammar lang -> runCheckGrammar reg lang
        CacheStats root -> runCacheStats root
        PageRankMode root -> runPageRank reg opts root
        RenderMode root -> runRender reg opts root

-- --------------------------------------------------------------------
-- Index mode
-- --------------------------------------------------------------------

runIndex :: GrammarRegistry -> Opts -> FilePath -> IO ()
runIndex reg opts root = do
  files <- listFiles root
  let filtered = case opts.langFilter of
        Nothing -> files
        Just lang -> filter (\f -> languageForFile f == Just lang) files
  putStrLn $
    "Files to process: "
      <> show (length filtered)
      <> (if opts.langFilter /= Nothing then " (filtered by lang)" else "")
  putStrLn ""

  results <- mapM (processFile reg) filtered

  let tagMap = Map.fromList (zip filtered results)
      totalTags = sum (map length results)
      filesWith0 = length (filter null results)
      filesWithTags = length (filter (not . null) results)

  if opts.verbose
    then do
      putStrLn "Per-file tag counts:"
      mapM_ (printFileLine tagMap) (sortOn fst (zip filtered results))
      putStrLn ""
    else do
      putStrLn $ "Files with tags:  " <> show filesWithTags
      putStrLn $ "Files with 0 tags:" <> show filesWith0
      putStrLn ""

  putStrLn $ "Total tags: " <> show totalTags
  putStrLn ""

  let langBreakdown = buildLangBreakdown filtered results
  putStrLn "By language:"
  mapM_ printLangLine (Map.toAscList langBreakdown)

printFileLine :: Map FilePath [Tag] -> (FilePath, [Tag]) -> IO ()
printFileLine _ (path, tags) =
  putStrLn $ "  " <> show (length tags) <> "\t" <> path

printLangLine :: (Text, (Int, Int)) -> IO ()
printLangLine (lang, (files, tags)) =
  putStrLn $
    "  "
      <> Text.unpack lang
      <> ": "
      <> show files
      <> " files, "
      <> show tags
      <> " tags"

buildLangBreakdown :: [FilePath] -> [[Tag]] -> Map Text (Int, Int)
buildLangBreakdown files results =
  foldl add Map.empty (zip files results)
 where
  add m (path, tags) =
    let lang = fromMaybe "unknown" (languageForFile path)
        (fc, tc) = fromMaybe (0, 0) (Map.lookup lang m)
     in Map.insert lang (fc + 1, tc + length tags) m

processFile :: GrammarRegistry -> FilePath -> IO [Tag]
processFile reg path = do
  result <- try @SomeException $ do
    src <- BS.readFile path
    extractTags reg path src
  case result of
    Left err -> do
      hPutStrLn stderr $ "  ERROR " <> path <> ": " <> show err
      pure []
    Right tags -> pure tags

-- --------------------------------------------------------------------
-- Check grammar mode
-- --------------------------------------------------------------------

-- Replicated from the vendored tree-sitter api.h for diagnostic output.
runtimeMaxAbi :: Word32
runtimeMaxAbi = 15

runtimeMinAbi :: Word32
runtimeMinAbi = 13

runCheckGrammar :: GrammarRegistry -> Text -> IO ()
runCheckGrammar reg lang = do
  putStrLn $ "Checking grammar: " <> Text.unpack lang
  putStrLn ""
  case lookupLanguage reg lang of
    Nothing -> do
      putStrLn "  FAIL  Grammar not loaded (file not found in grammars dir, or dlopen failed)"
      exitFailure
    Just tsLang -> do
      ver <- languageVersion tsLang
      putStrLn $ "  Grammar ABI version : " <> show ver
      putStrLn $ "  Runtime accepts     : " <> show runtimeMinAbi <> " - " <> show runtimeMaxAbi
      let abiOk = ver >= runtimeMinAbi && ver <= runtimeMaxAbi
      if not abiOk
        then do
          putStrLn "  ABI check           : FAIL (version out of range)"
          putStrLn ""
          putStrLn "  Recompile with: cabal run codestar-cli -- fetch-grammars --lang=<LANG>"
          exitFailure
        else putStrLn "  ABI check           : OK"

      parser <- TS.newParser
      ok <- TS.setLanguage parser tsLang
      if not ok
        then do
          putStrLn "  setLanguage         : FAIL"
          exitFailure
        else putStrLn "  setLanguage         : OK"

      let probe = "x"
      mTree <- TS.parseSource parser (BS.pack (map (fromIntegral . fromEnum) probe))
      case mTree of
        Nothing -> do
          putStrLn "  parse probe         : FAIL (parser returned Nothing)"
          exitFailure
        Just treeFp -> do
          root <- TS.rootNode treeFp
          hasErr <- TS.nodeHasError root
          putStrLn $
            "  parse probe         : OK"
              <> if hasErr then " (tree has errors - expected for snippet)" else ""
          putStrLn ""
          putStrLn "  Grammar is fully functional."

-- --------------------------------------------------------------------
-- Cache stats mode
-- --------------------------------------------------------------------

runCacheStats :: FilePath -> IO ()
runCacheStats root = do
  let cacheDir = root </> ".codestar" </> "cache"
  cacheExists <- doesDirectoryExist cacheDir
  putStrLn $ "Cache dir: " <> cacheDir
  if not cacheExists
    then putStrLn "  (does not exist - no cache yet)"
    else do
      backend <- newBackend cacheDir
      let cache = newRepoMapCache backend

      files <- listFiles root
      let codeFiles = filter (\f -> languageForFile f /= Nothing) files

      putStrLn $ "Code files in workspace: " <> show (length codeFiles)
      putStrLn ""

      stats <- mapM (checkFileCache cache) codeFiles

      let hits = length [() | (CacheHit, _) <- stats]
          misses = length [() | (CacheMiss, _) <- stats]
          cachedTags = sum [n | (CacheHit, n) <- stats]

      putStrLn $ "Cache hits  (current mtime): " <> show hits
      putStrLn $ "Cache miss  (never cached)  : " <> show misses
      putStrLn ""
      putStrLn $ "Tags in cache               : " <> show cachedTags

data CacheResult = CacheHit | CacheMiss deriving (Eq)

checkFileCache :: RepoMapCache -> FilePath -> IO (CacheResult, Int)
checkFileCache cache path = do
  mMtime <- safeMtime path
  case mMtime of
    Nothing -> pure (CacheMiss, 0)
    Just mtime -> do
      current <- cache.getTags path mtime
      case current of
        Just tags -> pure (CacheHit, length tags)
        Nothing -> pure (CacheMiss, 0)

safeMtime :: FilePath -> IO (Maybe UTCTime)
safeMtime path = do
  result <- try @SomeException (getModificationTime path)
  pure $ case result of
    Left _ -> Nothing
    Right t -> Just t

-- --------------------------------------------------------------------
-- PageRank mode
-- --------------------------------------------------------------------

runPageRank :: GrammarRegistry -> Opts -> FilePath -> IO ()
runPageRank reg opts root = do
  (_files, allTags) <- extractAll reg opts root

  let graph = buildSymbolGraph allTags
      scores = pageRank graph opts.focusFiles [] defaultWeights
      fileCount = Set.size graph.sgFiles
      edgeCount = sum (map Set.size (Map.elems graph.sgEdges))
      isolated = Set.size (Set.filter (`Map.notMember` graph.sgEdges) graph.sgFiles)

  putStrLn "Symbol graph:"
  putStrLn $ "  Files in graph : " <> show fileCount
  putStrLn $ "  Edge count     : " <> show edgeCount
  putStrLn $ "  Isolated files : " <> show isolated <> " (no outgoing reference edges)"
  putStrLn $ "  Unique symbols : " <> show (Map.size graph.sgSymbols)
  putStrLn ""

  if null opts.focusFiles
    then putStrLn "Personalisation: none (uniform - pass --focus FILE to personalise)"
    else do
      putStrLn "Personalisation:"
      mapM_ (\f -> putStrLn $ "  focus: " <> f) opts.focusFiles
  putStrLn ""

  let ranked = sortOn (negate . snd) (Map.toList scores)
      topN = if opts.verbose then ranked else take 20 ranked
      label = if opts.verbose then "All files" else "Top 20 files"

  putStrLn $ label <> " by PageRank score:"
  mapM_ printScore topN
  when (not opts.verbose && length ranked > 20) $
    putStrLn $ "  ... " <> show (length ranked - 20) <> " more (use -v to see all)"
 where
  printScore (path, score) =
    putStrLn $ "  " <> padScore score <> "  " <> path

  padScore s =
    let str = show (fromIntegral (round (s * 1e6) :: Int) / 1e6 :: Double)
     in replicate (10 - length str) ' ' <> str

-- --------------------------------------------------------------------
-- Render mode
-- --------------------------------------------------------------------

runRender :: GrammarRegistry -> Opts -> FilePath -> IO ()
runRender reg opts root = do
  (_files, allTags) <- extractAll reg opts root

  let graph = buildSymbolGraph allTags
      scores = pageRank graph opts.focusFiles [] defaultWeights
      cfg = defaultRenderConfig{RepoRender.maxTokens = opts.tokenBudget}
      rendered = renderRepoMap allTags scores graph cfg

  putStrLn $ "Token budget              : " <> show opts.tokenBudget
  putStrLn $ "Estimated tokens in output: " <> show (Text.length rendered `div` 4)
  if null opts.focusFiles
    then putStrLn "Personalisation           : none (pass --focus FILE to personalise)"
    else mapM_ (\f -> putStrLn $ "Focus                     : " <> f) opts.focusFiles
  putStrLn ""
  putStrLn "--- repo map ---"
  putStr (Text.unpack rendered)
  putStrLn "--- end ---"

-- | Extract all tags from a workspace, respecting --lang filter.
extractAll :: GrammarRegistry -> Opts -> FilePath -> IO ([FilePath], [Tag])
extractAll reg opts root = do
  files <- listFiles root
  let filtered = case opts.langFilter of
        Nothing -> files
        Just lang -> filter (\f -> languageForFile f == Just lang) files
  putStrLn $ "Indexing " <> show (length filtered) <> " files..."
  tagLists <- mapM (processFile reg) filtered
  let allTags = concat tagLists
  putStrLn $ "Tags extracted: " <> show (length allTags)
  putStrLn ""
  pure (filtered, allTags)

-- --------------------------------------------------------------------
-- Inspect mode
-- --------------------------------------------------------------------

runInspect :: GrammarRegistry -> FilePath -> IO ()
runInspect reg path = do
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn $ "File not found: " <> path
      exitFailure
    else do
      let lang = languageForFile path
      putStrLn $ "File:     " <> path
      putStrLn $ "Language: " <> maybe "(unknown)" Text.unpack lang
      putStrLn $ "Grammar:  " <> maybe "not loaded" (const "loaded") (lang >>= lookupLanguage reg)
      putStrLn ""

      src <- BS.readFile path
      tags <- extractTags reg path src

      putStrLn $ "Tags extracted: " <> show (length tags)
      putStrLn ""

      let defs = [t | t <- tags, t.tagKind == Definition]
          refs = [t | t <- tags, t.tagKind == Reference]
      putStrLn $ "Definitions (" <> show (length defs) <> "):"
      mapM_ printTag (sortOn (.tagLine) defs)
      putStrLn ""
      putStrLn $ "References (" <> show (length refs) <> "):"
      mapM_ printTag (sortOn (.tagLine) refs)

printTag :: Tag -> IO ()
printTag t =
  putStrLn $
    "  line "
      <> show (t.tagLine + 1)
      <> ": "
      <> Text.unpack t.tagName

-- --------------------------------------------------------------------
-- Dump AST mode
-- --------------------------------------------------------------------

runDumpAst :: GrammarRegistry -> FilePath -> IO ()
runDumpAst reg path = do
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn $ "File not found: " <> path
      exitFailure
    else do
      let lang = languageForFile path
      putStrLn $ "File:     " <> path
      putStrLn $ "Language: " <> maybe "(unknown)" Text.unpack lang
      putStrLn ""

      case lang >>= lookupLanguage reg of
        Nothing -> do
          putStrLn "No grammar loaded for this file type."
          exitFailure
        Just tsLang -> do
          src <- BS.readFile path
          parser <- TS.newParser
          ok <- TS.setLanguage parser tsLang
          if not ok
            then do
              putStrLn "setLanguage failed - ABI version mismatch between grammar and runtime."
              exitFailure
            else do
              mTree <- TS.parseSource parser src
              case mTree of
                Nothing -> putStrLn "parseSource returned Nothing (timeout or error)."
                Just treeFp -> do
                  root <- TS.rootNode treeFp
                  putStrLn "AST (named nodes only, depth <= 8):"
                  putStrLn ""
                  printNode 0 root
                  putStrLn ""
                  putStrLn "Node type summary (named nodes):"
                  typeSet <- collectTypes 8 root
                  mapM_ (\t -> putStrLn $ "  " <> Text.unpack t) (Set.toAscList typeSet)

printNode :: Int -> Node -> IO ()
printNode depth node = do
  isNamed <- TS.nodeIsNamed node
  when isNamed $ do
    typ <- TS.nodeType node
    pt <- TS.nodeStartPoint node
    count <- TS.nodeChildCount node
    let indent = replicate (depth * 2) ' '
        loc = "L" <> show (pt.row + 1) <> "C" <> show (pt.column)
    putStrLn $ indent <> Text.unpack typ <> "  [" <> loc <> "]"
    when (depth < 8 && count > 0) $ do
      children <- namedChildrenOf node count
      mapM_ (printNode (depth + 1)) children

collectTypes :: Int -> Node -> IO (Set Text)
collectTypes maxDepth node = go 0 node Set.empty
 where
  go depth n acc
    | depth > maxDepth = pure acc
    | otherwise = do
        isNamed <- TS.nodeIsNamed n
        if not isNamed
          then pure acc
          else do
            typ <- TS.nodeType n
            count <- TS.nodeChildCount n
            let acc' = Set.insert typ acc
            if count == 0
              then pure acc'
              else do
                children <- namedChildrenOf n count
                foldl (\m c -> m >>= \a -> go (depth + 1) c a) (pure acc') children

namedChildrenOf :: Node -> Word32 -> IO [Node]
namedChildrenOf node count
  | count == 0 = pure []
  | otherwise = go 0 []
 where
  go :: Word32 -> [Node] -> IO [Node]
  go i acc
    | i >= count = pure (reverse acc)
    | otherwise = do
        child <- TS.nodeChild node i
        isNull <- TS.nodeIsNull child
        if isNull
          then go (i + 1) acc
          else do
            isNamed <- TS.nodeIsNamed child
            if isNamed
              then go (i + 1) (child : acc)
              else go (i + 1) acc

-- --------------------------------------------------------------------
-- File listing
-- --------------------------------------------------------------------

listFiles :: FilePath -> IO [FilePath]
listFiles root = go root
 where
  go dir = do
    entries <- listDirectory dir
    let visible = filter (not . shouldSkip) entries
    fmap concat $ mapM (step dir) visible

  step dir name = do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir
      then go path
      else do
        isFile <- doesFileExist path
        if isFile then pure [path] else pure []

  shouldSkip name =
    ("." `isPrefixOf` name)
      || name `elem` excludedDirs

  excludedDirs =
    [ "dist-newstyle"
    , "dist"
    , ".stack-work"
    , "node_modules"
    , "__pycache__"
    , ".git"
    , "target"
    , "build"
    , ".cache"
    , "vendor"
    ]

printGrammarDiagnostics :: FilePath -> Int -> IO ()
printGrammarDiagnostics grammarDir loadedCount = do
  putStrLn $ "Grammars dir: " <> grammarDir
  putStrLn $ "Grammars loaded: " <> show loadedCount <> " / " <> show (length knownGrammars)
  mapM_ putStrLn (grammarWarnings grammarDir loadedCount)

grammarWarnings :: FilePath -> Int -> [String]
grammarWarnings grammarDir loadedCount
  | loadedCount == 0 =
      [ "WARNING: no grammars loaded. Repo-map extraction will skip supported files."
      , "  Remediation: run `codestar-cli fetch-grammars`."
      , "  Verify this path contains grammar libraries: " <> grammarDir
      , "  If this path is unexpected, check your XDG data directory env configuration."
      ]
  | loadedCount < max 3 (length knownGrammars `div` 4) =
      [ "WARNING: grammar load count is unexpectedly low."
      , "  Remediation: run `codestar-cli fetch-grammars` for missing languages."
      , "  Verify this path points to the grammar directory you expect: " <> grammarDir
      ]
  | otherwise = []

printStaleFingerprintSafetyRail :: Opts -> IO ()
printStaleFingerprintSafetyRail opts = do
  let workspace = modeWorkspace opts.mode
      cacheRoot = workspace </> ".codestar" </> "cache"
  backend <- newBackend cacheRoot
  printStaleFingerprintSafetyRailFor backend workspace

printStaleFingerprintSafetyRailFor :: StorageBackend -> FilePath -> IO ()
printStaleFingerprintSafetyRailFor backend workspace = do
  gcReport <- runCacheGc backend (Just workspace) False
  let staleGlobal = Map.findWithDefault 0 "stale-global" gcReport.staleByReason
      staleBoth = Map.findWithDefault 0 "stale-both" gcReport.staleByReason
      staleFingerprint = staleGlobal + staleBoth
  when (staleFingerprint > 0) $
    putStrLn $
      "Note: detected "
        <> show staleFingerprint
        <> " stale repo-map cache entries from a previous extractor/query fingerprint; these entries are ignored. "
        <> "Run `codestar-cli cache-gc --delete` to clean them."

modeWorkspace :: Mode -> FilePath
modeWorkspace = \case
  IndexDir root -> root
  CacheStats root -> root
  PageRankMode root -> root
  RenderMode root -> root
  InspectFile _ -> "."
  DumpAst _ -> "."
  CheckGrammar _ -> "."
