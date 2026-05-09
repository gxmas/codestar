module CodeStar.TreeSitter.Grammars
  ( GrammarSpec (..)
  , knownGrammars
  , grammarByExtension
  , fetchGrammar
  , fetchAllGrammars
  , grammarLibPath
  , grammarsDir
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , renameDirectory
  )
import System.FilePath ((</>))

import CodeStar.Config.Paths qualified as Paths
import System.Info (os)
import System.Process.Typed

-- --------------------------------------------------------------------
-- Grammar Specification
-- --------------------------------------------------------------------

data GrammarSpec = GrammarSpec
  { language :: Text
  , githubRepo :: Text
  , version :: Text
  , symbol :: Text
  , extensions :: [Text]
  , hasScanner :: Bool
  , subdir :: Maybe FilePath
  }
  deriving stock (Show)

spec :: Text -> Text -> Text -> Text -> [Text] -> Bool -> GrammarSpec
spec lang repo ver sym exts scanner =
  GrammarSpec
    { language = lang
    , githubRepo = repo
    , version = ver
    , symbol = sym
    , extensions = exts
    , hasScanner = scanner
    , subdir = Nothing
    }

specSub :: Text -> Text -> Text -> Text -> [Text] -> Bool -> FilePath -> GrammarSpec
specSub lang repo ver sym exts scanner sub =
  (spec lang repo ver sym exts scanner){subdir = Just sub}

-- --------------------------------------------------------------------
-- Known Grammars (30+ languages)
-- --------------------------------------------------------------------

knownGrammars :: [GrammarSpec]
knownGrammars =
  [ spec "python" "tree-sitter/tree-sitter-python" "v0.23.6" "tree_sitter_python" [".py", ".pyi"] True
  , spec "javascript" "tree-sitter/tree-sitter-javascript" "v0.23.1" "tree_sitter_javascript" [".js", ".mjs", ".cjs"] True
  , specSub "typescript" "tree-sitter/tree-sitter-typescript" "v0.23.2" "tree_sitter_typescript" [".ts"] True "typescript"
  , specSub "tsx" "tree-sitter/tree-sitter-typescript" "v0.23.2" "tree_sitter_tsx" [".tsx"] True "tsx"
  , spec "rust" "tree-sitter/tree-sitter-rust" "v0.23.0" "tree_sitter_rust" [".rs"] True
  , spec "go" "tree-sitter/tree-sitter-go" "v0.23.4" "tree_sitter_go" [".go"] False
  , spec "c" "tree-sitter/tree-sitter-c" "v0.23.4" "tree_sitter_c" [".c", ".h"] False
  , spec "cpp" "tree-sitter/tree-sitter-cpp" "v0.23.4" "tree_sitter_cpp" [".cpp", ".cc", ".cxx", ".hpp"] True
  , spec "java" "tree-sitter/tree-sitter-java" "v0.23.5" "tree_sitter_java" [".java"] False
  , spec "haskell" "tree-sitter/tree-sitter-haskell" "v0.23.1" "tree_sitter_haskell" [".hs", ".lhs"] True
  , spec "ruby" "tree-sitter/tree-sitter-ruby" "v0.23.1" "tree_sitter_ruby" [".rb"] True
  , spec "bash" "tree-sitter/tree-sitter-bash" "v0.23.3" "tree_sitter_bash" [".sh", ".bash"] True
  , spec "json" "tree-sitter/tree-sitter-json" "v0.24.8" "tree_sitter_json" [".json"] False
  , spec "yaml" "tree-sitter-grammars/tree-sitter-yaml" "v0.7.0" "tree_sitter_yaml" [".yaml", ".yml"] True
  , spec "toml" "tree-sitter/tree-sitter-toml" "v0.5.1" "tree_sitter_toml" [".toml"] True
  , spec "html" "tree-sitter/tree-sitter-html" "v0.23.2" "tree_sitter_html" [".html", ".htm"] True
  , spec "css" "tree-sitter/tree-sitter-css" "v0.23.2" "tree_sitter_css" [".css"] True
  , spec "c_sharp" "tree-sitter/tree-sitter-c-sharp" "v0.23.1" "tree_sitter_c_sharp" [".cs"] True
  , spec "kotlin" "fwcd/tree-sitter-kotlin" "0.3.8" "tree_sitter_kotlin" [".kt", ".kts"] True
  , spec "swift" "alex-pinkus/tree-sitter-swift" "0.7.2-with-generated-files" "tree_sitter_swift" [".swift"] True
  , spec "scala" "tree-sitter/tree-sitter-scala" "v0.23.4" "tree_sitter_scala" [".scala"] True
  , specSub "ocaml" "tree-sitter/tree-sitter-ocaml" "v0.23.2" "tree_sitter_ocaml" [".ml"] True "grammars/ocaml"
  , spec "elixir" "elixir-lang/tree-sitter-elixir" "v0.3.4" "tree_sitter_elixir" [".ex", ".exs"] True
  , spec "lua" "tree-sitter-grammars/tree-sitter-lua" "v0.2.0" "tree_sitter_lua" [".lua"] True
  , specSub "php" "tree-sitter/tree-sitter-php" "v0.23.11" "tree_sitter_php" [".php"] True "php"
  , spec "dockerfile" "camdencheek/tree-sitter-dockerfile" "v0.2.0" "tree_sitter_dockerfile" ["Dockerfile"] True
  , specSub "markdown" "tree-sitter-grammars/tree-sitter-markdown" "v0.4.1" "tree_sitter_markdown" [".md", ".markdown"] True "tree-sitter-markdown"
  , spec "perl" "ganezdragon/tree-sitter-perl" "v1.1.1" "tree_sitter_perl" [".pl", ".pm"] True
  , spec "r" "r-lib/tree-sitter-r" "v1.1.0" "tree_sitter_r" [".r", ".R"] True
  ]

grammarByExtension :: Map Text GrammarSpec
grammarByExtension =
  Map.fromList
    [ (ext, grammar)
    | grammar <- knownGrammars
    , ext <- grammar.extensions
    ]

-- --------------------------------------------------------------------
-- Paths
-- --------------------------------------------------------------------

grammarsDir :: IO FilePath
grammarsDir = Paths.grammarsDir

grammarLibPath :: FilePath -> GrammarSpec -> FilePath
grammarLibPath dir grammar =
  dir </> "libtree-sitter-" <> Text.unpack grammar.language <> libExt
 where
  libExt
    | os == "darwin" = ".dylib"
    | otherwise = ".so"

-- --------------------------------------------------------------------
-- Fetch and Compile
-- --------------------------------------------------------------------

fetchGrammar :: FilePath -> GrammarSpec -> IO (Either Text ())
fetchGrammar dir grammar = do
  let libPath = grammarLibPath dir grammar
  exists <- doesFileExist libPath
  if exists
    then pure (Right ())
    else downloadAndCompile dir grammar

downloadAndCompile :: FilePath -> GrammarSpec -> IO (Either Text ())
downloadAndCompile dir grammar = do
  createDirectoryIfMissing True dir
  let archivePath = dir </> Text.unpack grammar.language <> ".tar.gz"
      extractBase = dir </> Text.unpack grammar.language <> "-src"
  result <- try @SomeException $ do
    downloadArchive grammar archivePath
    extractArchive archivePath dir grammar
    compileGrammar dir grammar extractBase
  -- Clean up regardless of success or failure
  runProcess_ $ proc "rm" ["-rf", archivePath, extractBase]
  pure $ case result of
    Left ex -> Left (Text.pack (show ex))
    Right () -> Right ()

downloadArchive :: GrammarSpec -> FilePath -> IO ()
downloadArchive grammar dest = do
  let url =
        "https://github.com/"
          <> Text.unpack grammar.githubRepo
          <> "/archive/refs/tags/"
          <> Text.unpack grammar.version
          <> ".tar.gz"
  mgr <- newManager tlsManagerSettings
  req <- parseRequest url
  resp <- httpLbs req mgr
  let status = responseStatus resp
  if statusCode status >= 400
    then error $ "Download failed (" <> show (statusCode status) <> "): " <> url
    else LBS.writeFile dest (responseBody resp)

extractArchive :: FilePath -> FilePath -> GrammarSpec -> IO ()
extractArchive archivePath dir grammar = do
  runProcess_ $ proc "tar" ["-xzf", archivePath, "-C", dir]
  -- GitHub archives extract to <repo>-<version>/ (e.g. tree-sitter-haskell-0.23.1/)
  -- We predict the name from the repo and version tag.
  let repoName = Text.unpack (snd (Text.breakOnEnd "/" grammar.githubRepo))
      ver = Text.unpack (Text.dropWhile (== 'v') grammar.version)
      expected = repoName <> "-" <> ver
      extractBase = dir </> Text.unpack grammar.language <> "-src"
      srcPath = dir </> expected
  srcExists <- doesDirectoryExist srcPath
  if srcExists
    then renameDirectory srcPath extractBase
    else pure ()

compileGrammar :: FilePath -> GrammarSpec -> FilePath -> IO ()
compileGrammar dir grammar srcBase = do
  let libPath = grammarLibPath dir grammar
      srcDir = case grammar.subdir of
        Nothing -> srcBase </> "src"
        Just sub -> srcBase </> sub </> "src"
      parserC = srcDir </> "parser.c"
      scannerC = srcDir </> "scanner.c"
      scannerCpp = srcDir </> "scanner.cc"
  hasCpp <- doesFileExist scannerCpp
  let extraSrc
        | not grammar.hasScanner = []
        | hasCpp = [scannerCpp]
        | otherwise = [scannerC]
      srcRoot = case grammar.subdir of
        Nothing -> srcBase
        Just sub -> srcBase </> sub
      -- On macOS, -dynamiclib produces a .dylib; -shared is the Linux equivalent.
      -- Grammar shared libraries don't call into the tree-sitter runtime — they only
      -- define the TSLanguage struct, so no -ltree-sitter linkage is needed here.
      -- The tree-sitter runtime is compiled directly into the codestar binary.
      platformFlags
        | os == "darwin" = ["-dynamiclib"]
        | otherwise = []
      args =
        ["-shared", "-fPIC"]
          <> platformFlags
          <> [ "-I"
             , srcDir
             , "-I"
             , srcRoot
             , "-o"
             , libPath
             , parserC
             ]
          <> extraSrc
  runProcess_ (proc "cc" args)

-- --------------------------------------------------------------------
-- Fetch All
-- --------------------------------------------------------------------

fetchAllGrammars :: FilePath -> (Text -> IO ()) -> IO [(GrammarSpec, Either Text ())]
fetchAllGrammars dir onProgress = do
  createDirectoryIfMissing True dir
  mapM (fetchOne dir) knownGrammars
 where
  fetchOne dir' grammar = do
    onProgress grammar.language
    result <- fetchGrammar dir' grammar
    pure (grammar, result)
