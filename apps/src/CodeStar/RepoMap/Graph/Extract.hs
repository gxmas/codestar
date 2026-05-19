{- |
= Graph.Extract — Tree-sitter tag extraction dispatcher

This module is the __entry point__ into the tag-extraction pipeline.  It
decides which language-specific extractor to invoke based on the file
extension, then delegates to one of:

  * 'extractHaskellWithLang' — query-driven + AST walk fallback
  * 'extractPythonWithLang' — query-driven + AST walk fallback
  * 'extractTypeScriptWithLang' — query-driven + AST walk fallback
  * 'extractGenericWithLang' — AST walk only, for all other supported languages

== Query loading

The preferred way to extract definitions is via a __Tree-sitter query__
(@.scm@ file).  Queries can be loaded from:

1. A file path given by @CODESTAR_QUERIES_DIR@ (for development / overrides).
2. The compiled-in 'ByteString' asset (the default for deployed builds).

If @CODESTAR_QUERIES_STRICT=1@ is set and the query file cannot be loaded
or compiled, extraction fails hard rather than silently falling back —
useful for catching query regressions in CI.

== Generic walker

For languages without a dedicated extractor, 'walkGenericNode' does a
depth-first traversal of the AST, emitting a 'Definition' tag for every
identifier directly inside a known definition node type and a 'Reference'
tag for every standalone identifier.
-}
module CodeStar.RepoMap.Graph.Extract
  ( extractTags
  , extractTagsDetailed
  , querySourceModeLabel
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Vector qualified as V
import Data.Word (Word32)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import TreeSitter (Language, Node)
import TreeSitter qualified as TS
import TreeSitter.Query (newQuery)

import CodeStar.RepoMap.Graph.Extract.Haskell qualified as Haskell
import CodeStar.RepoMap.Graph.Extract.Python qualified as Python
import CodeStar.RepoMap.Graph.Extract.TypeScript qualified as TypeScript
import CodeStar.RepoMap.Graph.Extract.Types
  ( ExtractionSkip (..)
  , Tag (..)
  , TagExtraction (..)
  , TagKind (..)
  , namedChildren
  , wordAt
  )
import CodeStar.TreeSitter (GrammarRegistry, languageForFile, lookupLanguage)

{- | Extract definition and reference tags from source using tree-sitter.
Returns an empty list when the language is unsupported or not installed.
-}
extractTags :: GrammarRegistry -> FilePath -> ByteString -> IO [Tag]
extractTags registry path src = do
  detailed <- extractTagsDetailed registry path src
  pure $ case detailed of
    Extracted tags -> tags
    _ -> []

-- | Like 'extractTags' but returns the full 'TagExtraction' so callers can
-- distinguish a successful empty result from a skip or failure.
extractTagsDetailed :: GrammarRegistry -> FilePath -> ByteString -> IO TagExtraction
extractTagsDetailed registry path src =
  case languageForFile path of
    Nothing -> pure (Skipped UnsupportedExtension)
    Just langName ->
      case lookupLanguage registry langName of
        Nothing -> pure (Skipped NoGrammarInstalled)
        Just lang
          | langName == "haskell" -> extractHaskellWithLang lang path src
          | langName == "python" -> extractPythonWithLang lang path src
          | langName == "typescript" || langName == "tsx" -> extractTypeScriptWithLang lang path src
          | otherwise -> extractGenericWithLang lang path src

extractGenericWithLang :: Language -> FilePath -> ByteString -> IO TagExtraction
extractGenericWithLang lang path src = do
  let srcLines = V.fromList (Text.lines (decodeUtf8Lenient src))
  parser <- TS.newParser
  ok <- TS.setLanguage parser lang
  if not ok
    then pure (ExtractFailed "failed to set parser language")
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure (ExtractFailed "parser returned no syntax tree")
        Just tree -> do
          tags <- TS.rootNode tree >>= walkGenericNode srcLines path
          pure (Extracted tags)

extractHaskellWithLang :: Language -> FilePath -> ByteString -> IO TagExtraction
extractHaskellWithLang lang path src = do
  let srcLines = V.fromList (Text.lines (decodeUtf8Lenient src))
  queryResult <- loadDefinitionQuery Haskell.haskellDefinitionQueryFile Haskell.haskellDefinitionQueryEmbedded
  parser <- TS.newParser
  ok <- TS.setLanguage parser lang
  if not ok
    then pure (ExtractFailed "failed to set parser language")
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure (ExtractFailed "parser returned no syntax tree")
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr ->
              pure (ExtractFailed queryErr)
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then pure (ExtractFailed ("strict query mode: invalid query for haskell-definitions.scm from " <> Text.pack querySource.origin))
                    else do
                      fallback <- newQuery lang Haskell.haskellDefinitionQueryEmbedded
                      case fallback of
                        Right query -> do
                          tags <- Haskell.extractHaskellDefinitionsByQuery srcLines path query root
                          pure (Extracted tags)
                        Left _ -> do
                          tags <- Haskell.walkHaskellDefinitions srcLines path root
                          pure (Extracted tags)
                Right query -> do
                  tags <- Haskell.extractHaskellDefinitionsByQuery srcLines path query root
                  pure (Extracted tags)

extractTypeScriptWithLang :: Language -> FilePath -> ByteString -> IO TagExtraction
extractTypeScriptWithLang lang path src = do
  let srcLines = V.fromList (Text.lines (decodeUtf8Lenient src))
  queryResult <- loadDefinitionQuery TypeScript.typeScriptDefinitionQueryFile TypeScript.typeScriptDefinitionQueryEmbedded
  parser <- TS.newParser
  ok <- TS.setLanguage parser lang
  if not ok
    then pure (ExtractFailed "failed to set parser language")
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure (ExtractFailed "parser returned no syntax tree")
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr ->
              pure (ExtractFailed queryErr)
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then pure (ExtractFailed ("strict query mode: invalid query for typescript-definitions.scm from " <> Text.pack querySource.origin))
                    else do
                      fallback <- newQuery lang TypeScript.typeScriptDefinitionQueryEmbedded
                      case fallback of
                        Right query -> do
                          tags <- TypeScript.extractTypeScriptDefinitionsByQuery srcLines path query root
                          pure (Extracted tags)
                        Left _ -> do
                          tags <- walkGenericNode srcLines path root
                          pure (Extracted tags)
                Right query -> do
                  tags <- TypeScript.extractTypeScriptDefinitionsByQuery srcLines path query root
                  pure (Extracted tags)

extractPythonWithLang :: Language -> FilePath -> ByteString -> IO TagExtraction
extractPythonWithLang lang path src = do
  let srcLines = V.fromList (Text.lines (decodeUtf8Lenient src))
  queryResult <- loadDefinitionQuery Python.pythonDefinitionQueryFile Python.pythonDefinitionQueryEmbedded
  parser <- TS.newParser
  ok <- TS.setLanguage parser lang
  if not ok
    then pure (ExtractFailed "failed to set parser language")
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure (ExtractFailed "parser returned no syntax tree")
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr ->
              pure (ExtractFailed queryErr)
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then pure (ExtractFailed ("strict query mode: invalid query for python-definitions.scm from " <> Text.pack querySource.origin))
                    else do
                      fallback <- newQuery lang Python.pythonDefinitionQueryEmbedded
                      case fallback of
                        Right query -> do
                          tags <- Python.extractPythonTagsByQuery srcLines path query root
                          pure (Extracted tags)
                        Left _ -> do
                          tags <- walkGenericNode srcLines path root
                          pure (Extracted tags)
                Right query -> do
                  tags <- Python.extractPythonTagsByQuery srcLines path query root
                  pure (Extracted tags)

-- | Depth-first AST walk for languages without a dedicated query.
-- Emits 'Definition' tags for identifier children of definition nodes
-- and 'Reference' tags for all other leaf identifiers.
-- Bounded by 'maxDepth' and 'maxChildren' to avoid pathological inputs.
walkGenericNode :: V.Vector Text -> FilePath -> Node -> IO [Tag]
walkGenericNode srcLines path = go 0
 where
  maxDepth :: Int
  maxDepth = 15
  maxChildren :: Word32
  maxChildren = 100 -- Limit children per node to avoid blowup
  go depth node
    | depth > maxDepth = pure []
    | otherwise = do
        isNamed <- TS.nodeIsNamed node
        if not isNamed
          then pure []
          else do
            typ <- TS.nodeType node
            count <- TS.nodeChildCount node
            if typ `Set.member` identifierTypes && count == 0
              then mkTag srcLines path Reference node
              else do
                children <- namedChildren node (min count (fromIntegral maxChildren))
                ownTags <-
                  if typ `Set.member` definitionTypes
                    then concat <$> mapM (tryExtractAsDefinition srcLines path) children
                    else pure []
                childTags <- concat <$> mapM (go (depth + 1)) children
                pure (ownTags ++ childTags)

-- | Emit a 'Definition' tag if the node is a leaf identifier, otherwise
-- return empty.  Used to extract the name from a definition node's children.
tryExtractAsDefinition :: V.Vector Text -> FilePath -> Node -> IO [Tag]
tryExtractAsDefinition srcLines path node = do
  typ <- TS.nodeType node
  count <- TS.nodeChildCount node
  if typ `Set.member` identifierTypes && count == 0
    then mkTag srcLines path Definition node
    else pure []

mkTag :: V.Vector Text -> FilePath -> TagKind -> Node -> IO [Tag]
mkTag srcLines path kind node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
  pure [Tag path name row kind | not (Text.null name)]

-- Grammar node types that introduce a new definition.
definitionTypes :: Set Text
definitionTypes =
  Set.fromList
    [ "function_definition"
    , "function_declaration"
    , "function_item"
    , "class_definition"
    , "class_declaration"
    , "method_definition"
    , "method_declaration"
    , "type_alias_declaration"
    , "type_declaration"
    , "interface_declaration"
    , "struct_item"
    , "enum_item"
    , "impl_item"
    , "trait_item"
    , "let_declaration"
    , "const_declaration"
    , "const_item"
    , "lexical_declaration"
    ]

-- Leaf node types that carry an identifier name.
identifierTypes :: Set Text
identifierTypes =
  Set.fromList
    [ "identifier"
    , "name"
    , "type_identifier"
    , "property_identifier"
    , "field_identifier"
    , "variable_name"
    , "variable"
    , "constructor"
    , "operator"
    ]

-- | A successfully loaded Tree-sitter query, together with metadata about
-- where it came from (for error messages) and whether a compilation failure
-- should be treated as fatal.
data LoadedQuery = LoadedQuery
  { bytes         :: !ByteString
  -- ^ The raw @.scm@ query source.
  , origin        :: !FilePath
  -- ^ Human-readable description of the source (e.g. @"embedded asset"@
  --   or a file path).
  , strictFailure :: !Bool
  -- ^ When 'True', a query compilation failure returns 'ExtractFailed'
  --   instead of silently falling back to the embedded query.
  }

-- | Return a human-readable description of the active query-loading mode,
-- shown in the CLI @\/status@ command so operators know which query files
-- are in effect.
querySourceModeLabel :: IO Text
querySourceModeLabel = do
  mDir <- lookupEnv "CODESTAR_QUERIES_DIR"
  strict <- isStrictQueryMode
  pure $ case mDir of
    Nothing -> "embedded"
    Just _ ->
      if strict
        then "filesystem-strict"
        else "filesystem-preferred-with-embedded-fallback"

-- | Load a query, trying the filesystem first (if @CODESTAR_QUERIES_DIR@ is
-- set) and falling back to the compiled-in @embedded@ bytes.
-- Returns @Left err@ only when strict mode is enabled and the file is
-- missing or unreadable.
loadDefinitionQuery :: FilePath -> ByteString -> IO (Either Text LoadedQuery)
loadDefinitionQuery fileName embedded = do
  mDir <- lookupEnv "CODESTAR_QUERIES_DIR"
  strict <- isStrictQueryMode
  case mDir of
    Nothing -> pure (Right (LoadedQuery embedded "embedded asset" False))
    Just dir -> do
      let queryPath = dir </> fileName
      exists <- doesFileExist queryPath
      if exists
        then do
          content <- BS.readFile queryPath
          pure (Right (LoadedQuery content queryPath strict))
        else
          if strict
            then pure (Left (Text.pack ("Strict query mode is enabled and query file is missing: " <> queryPath)))
            else pure (Right (LoadedQuery embedded ("embedded fallback (missing: " <> queryPath <> ")") False))

isStrictQueryMode :: IO Bool
isStrictQueryMode = do
  mStrict <- lookupEnv "CODESTAR_QUERIES_STRICT"
  pure (mStrict == Just "1")
