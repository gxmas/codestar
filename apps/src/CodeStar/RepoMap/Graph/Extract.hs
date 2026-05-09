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
import System.IO (hPutStrLn, stderr)
import TreeSitter (Language, Node)
import TreeSitter qualified as TS
import TreeSitter.Query (newQuery)

import CodeStar.RepoMap.Graph.Extract.Haskell qualified as Haskell
import CodeStar.RepoMap.Graph.Extract.Python qualified as Python
import CodeStar.RepoMap.Graph.Extract.TypeScript qualified as TypeScript
import CodeStar.RepoMap.Graph.Extract.Types
  ( Tag (..)
  , TagExtraction (..)
  , TagKind (..)
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

extractTagsDetailed :: GrammarRegistry -> FilePath -> ByteString -> IO TagExtraction
extractTagsDetailed registry path src =
  case languageForFile path of
    Nothing -> pure SkippedUnsupported
    Just langName ->
      case lookupLanguage registry langName of
        Nothing -> pure SkippedNoGrammar
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
    then pure ExtractFailed
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure ExtractFailed
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
    then pure ExtractFailed
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure ExtractFailed
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr -> do
              hPutStrLn stderr ("[codestar] " <> Text.unpack queryErr)
              pure ExtractFailed
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then do
                      hPutStrLn stderr ("[codestar] Strict query mode: invalid query for haskell-definitions.scm from " <> querySource.origin)
                      pure ExtractFailed
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
    then pure ExtractFailed
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure ExtractFailed
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr -> do
              hPutStrLn stderr ("[codestar] " <> Text.unpack queryErr)
              pure ExtractFailed
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then do
                      hPutStrLn stderr ("[codestar] Strict query mode: invalid query for typescript-definitions.scm from " <> querySource.origin)
                      pure ExtractFailed
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
    then pure ExtractFailed
    else do
      mTree <- TS.parseSource parser src
      case mTree of
        Nothing -> pure ExtractFailed
        Just tree -> do
          root <- TS.rootNode tree
          case queryResult of
            Left queryErr -> do
              hPutStrLn stderr ("[codestar] " <> Text.unpack queryErr)
              pure ExtractFailed
            Right querySource -> do
              eQuery <- newQuery lang querySource.bytes
              case eQuery of
                Left _ ->
                  if querySource.strictFailure
                    then do
                      hPutStrLn stderr ("[codestar] Strict query mode: invalid query for python-definitions.scm from " <> querySource.origin)
                      pure ExtractFailed
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

namedChildren :: Node -> Word32 -> IO [Node]
namedChildren node count = do
  -- Avoid unsigned-underflow range bugs: when count == 0, [0 .. count - 1]
  -- would expand to a massive list ([0 .. maxBound]) and appear to hang.
  if count == 0
    then pure []
    else filterNamedM 0
 where
  filterNamedM i
    | i >= count = pure []
    | otherwise = do
        child <- TS.nodeChild node i
        isNull <- TS.nodeIsNull child
        if isNull
          then filterNamedM (i + 1)
          else do
            isNamed <- TS.nodeIsNamed child
            if isNamed
              then (child :) <$> filterNamedM (i + 1)
              else filterNamedM (i + 1)

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

data LoadedQuery = LoadedQuery
  { bytes :: !ByteString
  , origin :: !FilePath
  , strictFailure :: !Bool
  }

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
