{- |
= Graph.Extract.Haskell — Haskell-specific tag extraction

Haskell's grammar is unusual in that top-level definitions always start at
column 0, while local @where@\/@let@\/@do@ bindings are indented.  This
module exploits that invariant to produce clean definition tags without
capturing local variables as false positives.

== Two extraction strategies

  1. __Query-driven__ ('extractHaskellDefinitionsByQuery'): uses a
     Tree-sitter @.scm@ query to precisely capture declaration heads.
     This is the preferred path when grammars and queries are available.

  2. __AST walk__ ('walkHaskellDefinitions'): a depth-first traversal
     that tracks whether the current node is inside a definition context
     and emits a tag for any leaf identifier at column 0.  Used as a
     fallback when query compilation fails.

The 'haskellDefinitionQueryEmbedded' constant contains the compiled-in
@.scm@ query so the binary works without any external files.
-}
module CodeStar.RepoMap.Graph.Extract.Haskell
  ( haskellDefinitionQueryFile
  , haskellDefinitionQueryEmbedded
  , extractHaskellDefinitionsByQuery
  , walkHaskellDefinitions
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word32)
import TreeSitter (Node)
import TreeSitter qualified as TS
import TreeSitter.Query (Query, QueryCapture (..), queryCaptures)

import CodeStar.RepoMap.Graph.Extract.Types (Tag (..), TagKind (..), namedChildren, wordAt)

-- | File name (not full path) of the Haskell definition query.
-- 'loadDefinitionQuery' looks for this file under @CODESTAR_QUERIES_DIR@.
haskellDefinitionQueryFile :: FilePath
haskellDefinitionQueryFile = "haskell-definitions.scm"

-- | Run the Haskell definition query against the parse tree, collecting
-- both definition tags (from 'captureToDefinition') and import-reference
-- tags (from 'captureToHaskellReference').
extractHaskellDefinitionsByQuery :: V.Vector Text -> FilePath -> Query -> Node -> IO [Tag]
extractHaskellDefinitionsByQuery srcLines path query root = do
  captures <- queryCaptures query root
  defs <- concat <$> mapM (captureToDefinition srcLines path) captures
  refs <- concat <$> mapM (captureToHaskellReference srcLines path) captures
  pure (defs ++ refs)

-- | AST-walk fallback for when the definition query cannot be compiled.
-- Traverses the parse tree tracking whether the current node is inside a
-- definition context (@insideDef@) and emits a 'Definition' tag for any
-- leaf identifier at column 0 that is not a noise word.
walkHaskellDefinitions :: V.Vector Text -> FilePath -> Node -> IO [Tag]
walkHaskellDefinitions srcLines path = go 0 False
 where
  maxDepth :: Int
  maxDepth = 20
  maxChildren :: Word32
  maxChildren = 120

  go depth insideDef node
    | depth > maxDepth = pure []
    | otherwise = do
        isNamed <- TS.nodeIsNamed node
        if not isNamed
          then pure []
          else do
            typ <- TS.nodeType node
            count <- TS.nodeChildCount node
            let startsDef = typ `Set.member` haskellDefaultDefinitionTypes
                exitsDef = typ `Set.member` haskellDefinitionBoundaryTypes
                insideDef' = startsDef || (insideDef && not exitsDef)

            if typ `Set.member` identifierTypes && count == 0 && insideDef'
              then mkHaskellDefinitionTag srcLines path node
              else do
                children <- namedChildren node (min count (fromIntegral maxChildren))
                concat <$> mapM (go (depth + 1) insideDef') children

-- | Convert a query capture to zero or more definition tags.
-- Captures whose name is not in 'haskellAllowedCaptures' are ignored.
-- Module names are handled specially via 'mkModuleTag'.
captureToDefinition :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToDefinition srcLines path capture =
  if not (capture.captureName `Set.member` haskellAllowedCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if count /= 0
        then pure []
        else if typ `Set.member` identifierTypes
          then mkHaskellDefinitionTag srcLines path capture.captureNode
          else if typ == "module_id"
            then mkModuleTag Definition srcLines path capture.captureNode
            else pure []

-- | Convert an import-reference capture to a 'Reference' tag.
-- Only 'haskellReferenceCaptures' (@ref.import.module@) are handled;
-- all other captures produce an empty list.
captureToHaskellReference :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToHaskellReference srcLines path capture =
  if not (capture.captureName `Set.member` haskellReferenceCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if typ == "module_id" && count == 0
        then mkModuleTag Reference srcLines path capture.captureNode
        else pure []

-- | Emit a tag from a module_id node without the col==0 filter.
-- Used for module declarations and import references where the name
-- appears mid-line (e.g. "Graph" in "module CodeStar.RepoMap.Graph").
mkModuleTag :: TagKind -> V.Vector Text -> FilePath -> Node -> IO [Tag]
mkModuleTag kind srcLines path node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
  pure [Tag path name row kind | not (Text.null name)]

mkHaskellDefinitionTag :: V.Vector Text -> FilePath -> Node -> IO [Tag]
mkHaskellDefinitionTag srcLines path node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
      -- Top-level Haskell definitions (functions, type signatures, data
      -- declarations …) always start at column 0. Identifiers at column > 0
      -- appear inside where/let/do blocks and would produce spurious or
      -- duplicate tags, so we exclude them.
      isTopLevel = col == 0
  pure
    [ Tag path name row Definition
    | not (Text.null name)
    , isTopLevel
    , not (name `Set.member` haskellNoiseNames)
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

-- Definition-like nodes observed in Haskell grammar.
haskellDefaultDefinitionTypes :: Set Text
haskellDefaultDefinitionTypes =
  Set.fromList
    [ "function"
    , "bind"
    , "signature"
    , "type_signature"
    , "data_type"
    , "newtype"
    , "type_synonym"
    , "type_family"
    , "class"
    , "instance"
    , "foreign_import"
    , "module"
    ]

-- Nodes that mark transition into expression bodies where most identifiers
-- are references rather than declaration names.
haskellDefinitionBoundaryTypes :: Set Text
haskellDefinitionBoundaryTypes =
  Set.fromList
    [ "where"
    , "let"
    , "do"
    , "case"
    , "lambda"
    , "exp"
    , "expression"
    , "guard"
    ]

-- | Common Haskell type names that are legal identifiers but not useful
-- definition tags — they appear pervasively as type annotations and
-- constructor patterns, not as top-level definition heads.
haskellNoiseNames :: Set Text
haskellNoiseNames =
  Set.fromList
    [ "IO"
    , "Int"
    , "Integer"
    , "Text"
    , "Maybe"
    , "Either"
    , "Bool"
    , "String"
    , "Char"
    , "Double"
    , "Float"
    , "Word"
    , "Word32"
    , "Word64"
    , "FilePath"
    ]

haskellAllowedCaptures :: Set Text
haskellAllowedCaptures =
  Set.fromList
    [ "def.function.name"
    , "def.bind.name"
    , "def.signature.name"
    , "def.type.name"
    , "def.class.name"
    , "def.instance.name"
    , "def.foreign_import.name"
    , "def.module.name"
    ]

haskellReferenceCaptures :: Set Text
haskellReferenceCaptures =
  Set.fromList
    [ "ref.import.module"
    ]

-- | The built-in Haskell definition query, compiled into the binary.
-- This is the fallback when no external @haskell-definitions.scm@ file
-- is available.  The query captures declaration heads (functions,
-- signatures, type definitions, class\/instance declarations, module names)
-- and avoids local expression identifiers.
haskellDefinitionQueryEmbedded :: ByteString
haskellDefinitionQueryEmbedded =
  BS.intercalate "\n"
    [ "; Haskell declaration-name captures for RepoMap."
    , "; Goal: capture declaration heads, avoid local expression identifiers."
    , ""
    , "; value/function declarations"
    , "(function"
    , "  [(variable) (operator) (identifier)] @def.function.name)"
    , "(bind"
    , "  [(variable) (operator) (identifier)] @def.bind.name)"
    , ""
    , "; signatures"
    , "(signature"
    , "  [(variable) (operator) (identifier)] @def.signature.name)"
    , "(type_signature"
    , "  [(variable) (operator) (identifier)] @def.signature.name)"
    , ""
    , "; type-level declarations"
    , "(data_type"
    , "  [(type) (type_identifier) (constructor) (identifier)] @def.type.name)"
    , "(newtype"
    , "  [(type) (type_identifier) (constructor) (identifier)] @def.type.name)"
    , "(type_synonym"
    , "  [(type) (type_identifier) (constructor) (identifier)] @def.type.name)"
    , "(type_family"
    , "  [(type) (type_identifier) (constructor) (identifier)] @def.type.name)"
    , "(class"
    , "  [(type) (type_identifier) (identifier)] @def.class.name)"
    , "(instance"
    , "  [(type) (type_identifier) (identifier)] @def.instance.name)"
    , ""
    , "; module/import-like declarations"
    , "(foreign_import"
    , "  [(variable) (identifier)] @def.foreign_import.name)"
    , ""
    , "; Module declaration (all components — last is the canonical leaf name)."
    , "; Anchored inside `header` so it never fires on module paths inside imports."
    , "(header"
    , "  (module (module_id) @def.module.name))"
    , ""
    , "; Import references: all components of the imported module path."
    , "(import (module (module_id) @ref.import.module))"
    ]
