module CodeStar.RepoMap.Graph.Extract.TypeScript
  ( typeScriptDefinitionQueryFile
  , typeScriptDefinitionQueryEmbedded
  , extractTypeScriptDefinitionsByQuery
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import TreeSitter (Node)
import TreeSitter qualified as TS
import TreeSitter.Query (Query, QueryCapture (..), queryCaptures)

import CodeStar.RepoMap.Graph.Extract.Types (Tag (..), TagKind (..), wordAt)

typeScriptDefinitionQueryFile :: FilePath
typeScriptDefinitionQueryFile = "typescript-definitions.scm"

extractTypeScriptDefinitionsByQuery :: V.Vector Text -> FilePath -> Query -> Node -> IO [Tag]
extractTypeScriptDefinitionsByQuery srcLines path query root = do
  captures <- queryCaptures query root
  defs <- concat <$> mapM (captureToTypeScriptDefinition srcLines path) captures
  refs <- concat <$> mapM (captureToTypeScriptReference srcLines path) captures
  pure (defs ++ refs)

captureToTypeScriptDefinition :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToTypeScriptDefinition srcLines path capture =
  if not (capture.captureName `Set.member` typeScriptAllowedCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if typ `Set.member` identifierTypes && count == 0
        then mkTypeScriptDefinitionTag srcLines path capture.captureNode
        else pure []

captureToTypeScriptReference :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToTypeScriptReference srcLines path capture =
  if not (capture.captureName `Set.member` typeScriptReferenceCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if typ `Set.member` identifierTypes && count == 0
        then mkTypeScriptReferenceTag srcLines path capture.captureNode
        else pure []

mkTypeScriptDefinitionTag :: V.Vector Text -> FilePath -> Node -> IO [Tag]
mkTypeScriptDefinitionTag srcLines path node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
  pure
    [ Tag path name row Definition
    | not (Text.null name)
    ]

mkTypeScriptReferenceTag :: V.Vector Text -> FilePath -> Node -> IO [Tag]
mkTypeScriptReferenceTag srcLines path node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
  pure
    [ Tag path name row Reference
    | not (Text.null name)
    ]

typeScriptAllowedCaptures :: Set Text
typeScriptAllowedCaptures =
  Set.fromList
    [ "def.function.name"
    , "def.class.name"
    , "def.method.name"
    , "def.interface.name"
    , "def.type.name"
    , "def.enum.name"
    , "def.module.name"
    , "def.namespace.name"
    ]

typeScriptReferenceCaptures :: Set Text
typeScriptReferenceCaptures =
  Set.fromList
    [ "ref.class.name"
    , "ref.type.name"
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

typeScriptDefinitionQueryEmbedded :: ByteString
typeScriptDefinitionQueryEmbedded =
  BS.intercalate "\n"
    [ "; TypeScript declaration-name captures for RepoMap."
    , "; Focus on declaration heads, avoid local variable references."
    , ""
    , "; functions"
    , "(function_declaration"
    , "  name: (identifier) @def.function.name)"
    , "(generator_function_declaration"
    , "  name: (identifier) @def.function.name)"
    , ""
    , "; classes and methods"
    , "(class_declaration"
    , "  name: (type_identifier) @def.class.name)"
    , "(abstract_class_declaration"
    , "  name: (type_identifier) @def.class.name)"
    , "(method_definition"
    , "  name: (property_identifier) @def.method.name)"
    , "(method_definition"
    , "  name: (private_property_identifier) @def.method.name)"
    , ""
    , "; type-level declarations"
    , "(interface_declaration"
    , "  name: (type_identifier) @def.interface.name)"
    , "(type_alias_declaration"
    , "  name: (type_identifier) @def.type.name)"
    , "(enum_declaration"
    , "  name: (identifier) @def.enum.name)"
    , "(enum_declaration"
    , "  name: (type_identifier) @def.enum.name)"
    , ""
    , "; namespaces/modules"
    , "(internal_module"
    , "  name: (identifier) @def.module.name)"
    , ""
    , "; type/class references"
    , "(type_annotation"
    , "  (type_identifier) @ref.type.name)"
    , "(new_expression"
    , "  constructor: (identifier) @ref.class.name)"
    , "(new_expression"
    , "  constructor: (type_identifier) @ref.class.name)"
    ]
