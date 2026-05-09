module CodeStar.RepoMap.Graph.Extract.Python
  ( pythonDefinitionQueryFile
  , pythonDefinitionQueryEmbedded
  , extractPythonTagsByQuery
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

pythonDefinitionQueryFile :: FilePath
pythonDefinitionQueryFile = "python-definitions.scm"

extractPythonTagsByQuery :: V.Vector Text -> FilePath -> Query -> Node -> IO [Tag]
extractPythonTagsByQuery srcLines path query root = do
  captures <- queryCaptures query root
  defs <- concat <$> mapM (captureToPythonDefinition srcLines path) captures
  refs <- concat <$> mapM (captureToPythonReference srcLines path) captures
  pure (defs ++ refs)

captureToPythonDefinition :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToPythonDefinition srcLines path capture =
  if not (capture.captureName `Set.member` pythonDefinitionCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if typ `Set.member` identifierTypes && count == 0
        then mkTag srcLines path Definition capture.captureNode
        else pure []

captureToPythonReference :: V.Vector Text -> FilePath -> QueryCapture -> IO [Tag]
captureToPythonReference srcLines path capture =
  if not (capture.captureName `Set.member` pythonReferenceCaptures)
    then pure []
    else do
      typ <- TS.nodeType capture.captureNode
      count <- TS.nodeChildCount capture.captureNode
      if typ `Set.member` identifierTypes && count == 0
        then mkTag srcLines path Reference capture.captureNode
        else pure []

mkTag :: V.Vector Text -> FilePath -> TagKind -> Node -> IO [Tag]
mkTag srcLines path kind node = do
  pt <- TS.nodeStartPoint node
  let row = fromIntegral pt.row
      col = fromIntegral pt.column
      name = wordAt srcLines row col
  pure [Tag path name row kind | not (Text.null name)]

pythonDefinitionCaptures :: Set Text
pythonDefinitionCaptures =
  Set.fromList
    [ "def.class.name"
    , "def.function.name"
    ]

pythonReferenceCaptures :: Set Text
pythonReferenceCaptures =
  Set.fromList
    [ "ref.call.name"
    ]

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

pythonDefinitionQueryEmbedded :: ByteString
pythonDefinitionQueryEmbedded =
  BS.intercalate "\n"
    [ "; Python declaration/reference captures for RepoMap."
    , ""
    , "; declarations"
    , "(class_definition"
    , "  name: (identifier) @def.class.name)"
    , "(function_definition"
    , "  name: (identifier) @def.function.name)"
    , ""
    , "; call references"
    , "(call"
    , "  function: (identifier) @ref.call.name)"
    , "(call"
    , "  function: (attribute"
    , "    attribute: (identifier) @ref.call.name))"
    ]
