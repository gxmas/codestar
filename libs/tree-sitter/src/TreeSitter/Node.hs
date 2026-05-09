module TreeSitter.Node
  ( Node (..)
  , rootNode
  , nodeHasError
  , nodeIsNamed
  , nodeIsNull
  , nodeType
  , nodeStartPoint
  , nodeChildCount
  , nodeChild
  , Point (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Foreign
import Foreign.C

import TreeSitter.FFI (TSTreeRaw, TSNode, TSPoint (..))
import qualified TreeSitter.FFI as FFI


data Point = Point
  { row    :: !Word32
  , column :: !Word32
  } deriving stock (Eq, Show)

data Node = Node
  { nodeData  :: !TSNode
  , keepAlive :: !(ForeignPtr TSTreeRaw)
  }

rootNode :: ForeignPtr TSTreeRaw -> IO Node
rootNode treeFp =
  withForeignPtr treeFp $ \treePtr ->
    alloca $ \nodePtr -> do
      FFI.ts_tree_root_node treePtr nodePtr
      n <- peek nodePtr
      pure (Node n treeFp)

nodeHasError :: Node -> IO Bool
nodeHasError (Node n ka) =
  alloca $ \ptr -> do
    poke ptr n
    result <- FFI.ts_node_has_error ptr
    touchForeignPtr ka
    pure (result /= 0)

nodeIsNamed :: Node -> IO Bool
nodeIsNamed (Node n ka) =
  alloca $ \ptr -> do
    poke ptr n
    result <- FFI.ts_node_is_named ptr
    touchForeignPtr ka
    pure (result /= 0)

nodeType :: Node -> IO Text
nodeType (Node n ka) =
  alloca $ \ptr -> do
    poke ptr n
    cstr <- FFI.ts_node_type ptr
    result <- Text.pack <$> peekCString cstr
    touchForeignPtr ka
    pure result

nodeStartPoint :: Node -> IO Point
nodeStartPoint (Node n ka) =
  alloca $ \nodePtr ->
    alloca $ \pointPtr -> do
      poke nodePtr n
      FFI.ts_node_start_point nodePtr pointPtr
      TSPoint r c <- peek pointPtr
      touchForeignPtr ka
      pure (Point r c)

nodeChildCount :: Node -> IO Word32
nodeChildCount (Node n ka) =
  alloca $ \ptr -> do
    poke ptr n
    result <- fromIntegral <$> FFI.ts_node_child_count ptr
    touchForeignPtr ka
    pure result

nodeChild :: Node -> Word32 -> IO Node
nodeChild (Node n ka) idx =
  alloca $ \parentPtr ->
    alloca $ \childPtr -> do
      poke parentPtr n
      FFI.ts_node_child parentPtr (fromIntegral idx) childPtr
      child <- peek childPtr
      pure (Node child ka)

nodeIsNull :: Node -> IO Bool
nodeIsNull (Node n ka) =
  alloca $ \ptr -> do
    poke ptr n
    result <- FFI.ts_node_is_null ptr
    touchForeignPtr ka
    pure (result /= 0)
