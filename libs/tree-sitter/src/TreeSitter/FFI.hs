module TreeSitter.FFI where

import Foreign
import Foreign.C


-- --------------------------------------------------------------------
-- Opaque types
-- --------------------------------------------------------------------

data TSParserRaw
data TSTreeRaw
data TSLanguageRaw
data TSQueryRaw
data TSQueryCursorRaw


-- --------------------------------------------------------------------
-- TSPoint (value type)
-- --------------------------------------------------------------------

data TSPoint = TSPoint
  { row    :: {-# UNPACK #-} !Word32
  , column :: {-# UNPACK #-} !Word32
  } deriving stock (Eq, Show)

instance Storable TSPoint where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = TSPoint
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
  poke ptr (TSPoint r c) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 4 c


-- --------------------------------------------------------------------
-- TSNode (value type — passed via pointer through C shim)
-- --------------------------------------------------------------------

data TSNode = TSNode
  { context :: (Word32, Word32, Word32, Word32)
  , nodeId  :: {-# UNPACK #-} !(Ptr ())
  , tree    :: {-# UNPACK #-} !(Ptr TSTreeRaw)
  } deriving stock (Eq, Show)

instance Storable TSNode where
  sizeOf _ = 4 * 4 + 2 * sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr = do
    c0 <- peekByteOff ptr 0
    c1 <- peekByteOff ptr 4
    c2 <- peekByteOff ptr 8
    c3 <- peekByteOff ptr 12
    nid <- peekByteOff ptr 16
    tr <- peekByteOff ptr (16 + sizeOf (undefined :: Ptr ()))
    pure $ TSNode (c0, c1, c2, c3) nid tr
  poke ptr (TSNode (c0, c1, c2, c3) nid tr) = do
    pokeByteOff ptr 0 c0
    pokeByteOff ptr 4 c1
    pokeByteOff ptr 8 c2
    pokeByteOff ptr 12 c3
    pokeByteOff ptr 16 nid
    pokeByteOff ptr (16 + sizeOf (undefined :: Ptr ())) tr


-- --------------------------------------------------------------------
-- Parser functions
-- --------------------------------------------------------------------

foreign import ccall unsafe "ts_parser_new"
  ts_parser_new :: IO (Ptr TSParserRaw)

foreign import ccall unsafe "&ts_parser_delete"
  ts_parser_finalizer :: FinalizerPtr TSParserRaw

foreign import ccall unsafe "ts_parser_set_language"
  ts_parser_set_language :: Ptr TSParserRaw -> Ptr TSLanguageRaw -> IO CBool

foreign import ccall unsafe "ts_parser_parse_string"
  ts_parser_parse_string :: Ptr TSParserRaw -> Ptr TSTreeRaw -> CString -> CUInt -> IO (Ptr TSTreeRaw)


-- --------------------------------------------------------------------
-- Tree functions
-- --------------------------------------------------------------------

foreign import ccall unsafe "&ts_tree_delete"
  ts_tree_finalizer :: FinalizerPtr TSTreeRaw

-- via C shim (TSNode returned by value in C, passed via out-pointer)
foreign import ccall unsafe "ts_tree_root_node_p"
  ts_tree_root_node :: Ptr TSTreeRaw -> Ptr TSNode -> IO ()


-- --------------------------------------------------------------------
-- Node functions (all via C shim — TSNode passed by pointer)
-- --------------------------------------------------------------------

foreign import ccall unsafe "ts_node_has_error_p"
  ts_node_has_error :: Ptr TSNode -> IO CBool

foreign import ccall unsafe "ts_node_type_p"
  ts_node_type :: Ptr TSNode -> IO CString

foreign import ccall unsafe "ts_node_start_point_p"
  ts_node_start_point :: Ptr TSNode -> Ptr TSPoint -> IO ()

foreign import ccall unsafe "ts_node_child_count_p"
  ts_node_child_count :: Ptr TSNode -> IO CUInt

foreign import ccall unsafe "ts_node_child_p"
  ts_node_child :: Ptr TSNode -> CUInt -> Ptr TSNode -> IO ()

foreign import ccall unsafe "ts_node_is_null_p"
  ts_node_is_null :: Ptr TSNode -> IO CBool

foreign import ccall unsafe "ts_node_is_named_p"
  ts_node_is_named :: Ptr TSNode -> IO CBool


-- --------------------------------------------------------------------
-- Query functions
-- --------------------------------------------------------------------

foreign import ccall unsafe "ts_language_version"
  ts_language_version :: Ptr TSLanguageRaw -> IO CUInt

foreign import ccall unsafe "ts_query_new"
  ts_query_new
    :: Ptr TSLanguageRaw
    -> CString
    -> CUInt
    -> Ptr CUInt     -- error offset
    -> Ptr CInt      -- error type
    -> IO (Ptr TSQueryRaw)

foreign import ccall unsafe "&ts_query_delete"
  ts_query_finalizer :: FinalizerPtr TSQueryRaw

foreign import ccall unsafe "ts_query_capture_name_for_id"
  ts_query_capture_name_for_id
    :: Ptr TSQueryRaw
    -> CUInt
    -> Ptr CUInt
    -> IO CString

foreign import ccall unsafe "ts_query_capture_count"
  ts_query_capture_count :: Ptr TSQueryRaw -> IO CUInt

foreign import ccall unsafe "ts_query_cursor_new"
  ts_query_cursor_new :: IO (Ptr TSQueryCursorRaw)

foreign import ccall unsafe "&ts_query_cursor_delete"
  ts_query_cursor_finalizer :: FinalizerPtr TSQueryCursorRaw

foreign import ccall unsafe "ts_query_cursor_exec_p"
  ts_query_cursor_exec :: Ptr TSQueryCursorRaw -> Ptr TSQueryRaw -> Ptr TSNode -> IO ()

foreign import ccall unsafe "ts_query_cursor_next_capture_p"
  ts_query_cursor_next_capture
    :: Ptr TSQueryCursorRaw
    -> Ptr TSNode
    -> Ptr CUInt
    -> IO CBool
