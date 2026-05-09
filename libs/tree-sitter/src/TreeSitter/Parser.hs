module TreeSitter.Parser
  ( Parser
  , newParser
  , setLanguage
  , parseSource
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Foreign (ForeignPtr, newForeignPtr, nullPtr, withForeignPtr)
import Foreign.C ()

import TreeSitter.FFI
import TreeSitter.Language (Language (..))


newtype Parser = Parser (ForeignPtr TSParserRaw)

newParser :: IO Parser
newParser = do
  ptr <- ts_parser_new
  Parser <$> newForeignPtr ts_parser_finalizer ptr

setLanguage :: Parser -> Language -> IO Bool
setLanguage (Parser fp) (Language langPtr) =
  withForeignPtr fp $ \p -> do
    result <- ts_parser_set_language p langPtr
    pure (result /= 0)

parseSource :: Parser -> ByteString -> IO (Maybe (ForeignPtr TSTreeRaw))
parseSource (Parser fp) src =
  withForeignPtr fp $ \p ->
    BS.unsafeUseAsCStringLen src $ \(cstr, len) -> do
      treePtr <- ts_parser_parse_string p nullPtr cstr (fromIntegral len)
      if treePtr == nullPtr
        then pure Nothing
        else Just <$> newForeignPtr ts_tree_finalizer treePtr
