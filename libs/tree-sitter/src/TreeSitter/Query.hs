module TreeSitter.Query
  ( Query
  , QueryError (..)
  , QueryCapture (..)
  , newQuery
  , queryCaptures
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign

import TreeSitter.FFI
import TreeSitter.Language (Language (..))
import TreeSitter.Node (Node (..))


data Query = Query
  { qRaw      :: !(ForeignPtr TSQueryRaw)
  , qCaptures :: ![Text]
  }

data QueryError = QueryError
  { qeOffset :: !Word32
  , qeType   :: !Int
  } deriving stock (Eq, Show)

data QueryCapture = QueryCapture
  { captureName :: !Text
  , captureNode :: !Node
  }

newQuery :: Language -> ByteString -> IO (Either QueryError Query)
newQuery (Language langPtr) source =
  BS.useAsCStringLen source $ \(cstr, len) ->
    alloca $ \errOffsetPtr ->
      alloca $ \errTypePtr -> do
        raw <- ts_query_new langPtr cstr (fromIntegral len) errOffsetPtr errTypePtr
        if raw == nullPtr
          then do
            offset <- fromIntegral <$> peek errOffsetPtr
            errTyp <- fromIntegral <$> peek errTypePtr
            pure (Left (QueryError offset errTyp))
          else do
            fp <- newForeignPtr ts_query_finalizer raw
            names <- withForeignPtr fp loadCaptureNames
            pure (Right (Query fp names))

queryCaptures :: Query -> Node -> IO [QueryCapture]
queryCaptures query _root@(Node nodeData keepAlive) = do
  cursorRaw <- ts_query_cursor_new
  cursorFp <- newForeignPtr ts_query_cursor_finalizer cursorRaw
  withForeignPtr cursorFp $ \cursorPtr ->
    withForeignPtr (qRaw query) $ \queryPtr ->
      alloca $ \nodePtr -> do
        poke nodePtr nodeData
        ts_query_cursor_exec cursorPtr queryPtr nodePtr
        go cursorPtr
  where
    go cursorPtr =
      alloca $ \nodePtr ->
        alloca $ \capIdxPtr -> do
          ok <- ts_query_cursor_next_capture cursorPtr nodePtr capIdxPtr
          if ok == 0
            then pure []
            else do
              node <- peek nodePtr
              capIdx <- fromIntegral <$> peek capIdxPtr
              let capName = captureNameAt (qCaptures query) capIdx
                  capNode = Node node keepAlive
              rest <- go cursorPtr
              pure (QueryCapture capName capNode : rest)

captureNameAt :: [Text] -> Int -> Text
captureNameAt names idx
  | idx < 0 || idx >= length names = Text.pack ("capture_" <> show idx)
  | otherwise = names !! idx

loadCaptureNames :: Ptr TSQueryRaw -> IO [Text]
loadCaptureNames queryPtr = do
  count <- (fromIntegral <$> ts_query_capture_count queryPtr) :: IO Int
  mapM (loadOne queryPtr) [0 .. count - 1]
  where
    loadOne ptr i =
      alloca $ \lenPtr -> do
        cstr <- ts_query_capture_name_for_id ptr (fromIntegral i) lenPtr
        len <- fromIntegral <$> peek lenPtr
        bytes <- BS.packCStringLen (cstr, len)
        pure (Text.decodeUtf8 bytes)
