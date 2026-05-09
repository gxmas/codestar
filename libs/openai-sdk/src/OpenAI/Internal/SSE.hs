module OpenAI.Internal.SSE
  ( parseSSELine
  , SSEEvent (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as TE


data SSEEvent
  = SSEData !Text
  | SSEDone
  | SSEOther
  deriving stock (Eq, Show)

parseSSELine :: ByteString -> SSEEvent
parseSSELine line
  | line == "data: [DONE]" = SSEDone
  | BC.isPrefixOf "data: " line =
      SSEData (TE.decodeUtf8Lenient (BC.drop 6 line))
  | otherwise = SSEOther
