-- |
-- Module: OTel.Trace.TraceState
-- Description: W3C Trace Context tracestate header representation.
--
-- 'TraceState' carries vendor-specific trace identification data as an ordered
-- list of key-value pairs. Per the W3C Trace Context specification, the most
-- recently modified entry appears first.
module OTel.Trace.TraceState
  ( TraceState
  , empty
  , get
  , set
  , delete
  , toList
  ) where

import Data.Text (Text)


-- | An ordered list of vendor-specific key-value pairs, per W3C Trace Context.
--
-- The ordering is significant: the most recently modified entry comes first.
-- All operations return a new 'TraceState'; the type is immutable.
newtype TraceState = TraceState [(Text, Text)]
  deriving stock (Eq, Show)


-- | An empty 'TraceState' with no entries.
empty :: TraceState
empty = TraceState []


-- | Look up a value by vendor key. O(n).
get :: Text -> TraceState -> Maybe Text
get key (TraceState entries) = Prelude.lookup key entries


-- | Set a key-value pair. If the key already exists, update it and move it to
-- the front. If not, prepend it. Returns a new 'TraceState'.
set :: Text -> Text -> TraceState -> TraceState
set key val (TraceState entries) =
  TraceState ((key, val) : filter (\(k, _) -> k /= key) entries)


-- | Remove a key. Returns a new 'TraceState'.
delete :: Text -> TraceState -> TraceState
delete key (TraceState entries) =
  TraceState (filter (\(k, _) -> k /= key) entries)


-- | Get all entries as an ordered list.
toList :: TraceState -> [(Text, Text)]
toList (TraceState entries) = entries
