-- | Baggage: a set of key-value pairs propagated across process boundaries.
--
-- Each entry carries a value and optional metadata. Baggage is stored in
-- 'Context' via a typed 'ContextKey'.
module OTel.Baggage
  ( BaggageEntry (..)
  , Baggage
  , emptyBaggage
  , getBaggage
  , setBaggage
  , baggageToList
  , baggageFromList
  , getValue
  , getEntry
  , getAllValues
  , setValue
  , removeValue
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Context (Context)
import OTel.Context qualified as Context
import OTel.Context.Key (ContextKey, newContextKey)


-- | A single baggage entry with a value and optional metadata.
data BaggageEntry = BaggageEntry
  { entryValue :: !Text
  , entryMetadata :: !(Maybe Text)
  } deriving stock (Eq, Show)


-- | An immutable set of key-value pairs propagated across process boundaries.
newtype Baggage = Baggage (Map Text BaggageEntry)
  deriving stock (Eq, Show)


-- | An empty baggage with no entries.
emptyBaggage :: Baggage
emptyBaggage = Baggage Map.empty


-- | Convert baggage to a list of key-entry pairs.
baggageToList :: Baggage -> [(Text, BaggageEntry)]
baggageToList (Baggage m) = Map.toList m


-- | Construct baggage from a list of key-entry pairs.
baggageFromList :: [(Text, BaggageEntry)] -> Baggage
baggageFromList = Baggage . Map.fromList


-- | Look up the value for a key in the baggage.
getValue :: Text -> Baggage -> Maybe Text
getValue name (Baggage m) = fmap entryValue (Map.lookup name m)


-- | Look up the full entry for a key in the baggage.
getEntry :: Text -> Baggage -> Maybe BaggageEntry
getEntry name (Baggage m) = Map.lookup name m


-- | Get all entries as a 'Map'.
getAllValues :: Baggage -> Map Text BaggageEntry
getAllValues (Baggage m) = m


-- | Set a key-value pair in the baggage, replacing any existing entry.
setValue :: Text -> Text -> Maybe Text -> Baggage -> Baggage
setValue name value meta (Baggage m) =
  Baggage (Map.insert name (BaggageEntry value meta) m)


-- | Remove a key from the baggage.
removeValue :: Text -> Baggage -> Baggage
removeValue name (Baggage m) = Baggage (Map.delete name m)


-------------------------------------------------------------------------------
-- Context integration
-------------------------------------------------------------------------------

baggageContextKey :: ContextKey Baggage
baggageContextKey = unsafePerformIO (newContextKey "otel-baggage")
{-# NOINLINE baggageContextKey #-}


-- | Retrieve the baggage from a 'Context'.
getBaggage :: Context -> Baggage
getBaggage ctx = case Context.getValue baggageContextKey ctx of
  Nothing -> emptyBaggage
  Just b -> b


-- | Store baggage in a 'Context'.
setBaggage :: Baggage -> Context -> Context
setBaggage b = Context.setValue baggageContextKey b
