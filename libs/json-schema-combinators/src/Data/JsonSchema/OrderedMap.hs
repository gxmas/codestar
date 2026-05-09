{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Data.JsonSchema.OrderedMap
-- Description : Ordered associative container preserving insertion order
-- Copyright   : (c) 2026 Geoffrey Noel
-- License     : MIT
--
-- An associative container that preserves insertion order for
-- serialization while providing O(log n) lookup via an underlying 'Map'.
-- This is used throughout the library for JSON Schema fields where
-- property order matters for human readability (e.g., @properties@,
-- @patternProperties@, @$defs@).
--
-- == Key behaviours
--
-- * __Equality__ is order-insensitive: two @OrderedMap@s are equal iff
--   they contain the same key-value pairs, regardless of insertion order.
--   This delegates to 'Map' equality.
--
-- * __Duplicate keys in 'fromList'__: the /last/ value wins, but the
--   /first/ occurrence determines the position in the ordering.
--
-- * __'Semigroup' ('Data.Semigroup.<>')__ is left-biased: keys from
--   the left operand appear first, followed by new keys from the right.
--   For duplicate keys, the left value wins.
module Data.JsonSchema.OrderedMap
  ( OrderedMap
  , empty
  , singleton
  , fromList
  , toList
  , lookup
  , keys
  , elems
  , size
  , null
  , member
  , insert
  , union
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (lookup, null)

-- | An ordered associative container. Internally represented as a 'Map'
-- for efficient lookup paired with a key list for insertion order.
--
-- Invariant: the key list contains exactly the keys present in the map,
-- in insertion order, with no duplicates.
data OrderedMap k v = OrderedMap !(Map k v) ![k]
  deriving (Show)

-- | Equality is order-insensitive: two 'OrderedMap's are equal iff they
-- contain the same key-value pairs, regardless of insertion order.
instance (Eq k, Eq v) => Eq (OrderedMap k v) where
  OrderedMap m1 _ == OrderedMap m2 _ = m1 == m2

-- | Ord delegates to the underlying Map (order-insensitive).
instance (Ord k, Ord v) => Ord (OrderedMap k v) where
  compare (OrderedMap m1 _) (OrderedMap m2 _) = compare m1 m2

-- | Left-biased union. Keys from the left operand appear first in order,
-- followed by new keys from the right operand.
instance Ord k => Semigroup (OrderedMap k v) where
  OrderedMap m1 ks1 <> OrderedMap m2 ks2 =
    OrderedMap
      (Map.union m1 m2)
      (ks1 ++ filter (\k -> not (Map.member k m1)) ks2)

instance Ord k => Monoid (OrderedMap k v) where
  mempty = empty

-- | The empty ordered map.
empty :: OrderedMap k v
empty = OrderedMap Map.empty []

-- | A map with a single key-value pair.
singleton :: k -> v -> OrderedMap k v
singleton k v = OrderedMap (Map.singleton k v) [k]

-- | Build an ordered map from a list of key-value pairs.
--
-- Duplicate keys: the /last/ value wins, but the /first/ occurrence
-- determines the position in the ordering.
fromList :: Ord k => [(k, v)] -> OrderedMap k v
fromList kvs =
  let -- Build map from reversed list so last value wins via fromList
      m = Map.fromList kvs
      -- Preserve first-occurrence order, deduplicated
      ks = dedup kvs
  in OrderedMap m ks
  where
    dedup = go Map.empty
    go _ [] = []
    go seen ((k, _) : rest)
      | Map.member k seen = go seen rest
      | otherwise = k : go (Map.insert k () seen) rest

-- | Convert to a list of key-value pairs in insertion order.
toList :: Ord k => OrderedMap k v -> [(k, v)]
toList (OrderedMap m ks) =
  [ (k, v)
  | k <- ks
  , Just v <- [Map.lookup k m]
  ]

-- | Look up a value by key. O(log n).
lookup :: Ord k => k -> OrderedMap k v -> Maybe v
lookup k (OrderedMap m _) = Map.lookup k m

-- | The keys in insertion order.
keys :: OrderedMap k v -> [k]
keys (OrderedMap _ ks) = ks

-- | The values in insertion order.
elems :: Ord k => OrderedMap k v -> [v]
elems om = map snd (toList om)

-- | The number of entries.
size :: OrderedMap k v -> Int
size (OrderedMap m _) = Map.size m

-- | Is the map empty?
null :: OrderedMap k v -> Bool
null (OrderedMap m _) = Map.null m

-- | Is the key present?
member :: Ord k => k -> OrderedMap k v -> Bool
member k (OrderedMap m _) = Map.member k m

-- | Insert a key-value pair. If the key already exists, the value is
-- replaced and the position is preserved. If the key is new, it is
-- appended at the end.
insert :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
insert k v (OrderedMap m ks)
  | Map.member k m = OrderedMap (Map.insert k v m) ks
  | otherwise = OrderedMap (Map.insert k v m) (ks ++ [k])

-- | Left-biased union. Keys from the first map appear before new keys
-- from the second map in the resulting order.
union :: Ord k => OrderedMap k v -> OrderedMap k v -> OrderedMap k v
union = (<>)
