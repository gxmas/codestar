-- | Attribute types for annotating spans, metrics, logs, and resources.
module OTel.Attribute
  ( -- * Attribute values
    AttributeValue (..)

    -- * Keys and attributes
  , Key
  , Attribute

    -- * Attribute collections
  , Attributes (..)
  , emptyAttributes
  , fromList
  , toList
  , insert
  , lookup
  , size

    -- * Instrumentation scope
  , InstrumentationScope (..)
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude hiding (lookup)


-- | The value half of an attribute key-value pair. See the OTel attribute spec.
data AttributeValue
  = StringValue Text
  | BoolValue Bool
  | Int64Value Int64
  | Float64Value Double
  | StringArrayValue (Vector Text)
  | BoolArrayValue (Vector Bool)
  | Int64ArrayValue (Vector Int64)
  | Float64ArrayValue (Vector Double)
  deriving stock (Eq, Show)


-- | An attribute key.
type Key = Text


-- | A key-value pair.
type Attribute = (Key, AttributeValue)


-- | A map of attribute keys to values.
newtype Attributes = Attributes {unAttributes :: Map Key AttributeValue}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)


-- | An empty attribute set.
emptyAttributes :: Attributes
emptyAttributes = mempty


-- | Construct 'Attributes' from a list of key-value pairs.
fromList :: [Attribute] -> Attributes
fromList = Attributes . Map.fromList


-- | Convert 'Attributes' to a list of key-value pairs.
toList :: Attributes -> [Attribute]
toList = Map.toList . unAttributes


-- | Insert a key-value pair into 'Attributes'.
insert :: Key -> AttributeValue -> Attributes -> Attributes
insert k v (Attributes m) = Attributes (Map.insert k v m)


-- | Look up a value by key.
lookup :: Key -> Attributes -> Maybe AttributeValue
lookup k (Attributes m) = Map.lookup k m


-- | Return the number of attributes.
size :: Attributes -> Int
size (Attributes m) = Map.size m


-- | Identifies the library or module producing telemetry.
data InstrumentationScope = InstrumentationScope
  { scopeName :: Text
  , scopeVersion :: Maybe Text
  , scopeSchemaUrl :: Maybe Text
  , scopeAttributes :: Maybe Attributes
  }
  deriving stock (Eq, Show)
