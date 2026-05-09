-- | Typed, identity-based keys for 'Context' values.
--
-- Keys use identity semantics: two keys with the same name are distinct
-- if they were created by different 'newContextKey' calls. The name is
-- for debugging and display only.
module OTel.Context.Key
  ( ContextKey
  , newContextKey
  , keyName
  , keyId
  ) where

import Data.Text (Text)
import Data.Unique (Unique, newUnique)


-- | A typed, identity-based key for 'Context' values.
--
-- Two keys are equal only if they are the same value (created by the same
-- 'newContextKey' call). The phantom type parameter @a@ tracks the type of
-- value associated with this key.
data ContextKey a = ContextKey
  { _keyId :: !Unique
  , _keyName :: !Text
  }

instance Eq (ContextKey a) where
  k1 == k2 = _keyId k1 == _keyId k2

instance Ord (ContextKey a) where
  compare k1 k2 = compare (_keyId k1) (_keyId k2)

instance Show (ContextKey a) where
  show k = "ContextKey " <> show (_keyName k)


-- | Create a new unique context key with the given debugging name.
--
-- Each call produces a globally unique key, even if the same name is reused.
newContextKey :: Text -> IO (ContextKey a)
newContextKey name = do
  u <- newUnique
  pure (ContextKey u name)


-- | Get the unique identity of a key. Used internally by 'Context'.
keyId :: ContextKey a -> Unique
keyId = _keyId


-- | Get the debugging name of a key.
keyName :: ContextKey a -> Text
keyName = _keyName
