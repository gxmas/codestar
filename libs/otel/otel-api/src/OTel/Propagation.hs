-- | Context propagation API: TextMapPropagator type class, getter/setter
-- abstractions, composite propagation, and global registration.
module OTel.Propagation
  ( -- * Getter / Setter type classes
    TextMapGetter (..)
  , TextMapSetter (..)
    -- * TextMapPropagator
  , TextMapPropagator (..)
  , SomeTextMapPropagator (..)
  , NoOpPropagator (..)
    -- * CompositePropagator
  , CompositePropagator (..)
    -- * Global registration
  , setGlobalTextMapPropagator
  , getGlobalTextMapPropagator
  ) where

import Control.Monad (foldM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Context (Context)


-------------------------------------------------------------------------------
-- Getter / Setter
-------------------------------------------------------------------------------

-- | Extract header values from a carrier (e.g. HTTP request headers).
class TextMapGetter carrier where
  tmGet :: carrier -> Text -> Maybe Text
  tmKeys :: carrier -> [Text]


-- | Inject header values into a carrier (e.g. HTTP response headers).
class TextMapSetter carrier where
  tmSet :: carrier -> Text -> Text -> carrier


instance TextMapGetter (Map Text Text) where
  tmGet m k = Map.lookup k m
  tmKeys m = Map.keys m


instance TextMapSetter (Map Text Text) where
  tmSet m k v = Map.insert k v m


-------------------------------------------------------------------------------
-- TextMapPropagator
-------------------------------------------------------------------------------

-- | A propagator that injects and extracts context via text-based carriers.
class TextMapPropagator p where
  inject :: TextMapSetter carrier => p -> Context -> carrier -> IO carrier
  extract :: TextMapGetter carrier => p -> Context -> carrier -> IO Context
  fields :: p -> [Text]


-------------------------------------------------------------------------------
-- NoOpPropagator
-------------------------------------------------------------------------------

-- | A propagator that does nothing.
data NoOpPropagator = NoOpPropagator

instance TextMapPropagator NoOpPropagator where
  inject _ _ carrier = pure carrier
  extract _ ctx _ = pure ctx
  fields _ = []


-------------------------------------------------------------------------------
-- Existential wrapper
-------------------------------------------------------------------------------

-- | Existential wrapper for any 'TextMapPropagator'.
data SomeTextMapPropagator = forall p. TextMapPropagator p => SomeTextMapPropagator p

instance TextMapPropagator SomeTextMapPropagator where
  inject (SomeTextMapPropagator p) ctx carrier = inject p ctx carrier
  extract (SomeTextMapPropagator p) ctx carrier = extract p ctx carrier
  fields (SomeTextMapPropagator p) = fields p


-------------------------------------------------------------------------------
-- CompositePropagator
-------------------------------------------------------------------------------

-- | A propagator that delegates to multiple propagators in order.
newtype CompositePropagator = CompositePropagator [SomeTextMapPropagator]

instance TextMapPropagator CompositePropagator where
  inject (CompositePropagator ps) ctx carrier0 =
    foldM (\c p -> inject p ctx c) carrier0 ps
  extract (CompositePropagator ps) ctx0 carrier =
    foldM (\ctx p -> extract p ctx carrier) ctx0 ps
  fields (CompositePropagator ps) =
    concatMap fields ps


-------------------------------------------------------------------------------
-- Global registration
-------------------------------------------------------------------------------

globalPropagatorRef :: IORef SomeTextMapPropagator
globalPropagatorRef = unsafePerformIO (newIORef (SomeTextMapPropagator NoOpPropagator))
{-# NOINLINE globalPropagatorRef #-}


-- | Set the global text map propagator.
setGlobalTextMapPropagator :: SomeTextMapPropagator -> IO ()
setGlobalTextMapPropagator = writeIORef globalPropagatorRef


-- | Get the global text map propagator.
getGlobalTextMapPropagator :: IO SomeTextMapPropagator
getGlobalTextMapPropagator = readIORef globalPropagatorRef
