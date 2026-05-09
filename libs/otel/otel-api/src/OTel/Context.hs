-- | Immutable, heterogeneous context for propagating values across API
-- boundaries within a process.
--
-- 'Context' stores typed values indexed by 'ContextKey'. The primary API is
-- explicit context passing via 'root', 'getValue', and 'setValue'. The global
-- attach\/detach mechanism ('attach', 'detach', 'getCurrent') is provided for
-- spec compliance but is not thread-safe; prefer explicit passing in
-- concurrent code.
module OTel.Context
  ( -- * Context
    Context
  , root
  , getValue
  , setValue
    -- * Global context (spec compliance)
  , Token
  , getCurrent
  , attach
  , detach
  ) where

import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Unique (Unique)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Context.Key (ContextKey, keyId)


-- | An immutable collection of typed key-value pairs.
--
-- 'setValue' returns a new 'Context'; the original is unchanged.
newtype Context = Context (Map Unique Dynamic)


-- | The empty context.
root :: Context
root = Context Map.empty


-- | Look up a value by its typed key.
--
-- Returns 'Nothing' if the key is not present or if the stored type does not
-- match (which should not happen if the API is used correctly).
getValue :: Typeable a => ContextKey a -> Context -> Maybe a
getValue key (Context m) = Map.lookup (keyId key) m >>= fromDynamic


-- | Insert or overwrite a value for the given key, returning a new 'Context'.
setValue :: Typeable a => ContextKey a -> a -> Context -> Context
setValue key val (Context m) = Context (Map.insert (keyId key) (toDyn val) m)


-- --------------------------------------------------------------------------
-- Global context (spec compliance)
-- --------------------------------------------------------------------------

-- | Global context stack. The stack is never empty; 'root' is always at the
-- bottom.
--
-- This is intentionally a simple 'IORef' — it is /not/ thread-safe. The
-- idiomatic Haskell path is explicit 'Context' passing. This global mechanism
-- exists solely for OTel spec compliance in single-threaded attach\/detach
-- scenarios.
currentContextRef :: IORef [Context]
currentContextRef = unsafePerformIO (newIORef [root])
{-# NOINLINE currentContextRef #-}


-- | An opaque token returned by 'attach', used to restore the previous
-- context via 'detach'.
newtype Token = Token { _tokenDepth :: Int }


-- | Get the current active context from the global stack.
getCurrent :: IO Context
getCurrent = do
  stack <- readIORef currentContextRef
  case stack of
    [] -> pure root -- should not happen; defensive
    (ctx : _) -> pure ctx


-- | Push a context onto the global stack, returning a 'Token' that can
-- restore the previous state via 'detach'.
attach :: Context -> IO Token
attach ctx = do
  stack <- readIORef currentContextRef
  let depth = length stack
  writeIORef currentContextRef (ctx : stack)
  pure (Token depth)


-- | Restore the context that was active before the corresponding 'attach'.
--
-- Per the OTel spec, mismatched detach calls are silently tolerated.
detach :: Token -> IO ()
detach (Token _depth) = do
  stack <- readIORef currentContextRef
  case stack of
    [] -> pure () -- should not happen
    [_] -> pure () -- never pop root
    (_ : rest) -> writeIORef currentContextRef rest
