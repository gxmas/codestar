-- | Immutable, heterogeneous context for propagating values across API
-- boundaries within a process.
--
-- 'Context' stores typed values indexed by 'ContextKey'. The primary API is
-- explicit context passing via 'root', 'getValue', and 'setValue'. The global
-- attach\/detach mechanism ('attach', 'detach', 'getCurrent') is provided for
-- spec compliance and maintains a per-thread context stack, so it is safe to
-- use from concurrent threads.
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

import Control.Concurrent (ThreadId, myThreadId)
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
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

-- | Per-thread context stacks keyed by 'ThreadId'. Each thread's stack is
-- never empty while attached; 'root' is the implicit bottom. Threads that
-- have no entry in the map are treated as having only 'root'.
--
-- Thread safety: each thread only mutates its own entry; 'atomicModifyIORef''
-- ensures the read-modify-write on the shared 'Map' is atomic.
currentContextRef :: IORef (Map ThreadId [Context])
currentContextRef = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE currentContextRef #-}


-- | An opaque token returned by 'attach', used to restore the previous
-- context via 'detach'.
newtype Token = Token { _tokenDepth :: Int }


-- | Get the current active context from this thread's stack.
getCurrent :: IO Context
getCurrent = do
  tid <- myThreadId
  m <- readIORef currentContextRef
  case Map.lookup tid m of
    Nothing -> pure root
    Just [] -> pure root -- should not happen; defensive
    Just (ctx : _) -> pure ctx


-- | Push a context onto this thread's stack, returning a 'Token' that can
-- restore the previous state via 'detach'.
attach :: Context -> IO Token
attach ctx = do
  tid <- myThreadId
  atomicModifyIORef' currentContextRef $ \m ->
    let stack = Map.findWithDefault [] tid m
        depth = length stack
    in (Map.insert tid (ctx : stack) m, Token depth)


-- | Restore the context that was active before the corresponding 'attach'.
--
-- Per the OTel spec, mismatched detach calls are silently tolerated.
-- When the last context is popped, the thread's entry is removed from
-- the map to avoid leaking memory for short-lived threads.
detach :: Token -> IO ()
detach (Token _depth) = do
  tid <- myThreadId
  atomicModifyIORef' currentContextRef $ \m ->
    case Map.lookup tid m of
      Nothing -> (m, ())
      Just [] -> (Map.delete tid m, ())  -- should not happen
      Just [_] -> (Map.delete tid m, ()) -- last entry; clean up
      Just (_ : rest) -> (Map.insert tid rest m, ())
