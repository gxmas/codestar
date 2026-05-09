-- | IdGenerator type class for generating trace and span identifiers.
module OTel.SDK.Trace.IdGenerator
  ( -- * IdGenerator
    IdGenerator (..)
  , SomeIdGenerator (..)
  ) where

import OTel.Trace.SpanContext (SpanId, TraceId)


-------------------------------------------------------------------------------
-- IdGenerator type class
-------------------------------------------------------------------------------

-- | The interface for generating unique trace and span identifiers.
-- The default implementation uses cryptographically secure random bytes,
-- but custom implementations can be substituted (e.g., for deterministic
-- testing).
class IdGenerator g where
  -- | Generate a new globally-unique 128-bit trace identifier.
  generateTraceId :: g -> IO TraceId

  -- | Generate a new locally-unique 64-bit span identifier.
  generateSpanId :: g -> IO SpanId


-- | Existential wrapper for any 'IdGenerator' implementation.
data SomeIdGenerator = forall g. IdGenerator g => SomeIdGenerator g

instance IdGenerator SomeIdGenerator where
  generateTraceId (SomeIdGenerator g) = generateTraceId g
  generateSpanId (SomeIdGenerator g) = generateSpanId g
