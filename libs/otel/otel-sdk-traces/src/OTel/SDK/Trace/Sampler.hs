-- | Sampler type class, supporting types, and built-in sampler implementations.
module OTel.SDK.Trace.Sampler
  ( -- * Sampler
    Sampler (..)
  , SomeSampler (..)

    -- * Sampling result
  , SamplingResult (..)
  , SamplingDecision (..)

    -- * Built-in samplers
  , AlwaysOnSampler (..)
  , AlwaysOffSampler (..)
  , TraceIdRatioBasedSampler (..)
  , ParentBasedSampler (..)
  , defaultParentBasedSampler
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)

import OTel.Attribute (Attributes, emptyAttributes)
import OTel.Context (Context)
import OTel.SDK.Trace.Export (Link)
import OTel.Trace (Span(..), SpanKind, getSpanFromContext)
import OTel.Trace.SpanContext (SpanContext(..), TraceId, isSampled, isValid, traceIdToBytes)
import OTel.Trace.TraceState (TraceState)
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- Sampling types
-------------------------------------------------------------------------------

-- | The decision made by a 'Sampler' about whether to record and/or sample
-- a span.
data SamplingDecision = Drop | RecordOnly | RecordAndSample
  deriving stock (Eq, Show, Enum, Bounded)


-- | The result returned by a 'Sampler', carrying the decision plus any
-- additional attributes and trace state to attach to the span.
data SamplingResult = SamplingResult
  { samplingDecision :: !SamplingDecision
  , samplingAttributes :: !Attributes
  , samplingTraceState :: !TraceState
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Sampler type class
-------------------------------------------------------------------------------

-- | The interface for making span sampling decisions. Implementations
-- receive full context and span metadata to make their decision.
class Sampler s where
  -- | Decide whether to sample a span given its context, trace ID, name,
  -- kind, initial attributes, and links.
  shouldSample
    :: s
    -> Context
    -> TraceId
    -> Text
    -> SpanKind
    -> Attributes
    -> [Link]
    -> IO SamplingResult

  -- | A human-readable description of this sampler, useful for debugging
  -- and logging configuration.
  samplerDescription :: s -> Text


-- | Existential wrapper for any 'Sampler' implementation.
data SomeSampler = forall s. Sampler s => SomeSampler s

instance Sampler SomeSampler where
  shouldSample (SomeSampler s) = shouldSample s
  samplerDescription (SomeSampler s) = samplerDescription s


-------------------------------------------------------------------------------
-- AlwaysOnSampler
-------------------------------------------------------------------------------

-- | A sampler that always records and samples every span.
data AlwaysOnSampler = AlwaysOnSampler

instance Sampler AlwaysOnSampler where
  shouldSample _ _ _ _ _ _ _ = pure SamplingResult
    { samplingDecision = RecordAndSample
    , samplingAttributes = emptyAttributes
    , samplingTraceState = TraceState.empty
    }
  samplerDescription _ = "AlwaysOnSampler"


-------------------------------------------------------------------------------
-- AlwaysOffSampler
-------------------------------------------------------------------------------

-- | A sampler that drops every span (never records or samples).
data AlwaysOffSampler = AlwaysOffSampler

instance Sampler AlwaysOffSampler where
  shouldSample _ _ _ _ _ _ _ = pure SamplingResult
    { samplingDecision = Drop
    , samplingAttributes = emptyAttributes
    , samplingTraceState = TraceState.empty
    }
  samplerDescription _ = "AlwaysOffSampler"


-------------------------------------------------------------------------------
-- TraceIdRatioBasedSampler
-------------------------------------------------------------------------------

-- | A sampler that makes a deterministic decision based on the trace ID.
-- The ratio must be in [0.0, 1.0]. A ratio of 0.0 means no traces are sampled;
-- 1.0 means all traces are sampled. The decision is consistent for a given
-- trace ID — repeated calls with the same ID yield the same result.
data TraceIdRatioBasedSampler = TraceIdRatioBasedSampler !Double

instance Sampler TraceIdRatioBasedSampler where
  shouldSample (TraceIdRatioBasedSampler ratio) _ traceId_ _ _ _ _ = do
    let decision
          | ratio <= 0.0 = Drop
          | ratio >= 1.0 = RecordAndSample
          | otherwise =
              let bound :: Word64
                  bound = floor (ratio * (fromIntegral (maxBound :: Word64) :: Double))
                  idBytes = BS.unpack (traceIdToBytes traceId_)
                  idVal :: Word64
                  idVal = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (drop 8 idBytes)
              in if idVal < bound then RecordAndSample else Drop
    pure SamplingResult
      { samplingDecision = decision
      , samplingAttributes = emptyAttributes
      , samplingTraceState = TraceState.empty
      }
  samplerDescription (TraceIdRatioBasedSampler r) =
    "TraceIdRatioBased{" <> Text.pack (show r) <> "}"


-------------------------------------------------------------------------------
-- ParentBasedSampler
-------------------------------------------------------------------------------

-- | A composite sampler that delegates to different samplers based on whether
-- the parent span is remote/local and sampled/unsampled. Follows the
-- OpenTelemetry specification with 5 delegate slots: root (no parent),
-- remote-sampled, remote-not-sampled, local-sampled, local-not-sampled.
data ParentBasedSampler = ParentBasedSampler
  { parentBasedRoot                   :: !SomeSampler
  , parentBasedRemoteParentSampled    :: !SomeSampler
  , parentBasedRemoteParentNotSampled :: !SomeSampler
  , parentBasedLocalParentSampled     :: !SomeSampler
  , parentBasedLocalParentNotSampled  :: !SomeSampler
  }


-- | Construct a 'ParentBasedSampler' with the given root sampler and
-- spec-default delegates: 'AlwaysOnSampler' for sampled parents,
-- 'AlwaysOffSampler' for unsampled parents.
defaultParentBasedSampler :: SomeSampler -> ParentBasedSampler
defaultParentBasedSampler root_ = ParentBasedSampler
  { parentBasedRoot                   = root_
  , parentBasedRemoteParentSampled    = SomeSampler AlwaysOnSampler
  , parentBasedRemoteParentNotSampled = SomeSampler AlwaysOffSampler
  , parentBasedLocalParentSampled     = SomeSampler AlwaysOnSampler
  , parentBasedLocalParentNotSampled  = SomeSampler AlwaysOffSampler
  }


instance Sampler ParentBasedSampler where
  shouldSample sampler ctx traceId_ name kind attrs links = do
    mParentSc <- case getSpanFromContext ctx of
      Nothing -> pure Nothing
      Just someSpan -> do
        sc <- getSpanContext someSpan
        pure (if isValid sc then Just sc else Nothing)
    let delegate = case mParentSc of
          Nothing -> parentBasedRoot sampler
          Just parentSc ->
            case (parentSc._isRemote, isSampled parentSc.traceFlags) of
              (True,  True)  -> parentBasedRemoteParentSampled sampler
              (True,  False) -> parentBasedRemoteParentNotSampled sampler
              (False, True)  -> parentBasedLocalParentSampled sampler
              (False, False) -> parentBasedLocalParentNotSampled sampler
    shouldSample delegate ctx traceId_ name kind attrs links

  samplerDescription sampler =
    "ParentBased{root=" <> samplerDescription (parentBasedRoot sampler) <> "}"
