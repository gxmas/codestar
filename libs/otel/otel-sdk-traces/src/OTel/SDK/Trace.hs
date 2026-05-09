-- | SDK TracerProvider implementation: SdkTracerProvider, SdkTracer, SdkSpan,
-- along with configuration, AlwaysOnSampler, RandomIdGenerator, and SpanLimits.
module OTel.SDK.Trace
  ( -- * Span limits
    SpanLimits (..)
  , defaultSpanLimits

    -- * SdkTracerProvider
  , SdkTracerProvider
  , SdkTracerProviderConfig (..)
  , defaultSdkTracerProviderConfig
  , newSdkTracerProvider
  , addSpanProcessor
  , shutdown
  , forceFlush

    -- * Built-in sampler
  , AlwaysOnSampler (..)

    -- * Built-in ID generator
  , RandomIdGenerator (..)
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, swapTVar
  , writeTVar
  )
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Typeable (typeOf)
import Data.Word (Word8)
import Prelude hiding (lookup)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomIO)

import OTel.Attribute
  ( Attributes, AttributeValue(..), InstrumentationScope, Key
  , fromList, insert, lookup, size, toList
  )
import OTel.Context (Context, root)
import OTel.SDK.Export (FlushError, ShutdownError)
import OTel.SDK.Resource (Resource)
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace.Export
  ( Link(..), ReadWriteSpan, ReadableSpan(..), SomeReadWriteSpan(..)
  , SomeReadableSpan(..), SpanEvent(..)
  )
import OTel.SDK.Trace.IdGenerator (IdGenerator(..), SomeIdGenerator(..))
import OTel.SDK.Trace.Processor (SpanProcessor(..), SomeSpanProcessor(..))
import OTel.SDK.Trace.Sampler
  ( AlwaysOnSampler(..), Sampler(..), SamplingDecision(..), SamplingResult(..)
  , SomeSampler(..), defaultParentBasedSampler
  )
import OTel.Timestamp (Duration, Timestamp, now)
import OTel.Trace
  ( NoOpSpan(..), Span(..), SomeSpan(..), SpanConfig(..), SpanKind(..)
  , SpanStatus(..), StatusCode(..), Tracer(..), TracerProvider(..)
  , SomeTracer(..), getSpanFromContext
  )
import OTel.Trace.SpanContext
  ( SpanContext(..), isValid, emptyTraceFlags, sampledFlag
  , traceIdFromBytes, spanIdFromBytes
  )


-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

truncateAttributeValue :: Maybe Int -> AttributeValue -> AttributeValue
truncateAttributeValue Nothing v = v
truncateAttributeValue (Just maxLen) v = case v of
  StringValue t -> StringValue (Text.take maxLen t)
  StringArrayValue arr -> StringArrayValue (fmap (Text.take maxLen) arr)
  _ -> v


-------------------------------------------------------------------------------
-- SpanLimits
-------------------------------------------------------------------------------

-- | Configurable limits for span data collection. These prevent unbounded
-- memory growth when instrumented code adds excessive attributes, events,
-- or links.
data SpanLimits = SpanLimits
  { maxAttributes :: !Int
  , maxEvents :: !Int
  , maxLinks :: !Int
  , maxAttributesPerEvent :: !Int
  , maxAttributesPerLink :: !Int
  , maxAttributeValueLength :: !(Maybe Int)
  -- ^ 'Nothing' means no truncation of attribute values.
  } deriving stock (Eq, Show)


-- | Default span limits per the OpenTelemetry specification.
defaultSpanLimits :: SpanLimits
defaultSpanLimits = SpanLimits
  { maxAttributes = 128
  , maxEvents = 128
  , maxLinks = 128
  , maxAttributesPerEvent = 128
  , maxAttributesPerLink = 128
  , maxAttributeValueLength = Nothing
  }


-------------------------------------------------------------------------------
-- RandomIdGenerator
-------------------------------------------------------------------------------

-- | An ID generator that produces random trace and span IDs using
-- @System.Random@.
data RandomIdGenerator = RandomIdGenerator

instance IdGenerator RandomIdGenerator where
  generateTraceId _ = do
    bytes <- generateRandomBytes 16
    pure (traceIdFromBytes bytes)
  generateSpanId _ = do
    bytes <- generateRandomBytes 8
    pure (spanIdFromBytes bytes)


-- | Generate n random bytes.
generateRandomBytes :: Int -> IO BS.ByteString
generateRandomBytes n = do
  ws <- mapM (\_ -> randomIO @Word8) [1..n]
  pure (BS.pack ws)


-------------------------------------------------------------------------------
-- SdkTracerProviderConfig
-------------------------------------------------------------------------------

-- | Configuration for creating an 'SdkTracerProvider'.
data SdkTracerProviderConfig = SdkTracerProviderConfig
  { providerResource :: !Resource
  , providerSampler :: !SomeSampler
  , providerProcessors :: ![SomeSpanProcessor]
  , providerSpanLimits :: !SpanLimits
  , providerIdGenerator :: !SomeIdGenerator
  }


-- | Default configuration: empty resource, parentbased_always_on sampler (spec
-- default), no processors, default span limits, random ID generator.
defaultSdkTracerProviderConfig :: SdkTracerProviderConfig
defaultSdkTracerProviderConfig = SdkTracerProviderConfig
  { providerResource = Resource.empty
  , providerSampler = SomeSampler (defaultParentBasedSampler (SomeSampler AlwaysOnSampler))
  , providerProcessors = []
  , providerSpanLimits = defaultSpanLimits
  , providerIdGenerator = SomeIdGenerator RandomIdGenerator
  }


-------------------------------------------------------------------------------
-- SdkTracerProvider
-------------------------------------------------------------------------------

-- | The SDK implementation of 'TracerProvider'. Holds configuration, manages
-- span processors, and creates 'SdkTracer' instances.
data SdkTracerProvider = SdkTracerProvider
  { sdkResource :: !Resource
  , sdkSampler :: !SomeSampler
  , sdkProcessors :: !(TVar [SomeSpanProcessor])
  , sdkSpanLimits :: !SpanLimits
  , sdkIdGenerator :: !SomeIdGenerator
  , sdkShutdown :: !(TVar Bool)
  }


-- | Create a new 'SdkTracerProvider' from the given configuration.
newSdkTracerProvider :: SdkTracerProviderConfig -> IO SdkTracerProvider
newSdkTracerProvider config = do
  processors <- newTVarIO (providerProcessors config)
  shutdownVar <- newTVarIO False
  pure SdkTracerProvider
    { sdkResource = providerResource config
    , sdkSampler = providerSampler config
    , sdkProcessors = processors
    , sdkSpanLimits = providerSpanLimits config
    , sdkIdGenerator = providerIdGenerator config
    , sdkShutdown = shutdownVar
    }


instance TracerProvider SdkTracerProvider where
  getTracer provider scope = pure (SomeTracer (SdkTracer provider scope))


-- | Register an additional span processor with the provider.
addSpanProcessor :: SdkTracerProvider -> SomeSpanProcessor -> IO ()
addSpanProcessor provider proc =
  atomically $ modifyTVar' (sdkProcessors provider) (<> [proc])


-- | Shut down the provider and all registered processors. Idempotent:
-- subsequent calls after the first return @Right ()@ immediately.
shutdown :: SdkTracerProvider -> IO (Either ShutdownError ())
shutdown provider = do
  alreadyShutdown <- atomically $ swapTVar (sdkShutdown provider) True
  if alreadyShutdown
    then pure (Right ())
    else do
      procs <- readTVarIO (sdkProcessors provider)
      results <- mapM shutdownProcessor procs
      case [e | Left e <- results] of
        [] -> pure (Right ())
        (e:_) -> pure (Left e)


-- | Force-flush all registered processors. No-op if the provider is already
-- shut down.
forceFlush :: SdkTracerProvider -> Maybe Duration -> IO (Either FlushError ())
forceFlush provider timeout = do
  isDown <- readTVarIO (sdkShutdown provider)
  if isDown
    then pure (Right ())
    else do
      procs <- readTVarIO (sdkProcessors provider)
      results <- mapM (\p -> forceFlushProcessor p timeout) procs
      case [e | Left e <- results] of
        [] -> pure (Right ())
        (e:_) -> pure (Left e)


-------------------------------------------------------------------------------
-- SdkTracer
-------------------------------------------------------------------------------

-- | A tracer bound to a specific 'SdkTracerProvider' and
-- 'InstrumentationScope'.
data SdkTracer = SdkTracer
  { tracerProvider :: !SdkTracerProvider
  , tracerScope :: !InstrumentationScope
  }


instance Tracer SdkTracer where
  startSpan tracer name parentCtx config = do
    let provider = tracerProvider tracer

    isDown <- readTVarIO (sdkShutdown provider)
    if isDown
      then pure (SomeSpan NoOpSpan)
      else do
        -- Resolve effective context for parent lookup
        let effectiveCtx = case spanParent' config of
              Just ctx -> ctx
              Nothing -> if spanNoParent' config then root else parentCtx

        -- Extract parent span context (if any)
        mParentSpanContext <- case getSpanFromContext effectiveCtx of
          Just someSpan -> do
            sc <- getSpanContext someSpan
            pure (if isValid sc then Just sc else Nothing)
          Nothing -> pure Nothing

        -- Generate IDs
        traceId_ <- case mParentSpanContext of
          Just psc -> pure (traceId psc)
          Nothing -> generateTraceId (sdkIdGenerator provider)
        spanId_ <- generateSpanId (sdkIdGenerator provider)

        -- Sample
        let initialAttrs = fromList (spanAttributes' config)
        let links = map (\(sc, attrs) -> Link sc attrs 0) (spanLinks' config)
        samplingResult <- shouldSample
          (sdkSampler provider)
          effectiveCtx
          traceId_
          name
          (spanKind' config)
          initialAttrs
          links

        case samplingDecision samplingResult of
          Drop -> do
            -- Return a non-recording span that carries the trace context
            let spanCtx_ = SpanContext
                  { traceId = traceId_
                  , spanId = spanId_
                  , traceFlags = emptyTraceFlags
                  , traceState = case mParentSpanContext of
                      Just psc -> OTel.Trace.SpanContext.traceState psc
                      Nothing -> samplingTraceState samplingResult
                  , _isRemote = False
                  }
            pure (SomeSpan (DroppedSpan spanCtx_))
          decision -> do
            -- Create a recording span
            let traceFlags_ = case decision of
                  RecordAndSample -> sampledFlag
                  _ -> emptyTraceFlags
            let traceState_ = case mParentSpanContext of
                  Just psc -> OTel.Trace.SpanContext.traceState psc
                  Nothing -> samplingTraceState samplingResult
            let spanCtx_ = SpanContext
                  { traceId = traceId_
                  , spanId = spanId_
                  , traceFlags = traceFlags_
                  , traceState = traceState_
                  , _isRemote = False
                  }

            startTime <- case spanStartTimestamp' config of
              Just ts -> pure ts
              Nothing -> now

            -- Create mutable span state
            spanStateVar <- newTVarIO SdkSpanState
              { ssSpanCtx = spanCtx_
              , ssParentCtx = mParentSpanContext
              , ssName = name
              , ssKind = spanKind' config
              , ssStartTime = startTime
              , ssEndTime = Nothing
              , ssAttrs = initialAttrs <> samplingAttributes samplingResult
              , ssEvents = []
              , ssLinks = links
              , ssStatus = SpanStatus Unset Nothing
              , ssEnded = False
              , ssDroppedAttrs = 0
              , ssDroppedEvents = 0
              , ssDroppedLinks = 0
              }

            let sdkSpan = SdkSpan
                  { sdkSpanState = spanStateVar
                  , sdkSpanResource = sdkResource provider
                  , sdkSpanScope = tracerScope tracer
                  , sdkSpanLimits_ = sdkSpanLimits provider
                  , sdkSpanProcessors = sdkProcessors provider
                  }

            -- Notify processors of span start
            procs <- readTVarIO (sdkProcessors provider)
            let rwSpan = SomeReadWriteSpan sdkSpan
            mapM_ (\p -> onStart p rwSpan effectiveCtx) procs

            pure (SomeSpan sdkSpan)


-- Field accessors for SpanConfig that avoid DuplicateRecordFields ambiguity.
-- SpanConfig is defined in OTel.Trace with fields named spanKind, spanAttributes, etc.
-- With DuplicateRecordFields these field names are ambiguous, so we use explicit
-- pattern matching via helper functions.

spanKind' :: SpanConfig -> SpanKind
spanKind' cfg = cfg.spanKind

spanAttributes' :: SpanConfig -> [(Key, AttributeValue)]
spanAttributes' cfg = cfg.spanAttributes

spanLinks' :: SpanConfig -> [(SpanContext, Attributes)]
spanLinks' cfg = cfg.spanLinks

spanStartTimestamp' :: SpanConfig -> Maybe Timestamp
spanStartTimestamp' cfg = cfg.spanStartTimestamp

spanParent' :: SpanConfig -> Maybe Context
spanParent' cfg = cfg.spanParent

spanNoParent' :: SpanConfig -> Bool
spanNoParent' cfg = cfg.spanNoParent


-------------------------------------------------------------------------------
-- DroppedSpan (internal — for Drop sampling decision)
-------------------------------------------------------------------------------

-- | A non-recording span returned when the sampler decides to drop. Carries
-- a valid SpanContext so context propagation still works.
data DroppedSpan = DroppedSpan !SpanContext

instance Span DroppedSpan where
  getSpanContext (DroppedSpan sc) = pure sc
  isRecording _ = pure False
  setAttribute _ _ _ = pure ()
  addEvent _ _ _ _ = pure ()
  addLink _ _ _ = pure ()
  setStatus _ _ _ = pure ()
  recordException _ _ _ = pure ()
  updateName _ _ = pure ()
  end _ _ = pure ()


-------------------------------------------------------------------------------
-- SdkSpan (internal)
-------------------------------------------------------------------------------

-- | Mutable state of an in-flight span.
data SdkSpanState = SdkSpanState
  { ssSpanCtx :: !SpanContext
  , ssParentCtx :: !(Maybe SpanContext)
  , ssName :: !Text
  , ssKind :: !SpanKind
  , ssStartTime :: !Timestamp
  , ssEndTime :: !(Maybe Timestamp)
  , ssAttrs :: !Attributes
  , ssEvents :: ![SpanEvent]
  , ssLinks :: ![Link]
  , ssStatus :: !SpanStatus
  , ssEnded :: !Bool
  , ssDroppedAttrs :: !Int
  , ssDroppedEvents :: !Int
  , ssDroppedLinks :: !Int
  }


-- | The SDK span implementation. Holds a TVar for mutable state and
-- references to provider-level configuration.
data SdkSpan = SdkSpan
  { sdkSpanState :: !(TVar SdkSpanState)
  , sdkSpanResource :: !Resource
  , sdkSpanScope :: !InstrumentationScope
  , sdkSpanLimits_ :: !SpanLimits
  , sdkSpanProcessors :: !(TVar [SomeSpanProcessor])
  }


instance Span SdkSpan where
  getSpanContext s = do
    state <- readTVarIO (sdkSpanState s)
    pure (ssSpanCtx state)

  isRecording s = do
    state <- readTVarIO (sdkSpanState s)
    pure (not (ssEnded state))

  setAttribute s key val = do
    let truncatedVal = truncateAttributeValue (maxAttributeValueLength (sdkSpanLimits_ s)) val
    atomically $ modifyTVar' (sdkSpanState s) $ \state ->
      if ssEnded state then state
      else if size (ssAttrs state) >= maxAttributes (sdkSpanLimits_ s)
              && isNothing' (lookup key (ssAttrs state))
        then state { ssDroppedAttrs = ssDroppedAttrs state + 1 }
        else state { ssAttrs = insert key truncatedVal (ssAttrs state) }

  addEvent s evtName attrs mTimestamp = do
    ts <- case mTimestamp of
      Just t -> pure t
      Nothing -> now
    let limit = maxAttributesPerEvent (sdkSpanLimits_ s)
        attrCount = size attrs
        (truncatedAttrs, droppedCount) =
          if attrCount > limit
            then (fromList (Prelude.take limit (toList attrs)), attrCount - limit)
            else (attrs, 0)
    atomically $ modifyTVar' (sdkSpanState s) $ \state ->
      if ssEnded state then state
      else if length (ssEvents state) >= maxEvents (sdkSpanLimits_ s)
        then state { ssDroppedEvents = ssDroppedEvents state + 1 }
        else state { ssEvents = ssEvents state <> [SpanEvent evtName ts truncatedAttrs droppedCount] }

  addLink s sc attrs = do
    let limit = maxAttributesPerLink (sdkSpanLimits_ s)
        attrCount = size attrs
        (truncatedAttrs, droppedCount) =
          if attrCount > limit
            then (fromList (Prelude.take limit (toList attrs)), attrCount - limit)
            else (attrs, 0)
    atomically $ modifyTVar' (sdkSpanState s) $ \state ->
      if ssEnded state then state
      else if length (ssLinks state) >= maxLinks (sdkSpanLimits_ s)
        then state { ssDroppedLinks = ssDroppedLinks state + 1 }
        else state { ssLinks = ssLinks state <> [Link sc truncatedAttrs droppedCount] }

  setStatus s code desc = do
    atomically $ modifyTVar' (sdkSpanState s) $ \state ->
      if ssEnded state then state
      else if code == Unset then state  -- Unset never overrides a prior status
      else if statusCode (ssStatus state) == Ok then state
      else state { ssStatus = SpanStatus code (if code == Error then desc else Nothing) }

  recordException s exc attrs = do
    ts <- now
    let exAttrs = fromList
          [ ("exception.type", StringValue (pack (show (typeOf exc))))
          , ("exception.message", StringValue (pack (show exc)))
          ]
    addEvent s "exception" (exAttrs <> attrs) (Just ts)

  updateName s newName = do
    atomically $ modifyTVar' (sdkSpanState s) $ \state ->
      if ssEnded state then state
      else state { ssName = newName }

  end s mTimestamp = do
    endTime <- case mTimestamp of
      Just t -> pure t
      Nothing -> now
    -- Atomically set ended + endTime; check if already ended
    wasAlreadyEnded <- atomically $ do
      state <- readTVar (sdkSpanState s)
      if ssEnded state
        then pure True
        else do
          writeTVar (sdkSpanState s) state
            { ssEnded = True
            , ssEndTime = Just endTime
            }
          pure False
    -- Notify processors only on first end
    unless wasAlreadyEnded $ do
      state <- readTVarIO (sdkSpanState s)
      let snapshot = SpanSnapshot
            { snapContext = ssSpanCtx state
            , snapParentContext = ssParentCtx state
            , snapName = ssName state
            , snapKind = ssKind state
            , snapStartTimestamp = ssStartTime state
            , snapEndTimestamp = endTime
            , snapAttributes = ssAttrs state
            , snapEvents = ssEvents state
            , snapLinks = ssLinks state
            , snapStatus = ssStatus state
            , snapResource = sdkSpanResource s
            , snapScope = sdkSpanScope s
            , snapDroppedAttributesCount = ssDroppedAttrs state
            , snapDroppedEventsCount = ssDroppedEvents state
            , snapDroppedLinksCount = ssDroppedLinks state
            }
      procs <- readTVarIO (sdkSpanProcessors s)
      mapM_ (\p -> onEnd p (SomeReadableSpan snapshot)) procs


-- | Pure read of the span state via unsafePerformIO. Safe because readTVarIO
-- is a single atomic read with no observable side effects beyond memory access.
unsafeReadSpanState :: SdkSpan -> SdkSpanState
unsafeReadSpanState s = unsafePerformIO (readTVarIO (sdkSpanState s))
{-# NOINLINE unsafeReadSpanState #-}


instance ReadableSpan SdkSpan where
  readSpanContext s = ssSpanCtx (unsafeReadSpanState s)
  readParentSpanContext s = ssParentCtx (unsafeReadSpanState s)
  readName s = ssName (unsafeReadSpanState s)
  readKind s = ssKind (unsafeReadSpanState s)
  readStartTimestamp s = ssStartTime (unsafeReadSpanState s)
  readEndTimestamp s = case ssEndTime (unsafeReadSpanState s) of
    Just t -> t
    Nothing -> ssStartTime (unsafeReadSpanState s)
  readAttributes s = ssAttrs (unsafeReadSpanState s)
  readEvents s = ssEvents (unsafeReadSpanState s)
  readLinks s = ssLinks (unsafeReadSpanState s)
  readStatus s = ssStatus (unsafeReadSpanState s)
  readResource s = sdkSpanResource s
  readInstrumentationScope s = sdkSpanScope s
  readDroppedAttributesCount s = ssDroppedAttrs (unsafeReadSpanState s)
  readDroppedEventsCount s = ssDroppedEvents (unsafeReadSpanState s)
  readDroppedLinksCount s = ssDroppedLinks (unsafeReadSpanState s)


instance ReadWriteSpan SdkSpan


-------------------------------------------------------------------------------
-- SpanSnapshot (internal)
-------------------------------------------------------------------------------

-- | An immutable snapshot of a completed span, taken at 'end' time.
-- Passed to processors via 'onEnd'.
data SpanSnapshot = SpanSnapshot
  { snapContext :: !SpanContext
  , snapParentContext :: !(Maybe SpanContext)
  , snapName :: !Text
  , snapKind :: !SpanKind
  , snapStartTimestamp :: !Timestamp
  , snapEndTimestamp :: !Timestamp
  , snapAttributes :: !Attributes
  , snapEvents :: ![SpanEvent]
  , snapLinks :: ![Link]
  , snapStatus :: !SpanStatus
  , snapResource :: !Resource
  , snapScope :: !InstrumentationScope
  , snapDroppedAttributesCount :: !Int
  , snapDroppedEventsCount :: !Int
  , snapDroppedLinksCount :: !Int
  }


instance ReadableSpan SpanSnapshot where
  readSpanContext = snapContext
  readParentSpanContext = snapParentContext
  readName = snapName
  readKind = snapKind
  readStartTimestamp = snapStartTimestamp
  readEndTimestamp = snapEndTimestamp
  readAttributes = snapAttributes
  readEvents = snapEvents
  readLinks = snapLinks
  readStatus = snapStatus
  readResource = snapResource
  readInstrumentationScope = snapScope
  readDroppedAttributesCount = snapDroppedAttributesCount
  readDroppedEventsCount = snapDroppedEventsCount
  readDroppedLinksCount = snapDroppedLinksCount


-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Check if a Maybe is Nothing (avoiding import of Data.Maybe for a single use).
isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' (Just _) = False
