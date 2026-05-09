{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | SDK MeterProvider implementation: SdkMeterProvider, SdkMeter, and concrete
-- instrument types (Counter, UpDownCounter, Histogram, Gauge, observable
-- variants), with TVar-based accumulation and per-reader collection.
module OTel.SDK.Metric
  ( -- * SdkMeterProvider
    SdkMeterProvider
  , SdkMeterProviderConfig (..)
  , defaultSdkMeterProviderConfig
  , newSdkMeterProvider
  , sdkMeterProviderShutdown
  , sdkMeterProviderForceFlush

    -- * Re-exports
  , module OTel.SDK.Metric.Export
  , module OTel.SDK.Metric.Reader
  , module OTel.SDK.Metric.View
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar
  )
import Control.Monad (forM, forM_, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Data.Vector qualified as Vector
import Data.Word (Word64)

import OTel.Attribute
  ( Attributes (..), InstrumentationScope (..)
  , emptyAttributes
  )
import OTel.Context (getCurrent)
import OTel.Metric
  ( Meter (..), MeterProvider (..), SomeMeter (..), NoOpMeter (..)
  , SomeCounter (..), SomeUpDownCounter (..), SomeHistogram (..), SomeGauge (..)
  , SomeObservableCounter (..), SomeObservableUpDownCounter (..)
  , SomeObservableGauge (..)
  , SomeCallbackRegistration (..)
  )
import OTel.Metric.Instrument
  ( Counter (..), UpDownCounter (..), Histogram (..), Gauge (..)
  , ObservableCounter (..), ObservableUpDownCounter (..), ObservableGauge (..)
  , ObservableResult (..), SomeObservableResult (..)
  , BatchObservableResult (..), SomeBatchObservableResult (..)
  , BatchObservableCallback
  , SomeObservableInstrument (..)
  , ObservableCallback
  , CallbackRegistration (..)
  , InstrumentOptions (..)
  , NoOpCounter (..), NoOpUpDownCounter (..), NoOpHistogram (..), NoOpGauge (..)
  , NoOpObservableCounter (..), NoOpObservableUpDownCounter (..)
  , NoOpObservableGauge (..)
  )
import OTel.SDK.Export (FlushError, ShutdownError)
import OTel.SDK.Metric.Export
import OTel.SDK.Metric.Reader
import OTel.SDK.Metric.View (View, matchesInstrument, applyView)
import OTel.SDK.Resource (Resource)
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (Duration, Timestamp, now)
import OTel.Trace (Span (..), SomeSpan (..), getSpanFromContext)
import OTel.Trace.SpanContext (SpanContext (..), isSampled, isValid)


-- Ord instances for AttributeValue, Attributes, InstrumentationScope
-- are provided by OTel.SDK.Metric.View (orphan instances).


-------------------------------------------------------------------------------
-- Internal accumulator types
-------------------------------------------------------------------------------

data CounterAcc = CounterAcc
  { caSum       :: !Double
  , caStartTime :: !Timestamp
  , caExemplars :: ![Exemplar]
  }

data GaugeAcc = GaugeAcc
  { gaValue     :: !Double
  , gaTime      :: !Timestamp
  , gaStartTime :: !Timestamp
  , gaExemplars :: ![Exemplar]
  }

data HistogramAcc = HistogramAcc
  { haCount      :: !Word64
  , haSum        :: !Double
  , haBuckets    :: !(Vector.Vector Word64)  -- length = length boundaries + 1
  , haBoundaries :: ![Double]
  , haMin        :: !(Maybe Double)
  , haMax        :: !(Maybe Double)
  , haStartTime  :: !Timestamp
  , haExemplars  :: ![Exemplar]
  }

-------------------------------------------------------------------------------
-- Histogram helpers
-------------------------------------------------------------------------------

-- | Record a value into a histogram accumulator by finding the correct bucket.
recordHistogram :: Double -> HistogramAcc -> HistogramAcc
recordHistogram v acc =
  let idx = findBucketIndex v (haBoundaries acc)
      buckets' = haBuckets acc Vector.// [(idx, (haBuckets acc Vector.! idx) + 1)]
      newMin = case haMin acc of
        Nothing -> Just v
        Just m  -> Just (min m v)
      newMax = case haMax acc of
        Nothing -> Just v
        Just m  -> Just (max m v)
  in acc
    { haCount   = haCount acc + 1
    , haSum     = haSum acc + v
    , haBuckets = buckets'
    , haMin     = newMin
    , haMax     = newMax
    }


-- | Find the bucket index for a value given explicit boundaries.
-- Bucket i covers (boundaries[i-1], boundaries[i]].
-- The first bucket covers (-inf, boundaries[0]].
-- The last bucket covers (boundaries[n-1], +inf).
findBucketIndex :: Double -> [Double] -> Int
findBucketIndex v bounds = go 0 bounds
  where
    go i []     = i
    go i (b:bs)
      | v <= b    = i
      | otherwise = go (i + 1) bs


-------------------------------------------------------------------------------
-- Exemplar capture
-------------------------------------------------------------------------------

-- | Capture an exemplar from the current trace context (trace-based filter).
-- Returns Nothing if there is no valid, sampled span in context.
captureExemplar :: Double -> IO (Maybe Exemplar)
captureExemplar value = do
  ctx <- getCurrent
  case getSpanFromContext ctx of
    Nothing -> pure Nothing
    Just (SomeSpan span_) -> do
      sc <- getSpanContext span_
      if isSampled (sc.traceFlags) && isValid sc
        then do
          t <- now
          pure $ Just Exemplar
            { exemplarAttributes = emptyAttributes
            , exemplarTime       = t
            , exemplarValue      = value
            , exemplarSpanId     = Just (sc.spanId)
            , exemplarTraceId    = Just (sc.traceId)
            }
        else pure Nothing


-------------------------------------------------------------------------------
-- InstrumentRecord (internal registry entry)
-------------------------------------------------------------------------------

data InstrumentRecord = InstrumentRecord
  { irScope   :: !InstrumentationScope
  , irKind    :: !InstrumentKind
  , irName    :: !Text
  , irCollect :: !(Map Text (Map Attributes Double) -> AggregationTemporality -> IO (Maybe Metric))
  }


-------------------------------------------------------------------------------
-- Concrete instrument types
-------------------------------------------------------------------------------

data SdkCounter = SdkCounter
  { _sdkCounterName :: !Text
  , _sdkCounterDesc :: !Text
  , _sdkCounterUnit :: !Text
  , _sdkCounterAcc  :: !(TVar (Map Attributes CounterAcc))
  }

instance Counter SdkCounter where
  counterAdd counter value attrs = when (value >= 0) $ do
    mexemplar <- captureExemplar value
    t <- now
    atomically $ modifyTVar' (_sdkCounterAcc counter) $ \m ->
      let acc = Map.findWithDefault (CounterAcc 0 t []) attrs m
          acc' = acc { caSum = caSum acc + value
                     , caExemplars = maybe (caExemplars acc) (: caExemplars acc) mexemplar
                     }
      in Map.insert attrs acc' m


data SdkUpDownCounter = SdkUpDownCounter
  { _sdkUDCName :: !Text
  , _sdkUDCDesc :: !Text
  , _sdkUDCUnit :: !Text
  , _sdkUDCAcc  :: !(TVar (Map Attributes CounterAcc))
  }

instance UpDownCounter SdkUpDownCounter where
  upDownCounterAdd counter value attrs = do
    mexemplar <- captureExemplar value
    t <- now
    atomically $ modifyTVar' (_sdkUDCAcc counter) $ \m ->
      let acc = Map.findWithDefault (CounterAcc 0 t []) attrs m
          acc' = acc { caSum = caSum acc + value
                     , caExemplars = maybe (caExemplars acc) (: caExemplars acc) mexemplar
                     }
      in Map.insert attrs acc' m


data SdkHistogram = SdkHistogram
  { _sdkHistName       :: !Text
  , _sdkHistDesc       :: !Text
  , _sdkHistUnit       :: !Text
  , _sdkHistBoundaries :: ![Double]
  , _sdkHistAcc        :: !(TVar (Map Attributes HistogramAcc))
  }

instance Histogram SdkHistogram where
  histogramRecord hist value attrs = when (value >= 0) $ do
    mexemplar <- captureExemplar value
    t <- now
    atomically $ modifyTVar' (_sdkHistAcc hist) $ \m ->
      let bounds = _sdkHistBoundaries hist
          acc = Map.findWithDefault
                  (HistogramAcc 0 0 (Vector.replicate (length bounds + 1) 0) bounds Nothing Nothing t [])
                  attrs m
          acc' = recordHistogram value acc
          acc'' = acc' { haExemplars = maybe (haExemplars acc') (: haExemplars acc') mexemplar }
      in Map.insert attrs acc'' m


data SdkGauge = SdkGauge
  { _sdkGaugeName :: !Text
  , _sdkGaugeDesc :: !Text
  , _sdkGaugeUnit :: !Text
  , _sdkGaugeAcc  :: !(TVar (Map Attributes GaugeAcc))
  }

instance Gauge SdkGauge where
  gaugeSet gauge value attrs = do
    mexemplar <- captureExemplar value
    t <- now
    atomically $ modifyTVar' (_sdkGaugeAcc gauge) $ \m ->
      let acc = Map.findWithDefault (GaugeAcc 0 t t []) attrs m
          acc' = acc { gaValue = value
                     , gaTime  = t
                     , gaExemplars = maybe (gaExemplars acc) (: gaExemplars acc) mexemplar
                     }
      in Map.insert attrs acc' m


-------------------------------------------------------------------------------
-- Observable instruments
-------------------------------------------------------------------------------

data SdkObservableInstr = SdkObservableInstr
  { _sdkObsName      :: !Text
  , _sdkObsDesc      :: !Text
  , _sdkObsUnit      :: !Text
  , _sdkObsKind      :: !InstrumentKind
  , _sdkObsCallbacks :: !(TVar [ObservableCallback])
  }

instance ObservableCounter SdkObservableInstr where
  addObservableCounterCallback instr cb =
    atomically $ modifyTVar' (_sdkObsCallbacks instr) (cb :)

instance ObservableUpDownCounter SdkObservableInstr where
  addObservableUpDownCounterCallback instr cb =
    atomically $ modifyTVar' (_sdkObsCallbacks instr) (cb :)

instance ObservableGauge SdkObservableInstr where
  addObservableGaugeCallback instr cb =
    atomically $ modifyTVar' (_sdkObsCallbacks instr) (cb :)


-- | Internal observable result that accumulates into a TVar.
newtype SdkObsResult = SdkObsResult (TVar (Map Attributes Double))

-- | Batch observable result that routes observations by instrument name.
newtype SdkBatchObsResult = SdkBatchObsResult (TVar (Map Text (Map Attributes Double)))

instance BatchObservableResult SdkBatchObsResult where
  batchObserveValue (SdkBatchObsResult var) instrument value attrs =
    atomically $ modifyTVar' var $
      Map.insertWith Map.union (soiName instrument) (Map.singleton attrs value)

-- | A registered batch callback with a stable identity for deregistration.
data BatchRegistration = BatchRegistration
  { brId          :: !Unique
  , _brInstruments :: ![SomeObservableInstrument]
  , brCallback    :: !BatchObservableCallback
  }

-- | A real callback registration that removes itself from the provider's
-- batch registry when 'unregister' is called. Idempotent.
data SdkCallbackRegistration = SdkCallbackRegistration
  { _sbrRegsVar :: !(TVar [BatchRegistration])
  , _sbrId      :: !Unique
  }

instance CallbackRegistration SdkCallbackRegistration where
  unregister (SdkCallbackRegistration regsVar uid) =
    atomically $ modifyTVar' regsVar (filter (\r -> brId r /= uid))

instance ObservableResult SdkObsResult where
  observeValue (SdkObsResult var) value attrs =
    atomically $ modifyTVar' var (Map.insert attrs value)


-------------------------------------------------------------------------------
-- Collect functions
-------------------------------------------------------------------------------

-- | Extract description and unit from optional instrument options.
optDescUnit :: Maybe InstrumentOptions -> (Text, Text)
optDescUnit Nothing = ("", "")
optDescUnit (Just opts) =
  ( fromMaybe "" (opts.instrumentDescription)
  , fromMaybe "" (opts.instrumentUnit)
  )


mkCounterCollect
  :: Bool -> Text -> Text -> Text -> TVar (Map Attributes CounterAcc)
  -> AggregationTemporality -> IO (Maybe Metric)
mkCounterCollect isMonotonic name desc unit accVar temporality = do
  t <- now
  accs <- atomically $ do
    m <- readTVar accVar
    when (temporality == Delta) $
      writeTVar accVar (Map.map (\a -> a { caSum = 0, caExemplars = [], caStartTime = t }) m)
    pure m
  if Map.null accs
    then pure Nothing
    else do
      let points =
            [ NumberDataPoint
                { ndpAttributes = attrs
                , ndpStartTime  = caStartTime acc
                , ndpTime       = t
                , ndpValue      = caSum acc
                , ndpExemplars  = caExemplars acc
                }
            | (attrs, acc) <- Map.toList accs
            ]
      pure $ Just Metric
        { metricName = name
        , metricDescription = desc
        , metricUnit = unit
        , metricPointData = SumPointData SumData
            { sumDataPoints = points
            , sumTemporality = temporality
            , sumIsMonotonic = isMonotonic
            }
        }


mkGaugeCollect
  :: Text -> Text -> Text -> TVar (Map Attributes GaugeAcc)
  -> AggregationTemporality -> IO (Maybe Metric)
mkGaugeCollect name desc unit accVar _temporality = do
  t <- now
  accs <- readTVarIO accVar
  if Map.null accs
    then pure Nothing
    else do
      let points =
            [ NumberDataPoint
                { ndpAttributes = attrs
                , ndpStartTime  = gaStartTime acc
                , ndpTime       = gaTime acc
                , ndpValue      = gaValue acc
                , ndpExemplars  = gaExemplars acc
                }
            | (attrs, acc) <- Map.toList accs
            ]
      let _ = t  -- suppress unused warning
      pure $ Just Metric
        { metricName = name
        , metricDescription = desc
        , metricUnit = unit
        , metricPointData = GaugePointData GaugeData { gaugeDataPoints = points }
        }


mkHistogramCollect
  :: Text -> Text -> Text -> TVar (Map Attributes HistogramAcc)
  -> AggregationTemporality -> IO (Maybe Metric)
mkHistogramCollect name desc unit accVar temporality = do
  t <- now
  accs <- atomically $ do
    m <- readTVar accVar
    when (temporality == Delta) $
      writeTVar accVar $ Map.map (\a -> a
        { haCount = 0
        , haSum = 0
        , haBuckets = Vector.replicate (Vector.length (haBuckets a)) 0
        , haMin = Nothing
        , haMax = Nothing
        , haExemplars = []
        , haStartTime = t
        }) m
    pure m
  if Map.null accs
    then pure Nothing
    else do
      let points =
            [ HistogramDataPoint
                { hdpAttributes     = attrs
                , hdpStartTime      = haStartTime acc
                , hdpTime           = t
                , hdpCount          = haCount acc
                , hdpSum            = Just (haSum acc)
                , hdpBucketCounts   = Vector.toList (haBuckets acc)
                , hdpExplicitBounds = haBoundaries acc
                , hdpMin            = haMin acc
                , hdpMax            = haMax acc
                , hdpExemplars      = haExemplars acc
                }
            | (attrs, acc) <- Map.toList accs
            ]
      pure $ Just Metric
        { metricName = name
        , metricDescription = desc
        , metricUnit = unit
        , metricPointData = HistogramPointData HistogramData
            { histDataPoints = points
            , histTemporality = temporality
            }
        }


mkObservableCollect
  :: Bool -> Text -> Text -> Text -> InstrumentKind -> TVar [ObservableCallback]
  -> Map Text (Map Attributes Double)
  -> AggregationTemporality -> IO (Maybe Metric)
mkObservableCollect isMonotonic name desc unit kind callbacksVar batchObs temporality = do
  callbacks <- readTVarIO callbacksVar
  resultVar <- newTVarIO Map.empty
  let obsResult = SdkObsResult resultVar
  forM_ callbacks $ \cb -> cb (SomeObservableResult obsResult)
  t <- now
  observed <- readTVarIO resultVar
  -- Merge batch observations with per-instrument callback observations.
  -- Gauge (LastValue): batch wins (Map.union left-biased; batch fires last).
  -- Additive instruments (Counter, UpDownCounter): sum both sources per spec.
  let batchForThis = Map.findWithDefault Map.empty name batchObs
      merged = case kind of
        ObservableGaugeKind -> Map.union batchForThis observed
        _                   -> Map.unionWith (+) batchForThis observed
  if Map.null merged
    then pure Nothing
    else do
      let points =
            [ NumberDataPoint
                { ndpAttributes = attrs
                , ndpStartTime  = t
                , ndpTime       = t
                , ndpValue      = v
                , ndpExemplars  = []
                }
            | (attrs, v) <- Map.toList merged
            ]
      let pointData = case kind of
            ObservableGaugeKind -> GaugePointData (GaugeData points)
            _ -> SumPointData (SumData points temporality isMonotonic)
      pure $ Just Metric
        { metricName = name
        , metricDescription = desc
        , metricUnit = unit
        , metricPointData = pointData
        }


-------------------------------------------------------------------------------
-- SdkMeter
-------------------------------------------------------------------------------

data SdkMeter = SdkMeter
  { sdkMeterScope     :: !InstrumentationScope
  , sdkMeterRegistry  :: !(TVar [InstrumentRecord])
  , sdkMeterShutdown  :: !(TVar Bool)
  , sdkMeterBatchRegs :: !(TVar [BatchRegistration])
  }

-- | Register an instrument record with the provider's registry.
registerInstrument :: TVar [InstrumentRecord] -> InstrumentRecord -> IO ()
registerInstrument registry ir =
  atomically $ modifyTVar' registry (ir :)


instance Meter SdkMeter where
  createCounter m name opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeCounter NoOpCounter)
      else do
        accVar <- newTVarIO Map.empty
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = CounterKind
              , irName  = name
              , irCollect = \_ -> mkCounterCollect True name desc unit accVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeCounter $ SdkCounter name desc unit accVar

  createUpDownCounter m name opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeUpDownCounter NoOpUpDownCounter)
      else do
        accVar <- newTVarIO Map.empty
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = UpDownCounterKind
              , irName  = name
              , irCollect = \_ -> mkCounterCollect False name desc unit accVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeUpDownCounter $ SdkUpDownCounter name desc unit accVar

  createHistogram m name opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeHistogram NoOpHistogram)
      else do
        accVar <- newTVarIO Map.empty
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = HistogramKind
              , irName  = name
              , irCollect = \_ -> mkHistogramCollect name desc unit accVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeHistogram $ SdkHistogram name desc unit defaultHistogramBoundaries accVar

  createGauge m name opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeGauge NoOpGauge)
      else do
        accVar <- newTVarIO Map.empty
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = GaugeKind
              , irName  = name
              , irCollect = \_ -> mkGaugeCollect name desc unit accVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeGauge $ SdkGauge name desc unit accVar

  createObservableCounter m name cbs opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeObservableCounter NoOpObservableCounter)
      else do
        cbVar <- newTVarIO cbs
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = ObservableCounterKind
              , irName  = name
              , irCollect = mkObservableCollect True name desc unit ObservableCounterKind cbVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeObservableCounter $ SdkObservableInstr name desc unit ObservableCounterKind cbVar

  createObservableUpDownCounter m name cbs opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeObservableUpDownCounter NoOpObservableUpDownCounter)
      else do
        cbVar <- newTVarIO cbs
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = ObservableUpDownCounterKind
              , irName  = name
              , irCollect = mkObservableCollect False name desc unit ObservableUpDownCounterKind cbVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeObservableUpDownCounter $ SdkObservableInstr name desc unit ObservableUpDownCounterKind cbVar

  createObservableGauge m name cbs opts = do
    shutdown_ <- readTVarIO (sdkMeterShutdown m)
    if shutdown_
      then pure (SomeObservableGauge NoOpObservableGauge)
      else do
        cbVar <- newTVarIO cbs
        let (desc, unit) = optDescUnit opts
        let ir = InstrumentRecord
              { irScope = sdkMeterScope m
              , irKind  = ObservableGaugeKind
              , irName  = name
              , irCollect = mkObservableCollect False name desc unit ObservableGaugeKind cbVar
              }
        registerInstrument (sdkMeterRegistry m) ir
        pure $ SomeObservableGauge $ SdkObservableInstr name desc unit ObservableGaugeKind cbVar

  registerCallback m instruments callback = do
    uid <- newUnique
    let reg = BatchRegistration { brId = uid, _brInstruments = instruments, brCallback = callback }
    atomically $ modifyTVar' (sdkMeterBatchRegs m) (reg :)
    pure $ SomeCallbackRegistration $
      SdkCallbackRegistration (sdkMeterBatchRegs m) uid


-------------------------------------------------------------------------------
-- SdkMeterProvider
-------------------------------------------------------------------------------

-- | Configuration for creating a 'SdkMeterProvider'.
data SdkMeterProviderConfig = SdkMeterProviderConfig
  { providerResource :: !Resource
  , providerReaders  :: ![SomeMetricReader]
  , providerViews    :: ![View]
  } deriving stock (Show)


-- | Default meter provider config: empty resource, no readers, no views.
defaultSdkMeterProviderConfig :: SdkMeterProviderConfig
defaultSdkMeterProviderConfig = SdkMeterProviderConfig
  { providerResource = Resource.empty
  , providerReaders  = []
  , providerViews    = []
  }


-- | SDK implementation of 'MeterProvider'. See the OTel Metrics SDK spec.
data SdkMeterProvider = SdkMeterProvider
  { sdkProviderResource  :: !Resource
  , sdkProviderRegistry  :: !(TVar [InstrumentRecord])
  , sdkProviderReaders   :: ![SomeMetricReader]
  , sdkProviderViews     :: ![View]
  , sdkProviderShutdown  :: !(TVar Bool)
  , sdkProviderBatchRegs :: !(TVar [BatchRegistration])
  }


-- | Create a new 'SdkMeterProvider' and wire up each reader with its
-- collect source.
newSdkMeterProvider :: SdkMeterProviderConfig -> IO SdkMeterProvider
newSdkMeterProvider cfg = do
  registry <- newTVarIO []
  shutdownFlag <- newTVarIO False
  batchRegs <- newTVarIO []
  let provider = SdkMeterProvider
        { sdkProviderResource  = providerResource cfg
        , sdkProviderRegistry  = registry
        , sdkProviderReaders   = providerReaders cfg
        , sdkProviderViews     = providerViews cfg
        , sdkProviderShutdown  = shutdownFlag
        , sdkProviderBatchRegs = batchRegs
        }
  -- Wire up each reader with a collect source tailored to its temporality
  forM_ (providerReaders cfg) $ \reader ->
    readerSetCollectSource reader (collectAllForReader provider reader)
  pure provider


-- | Collect all metrics grouped by scope, using the reader's temporality
-- preference for each instrument kind.
collectAllForReader :: SdkMeterProvider -> SomeMetricReader -> IO MetricData
collectAllForReader p reader = do
  -- Run all batch callbacks and collect their observations
  batchRegs <- readTVarIO (sdkProviderBatchRegs p)
  batchObsVar <- newTVarIO (Map.empty :: Map Text (Map Attributes Double))
  forM_ batchRegs $ \reg -> do
    let result = SdkBatchObsResult batchObsVar
    brCallback reg (SomeBatchObservableResult result)
  batchObs <- readTVarIO batchObsVar

  instruments <- readTVarIO (sdkProviderRegistry p)
  let views = sdkProviderViews p
  let byScope = Map.fromListWith (++)
        [(irScope ir, [ir]) | ir <- instruments]
  scopeMetricsList <- forM (Map.toList byScope) $ \(scope, instrs) -> do
    allMetrics <- fmap concat $ forM instrs $ \ir -> do
      let temporality = readerTemporality reader (irKind ir)
      mMetric <- irCollect ir batchObs temporality
      case mMetric of
        Nothing     -> pure []
        Just metric ->
          let matchingViews = filter
                (\v -> matchesInstrument v (irScope ir) (irKind ir) (irName ir))
                views
          in case matchingViews of
               []  -> pure [metric]
               vs  -> pure (catMaybes (map (`applyView` metric) vs))
    pure $ ScopeMetrics scope allMetrics
  pure $ MetricData (sdkProviderResource p) scopeMetricsList


instance MeterProvider SdkMeterProvider where
  getMeter p scope = do
    shutdown_ <- readTVarIO (sdkProviderShutdown p)
    if shutdown_
      then pure (SomeMeter NoOpMeter)
      else pure $ SomeMeter SdkMeter
             { sdkMeterScope     = scope
             , sdkMeterRegistry  = sdkProviderRegistry p
             , sdkMeterShutdown  = sdkProviderShutdown p
             , sdkMeterBatchRegs = sdkProviderBatchRegs p
             }


-- | Shut down the provider: sets the shutdown flag and shuts down all readers.
sdkMeterProviderShutdown :: SdkMeterProvider -> IO (Either ShutdownError ())
sdkMeterProviderShutdown p = do
  atomically $ writeTVar (sdkProviderShutdown p) True
  results <- mapM readerShutdown (sdkProviderReaders p)
  pure $ case [e | Left e <- results] of
    []    -> Right ()
    (e:_) -> Left e


-- | Force flush all readers.
sdkMeterProviderForceFlush :: SdkMeterProvider -> Maybe Duration -> IO (Either FlushError ())
sdkMeterProviderForceFlush p timeout_ = do
  results <- mapM (\r -> readerForceFlush r timeout_) (sdkProviderReaders p)
  pure $ case [e | Left e <- results] of
    []    -> Right ()
    (e:_) -> Left e
