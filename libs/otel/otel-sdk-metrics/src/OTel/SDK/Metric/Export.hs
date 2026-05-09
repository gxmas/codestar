-- | OTel metric data model and MetricExporter type class.
--
-- This module defines all types needed to represent collected metrics
-- (data points, aggregations, scope grouping) and the interface that
-- exporters implement to ship metric data out of process.
module OTel.SDK.Metric.Export
  ( -- * Instrument kind
    InstrumentKind (..)
    -- * Aggregation temporality
  , AggregationTemporality (..)
    -- * Aggregation
  , Aggregation (..)
    -- * Exemplar filter
  , ExemplarFilter (..)
    -- * Exemplar
  , Exemplar (..)
    -- * Data point types
  , NumberDataPoint (..)
  , HistogramDataPoint (..)
  , ExponentialHistogramDataPoint (..)
  , ExponentialBuckets (..)
    -- * Metric point data
  , MetricPointData (..)
  , SumData (..)
  , GaugeData (..)
  , HistogramData (..)
  , ExponentialHistogramData (..)
    -- * Metric
  , Metric (..)
  , ScopeMetrics (..)
  , MetricData (..)
    -- * MetricExporter type class
  , MetricExporter (..)
  , SomeMetricExporter (..)
  , NoOpMetricExporter (..)
  ) where

import Data.Text (Text)
import Data.Word (Word64)

import OTel.Attribute (Attributes, InstrumentationScope)
import OTel.SDK.Export (ExportResult (..), FlushError, ShutdownError)
import OTel.SDK.Resource (Resource)
import OTel.Timestamp (Duration, Timestamp)
import OTel.Trace.SpanContext (SpanId, TraceId)


-------------------------------------------------------------------------------
-- Instrument kind
-------------------------------------------------------------------------------

-- | Classification of metric instruments per the OTel Metrics data model.
data InstrumentKind
  = CounterKind
  | UpDownCounterKind
  | HistogramKind
  | GaugeKind
  | ObservableCounterKind
  | ObservableUpDownCounterKind
  | ObservableGaugeKind
  deriving stock (Eq, Ord, Show, Enum, Bounded)


-------------------------------------------------------------------------------
-- Aggregation temporality
-------------------------------------------------------------------------------

-- | Whether metric data points are delta or cumulative.
data AggregationTemporality
  = Delta
  | Cumulative
  deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Aggregation
-------------------------------------------------------------------------------

-- | Aggregation strategy for metric data points.
data Aggregation
  = DropAggregation
  | DefaultAggregation
  | SumAggregation
  | LastValueAggregation
  | ExplicitBucketHistogramAggregation { explicitBucketBounds :: ![Double] }
  | Base2ExponentialBucketHistogramAggregation { expMaxSize :: !Int, expMaxScale :: !Int }
  deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Exemplar filter
-------------------------------------------------------------------------------

-- | Filter determining which measurements get exemplars attached.
data ExemplarFilter
  = AlwaysOnFilter
  | AlwaysOffFilter
  | TraceBasedFilter
  deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Exemplar
-------------------------------------------------------------------------------

-- | A sample measurement stored alongside aggregated data.
data Exemplar = Exemplar
  { exemplarAttributes :: !Attributes
  , exemplarTime       :: !Timestamp
  , exemplarValue      :: !Double
  , exemplarSpanId     :: !(Maybe SpanId)
  , exemplarTraceId    :: !(Maybe TraceId)
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Data point types
-------------------------------------------------------------------------------

-- | A single numeric data point.
data NumberDataPoint = NumberDataPoint
  { ndpAttributes :: !Attributes
  , ndpStartTime  :: !Timestamp
  , ndpTime       :: !Timestamp
  , ndpValue      :: !Double
  , ndpExemplars  :: ![Exemplar]
  } deriving stock (Eq, Show)


-- | A single histogram data point with bucket counts.
data HistogramDataPoint = HistogramDataPoint
  { hdpAttributes     :: !Attributes
  , hdpStartTime      :: !Timestamp
  , hdpTime           :: !Timestamp
  , hdpCount          :: !Word64
  , hdpSum            :: !(Maybe Double)
  , hdpBucketCounts   :: ![Word64]
  , hdpExplicitBounds :: ![Double]
  , hdpMin            :: !(Maybe Double)
  , hdpMax            :: !(Maybe Double)
  , hdpExemplars      :: ![Exemplar]
  } deriving stock (Eq, Show)


-- | Bucket representation for exponential histograms.
data ExponentialBuckets = ExponentialBuckets
  { ebOffset       :: !Int
  , ebBucketCounts :: ![Word64]
  } deriving stock (Eq, Show)


-- | A single exponential histogram data point.
data ExponentialHistogramDataPoint = ExponentialHistogramDataPoint
  { ehdpAttributes    :: !Attributes
  , ehdpStartTime     :: !Timestamp
  , ehdpTime          :: !Timestamp
  , ehdpCount         :: !Word64
  , ehdpSum           :: !(Maybe Double)
  , ehdpScale         :: !Int
  , ehdpZeroCount     :: !Word64
  , ehdpZeroThreshold :: !Double
  , ehdpPositive      :: !ExponentialBuckets
  , ehdpNegative      :: !ExponentialBuckets
  , ehdpMin           :: !(Maybe Double)
  , ehdpMax           :: !(Maybe Double)
  , ehdpExemplars     :: ![Exemplar]
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Metric point data
-------------------------------------------------------------------------------

-- | Sum aggregation data.
data SumData = SumData
  { sumDataPoints  :: ![NumberDataPoint]
  , sumTemporality :: !AggregationTemporality
  , sumIsMonotonic :: !Bool
  } deriving stock (Eq, Show)


-- | Gauge aggregation data.
data GaugeData = GaugeData
  { gaugeDataPoints :: ![NumberDataPoint]
  } deriving stock (Eq, Show)


-- | Histogram aggregation data.
data HistogramData = HistogramData
  { histDataPoints  :: ![HistogramDataPoint]
  , histTemporality :: !AggregationTemporality
  } deriving stock (Eq, Show)


-- | Exponential histogram aggregation data.
data ExponentialHistogramData = ExponentialHistogramData
  { expHistDataPoints  :: ![ExponentialHistogramDataPoint]
  , expHistTemporality :: !AggregationTemporality
  } deriving stock (Eq, Show)


-- | Sum type wrapping all metric point data variants.
data MetricPointData
  = SumPointData SumData
  | GaugePointData GaugeData
  | HistogramPointData HistogramData
  | ExponentialHistogramPointData ExponentialHistogramData
  deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- Metric
-------------------------------------------------------------------------------

-- | A named metric with its aggregated point data.
data Metric = Metric
  { metricName        :: !Text
  , metricDescription :: !Text
  , metricUnit        :: !Text
  , metricPointData   :: !MetricPointData
  } deriving stock (Eq, Show)


-- | Metrics grouped by instrumentation scope.
data ScopeMetrics = ScopeMetrics
  { smScope   :: !InstrumentationScope
  , smMetrics :: ![Metric]
  } deriving stock (Eq, Show)


-- | Top-level metric data container with resource and scoped metrics.
data MetricData = MetricData
  { mdResource     :: !Resource
  , mdScopeMetrics :: ![ScopeMetrics]
  } deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- MetricExporter type class
-------------------------------------------------------------------------------

-- | Interface for exporting collected metrics out of process.
class MetricExporter e where
  -- | Export a batch of metric data. Returns 'ExportSuccess' if accepted.
  exportMetrics :: e -> MetricData -> IO ExportResult

  -- | Shut down the exporter, releasing any held resources.
  shutdownMetricExporter :: e -> IO (Either ShutdownError ())

  -- | Force flush any buffered metric data.
  forceFlushMetricExporter :: e -> Maybe Duration -> IO (Either FlushError ())

  -- | Preferred aggregation temporality for the given instrument kind.
  exporterTemporality :: e -> InstrumentKind -> AggregationTemporality

  -- | Default aggregation for the given instrument kind.
  exporterDefaultAggregation :: e -> InstrumentKind -> Aggregation


-- | Existential wrapper for any 'MetricExporter' implementation.
data SomeMetricExporter = forall e. MetricExporter e => SomeMetricExporter e

instance Show SomeMetricExporter where
  show _ = "SomeMetricExporter"

instance MetricExporter SomeMetricExporter where
  exportMetrics (SomeMetricExporter e) = exportMetrics e
  shutdownMetricExporter (SomeMetricExporter e) = shutdownMetricExporter e
  forceFlushMetricExporter (SomeMetricExporter e) = forceFlushMetricExporter e
  exporterTemporality (SomeMetricExporter e) = exporterTemporality e
  exporterDefaultAggregation (SomeMetricExporter e) = exporterDefaultAggregation e


-- | A no-op exporter that accepts everything and discards it.
data NoOpMetricExporter = NoOpMetricExporter
  deriving stock (Show)

instance MetricExporter NoOpMetricExporter where
  exportMetrics _ _ = pure ExportSuccess
  shutdownMetricExporter _ = pure (Right ())
  forceFlushMetricExporter _ _ = pure (Right ())
  exporterTemporality _ _ = Cumulative
  exporterDefaultAggregation _ _ = DefaultAggregation
