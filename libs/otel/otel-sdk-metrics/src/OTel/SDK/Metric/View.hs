{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Metric Views: selection criteria and output configuration for metric
-- streams, per the OpenTelemetry SDK specification.
module OTel.SDK.Metric.View
  ( -- * View
    View (..)
  , defaultView
    -- * Matching
  , matchesInstrument
    -- * Application
  , applyView
  ) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)

import OTel.Attribute (Attributes (..), AttributeValue (..), InstrumentationScope (..), Key)
import OTel.SDK.Metric.Export


-------------------------------------------------------------------------------
-- Orphan Ord instances needed for Map keys
-------------------------------------------------------------------------------

deriving stock instance Ord AttributeValue
deriving newtype instance Ord Attributes
deriving stock instance Ord InstrumentationScope


-- | A metric view that selects instruments and configures their output stream.
data View = View
  { -- Selection criteria
    viewInstrumentName   :: !(Maybe Text)          -- ^ Nothing = match all; supports '*' wildcard
  , viewInstrumentKind   :: !(Maybe InstrumentKind)
  , viewMeterName        :: !(Maybe Text)
  , viewMeterVersion     :: !(Maybe Text)
  , viewMeterSchemaUrl   :: !(Maybe Text)
    -- Output configuration
  , viewName             :: !(Maybe Text)           -- ^ Rename metric stream
  , viewDescription      :: !(Maybe Text)           -- ^ Override description
  , viewAttributeKeys    :: !(Maybe (Set Key))      -- ^ Keep only these keys (Nothing = keep all)
  , viewAggregation      :: !(Maybe Aggregation)    -- ^ Override aggregation
  , viewExemplarFilter   :: !(Maybe ExemplarFilter)
  , viewCardinalityLimit :: !(Maybe Int)            -- ^ Cap data points per stream
  } deriving stock (Eq, Show)


-- | A view with no selection criteria (matches everything) and no overrides.
defaultView :: View
defaultView = View Nothing Nothing Nothing Nothing Nothing
                   Nothing Nothing Nothing Nothing Nothing Nothing


-------------------------------------------------------------------------------
-- Matching
-------------------------------------------------------------------------------

-- | Returns True if ALL specified selection criteria in the view match
-- the given instrument.
matchesInstrument
  :: View
  -> InstrumentationScope  -- ^ The meter's scope
  -> InstrumentKind
  -> Text                  -- ^ Instrument name
  -> Bool
matchesInstrument v scope kind instName =
     nameMatches (viewInstrumentName v) instName
  && maybe True (== kind) (viewInstrumentKind v)
  && maybe True (== scope.scopeName) (viewMeterName v)
  && maybe True (\ver -> scope.scopeVersion == Just ver) (viewMeterVersion v)
  && maybe True (\url -> scope.scopeSchemaUrl == Just url) (viewMeterSchemaUrl v)


-- | Glob-style wildcard matching for instrument names.
-- Supports '*' as a wildcard that matches any sequence of characters.
-- A pattern of Nothing matches everything.
nameMatches :: Maybe Text -> Text -> Bool
nameMatches Nothing    _    = True
nameMatches (Just pat) instName = globMatch pat instName


globMatch :: Text -> Text -> Bool
globMatch pat instName = go (Text.unpack pat) (Text.unpack instName)
  where
    go []       []       = True
    go ('*':ps) ns       = go ps ns || (not (null ns) && go ('*':ps) (drop 1 ns))
    go (p:ps)   (n:ns)   = p == n && go ps ns
    go _        _        = False


-------------------------------------------------------------------------------
-- Application
-------------------------------------------------------------------------------

-- | Apply a view's output configuration to a collected Metric.
-- Returns Nothing if the view's aggregation is DropAggregation.
applyView :: View -> Metric -> Maybe Metric
applyView v metric =
  case viewAggregation v of
    Just DropAggregation -> Nothing
    Just aggOverride ->
      Just . applyCardinalityLimit_ v
           . applyAttributeFilter_ v
           . applyAggregationOverride aggOverride
           . applyRename_ v
           $ metric
    Nothing ->
      Just . applyCardinalityLimit_ v
           . applyAttributeFilter_ v
           . applyRename_ v
           $ metric


applyRename_ :: View -> Metric -> Metric
applyRename_ v m = m
  { metricName        = fromMaybe m.metricName (viewName v)
  , metricDescription = fromMaybe m.metricDescription (viewDescription v)
  }


applyAttributeFilter_ :: View -> Metric -> Metric
applyAttributeFilter_ v m = case viewAttributeKeys v of
  Nothing   -> m
  Just keys -> m { metricPointData = filterPointData keys m.metricPointData }


filterPointData :: Set Key -> MetricPointData -> MetricPointData
filterPointData keys pd = case pd of
  SumPointData sd ->
    SumPointData sd { sumDataPoints = mergeByAttrs (+) (map (filterNDP keys) sd.sumDataPoints) }
  GaugePointData gd ->
    GaugePointData gd { gaugeDataPoints = mergeByAttrsLast (map (filterNDP keys) gd.gaugeDataPoints) }
  HistogramPointData hd ->
    HistogramPointData hd { histDataPoints = mergeByAttrsHist (map (filterHDP keys) hd.histDataPoints) }
  ExponentialHistogramPointData ehd ->
    ExponentialHistogramPointData ehd { expHistDataPoints = mergeByAttrsExpHist (map (filterEHDP keys) ehd.expHistDataPoints) }


filterNDP :: Set Key -> NumberDataPoint -> NumberDataPoint
filterNDP keys dp = dp { ndpAttributes = keepKeys keys dp.ndpAttributes }


filterHDP :: Set Key -> HistogramDataPoint -> HistogramDataPoint
filterHDP keys dp = dp { hdpAttributes = keepKeys keys dp.hdpAttributes }


filterEHDP :: Set Key -> ExponentialHistogramDataPoint -> ExponentialHistogramDataPoint
filterEHDP keys dp = dp { ehdpAttributes = keepKeys keys dp.ehdpAttributes }


keepKeys :: Set Key -> Attributes -> Attributes
keepKeys keys (Attributes m) = Attributes (Map.filterWithKey (\k _ -> Set.member k keys) m)


-- | Merge NumberDataPoints with the same attributes by applying the combining
-- function to their values. This is needed when attribute filtering collapses
-- multiple data points to the same attribute set.
mergeByAttrs :: (Double -> Double -> Double) -> [NumberDataPoint] -> [NumberDataPoint]
mergeByAttrs combine dps =
  Map.elems $
    Map.fromListWith (\a b -> a { ndpValue = combine a.ndpValue b.ndpValue })
      [(dp.ndpAttributes, dp) | dp <- dps]


-- | For gauge (last-value semantics): keep the data point with the latest time.
mergeByAttrsLast :: [NumberDataPoint] -> [NumberDataPoint]
mergeByAttrsLast dps =
  Map.elems $
    Map.fromListWith (\a b -> if a.ndpTime >= b.ndpTime then a else b)
      [(dp.ndpAttributes, dp) | dp <- dps]


-- | Merge HistogramDataPoints with identical attributes (after filtering).
-- Sums counts, sums hdpSum, element-wise sums bucket counts, takes min/max.
mergeByAttrsHist :: [HistogramDataPoint] -> [HistogramDataPoint]
mergeByAttrsHist dps =
  Map.elems $
    Map.fromListWith mergeHDP
      [(dp.hdpAttributes, dp) | dp <- dps]
  where
    mergeHDP a b = a
      { hdpCount        = a.hdpCount + b.hdpCount
      , hdpSum          = (+) <$> a.hdpSum <*> b.hdpSum <|> a.hdpSum <|> b.hdpSum
      , hdpBucketCounts = zipWithPad (+) a.hdpBucketCounts b.hdpBucketCounts
      , hdpMin          = minMaybe a.hdpMin b.hdpMin
      , hdpMax          = maxMaybe a.hdpMax b.hdpMax
      , hdpStartTime    = min a.hdpStartTime b.hdpStartTime
      , hdpTime         = max a.hdpTime b.hdpTime
      , hdpExemplars    = a.hdpExemplars ++ b.hdpExemplars
      }
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x       <|> _ = x
    zipWithPad :: (Word64 -> Word64 -> Word64) -> [Word64] -> [Word64] -> [Word64]
    zipWithPad f xs ys = go xs ys
      where
        go []     []     = []
        go (x:xs') (y:ys') = f x y : go xs' ys'
        go xs'    []     = xs'
        go []     ys'    = ys'
    minMaybe :: Maybe Double -> Maybe Double -> Maybe Double
    minMaybe (Just a) (Just b) = Just (min a b)
    minMaybe a        b        = a <|> b
    maxMaybe :: Maybe Double -> Maybe Double -> Maybe Double
    maxMaybe (Just a) (Just b) = Just (max a b)
    maxMaybe a        b        = a <|> b


-- | Merge ExponentialHistogramDataPoints with identical attributes.
-- Assumes same scale; sums counts and bucket entries element-wise.
mergeByAttrsExpHist :: [ExponentialHistogramDataPoint] -> [ExponentialHistogramDataPoint]
mergeByAttrsExpHist dps =
  Map.elems $
    Map.fromListWith mergeEHDP
      [(dp.ehdpAttributes, dp) | dp <- dps]
  where
    mergeEHDP a b = a
      { ehdpCount      = a.ehdpCount + b.ehdpCount
      , ehdpSum        = (+) <$> a.ehdpSum <*> b.ehdpSum <|> a.ehdpSum <|> b.ehdpSum
      , ehdpZeroCount  = a.ehdpZeroCount + b.ehdpZeroCount
      , ehdpPositive   = mergeBuckets a.ehdpPositive b.ehdpPositive
      , ehdpNegative   = mergeBuckets a.ehdpNegative b.ehdpNegative
      , ehdpMin        = minMaybe a.ehdpMin b.ehdpMin
      , ehdpMax        = maxMaybe a.ehdpMax b.ehdpMax
      , ehdpStartTime  = min a.ehdpStartTime b.ehdpStartTime
      , ehdpTime       = max a.ehdpTime b.ehdpTime
      , ehdpExemplars  = a.ehdpExemplars ++ b.ehdpExemplars
      }
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x       <|> _ = x
    mergeBuckets :: ExponentialBuckets -> ExponentialBuckets -> ExponentialBuckets
    mergeBuckets p q =
      let counts = foldl' (\m (i, c) -> Map.insertWith (+) i c m)
                     Map.empty
                     (zip [p.ebOffset ..] p.ebBucketCounts
                      ++ zip [q.ebOffset ..] q.ebBucketCounts)
          offset  = if Map.null counts then 0 else fst (Map.findMin counts)
          buckets = Map.elems counts
      in ExponentialBuckets { ebOffset = offset, ebBucketCounts = buckets }
    minMaybe :: Maybe Double -> Maybe Double -> Maybe Double
    minMaybe (Just a) (Just b) = Just (min a b)
    minMaybe a        b        = a <|> b
    maxMaybe :: Maybe Double -> Maybe Double -> Maybe Double
    maxMaybe (Just a) (Just b) = Just (max a b)
    maxMaybe a        b        = a <|> b


applyCardinalityLimit_ :: View -> Metric -> Metric
applyCardinalityLimit_ v m = case viewCardinalityLimit v of
  Nothing    -> m
  Just limit -> m { metricPointData = limitPointData limit m.metricPointData }


limitPointData :: Int -> MetricPointData -> MetricPointData
limitPointData n pd = case pd of
  SumPointData sd           -> SumPointData sd { sumDataPoints = take n sd.sumDataPoints }
  GaugePointData gd         -> GaugePointData gd { gaugeDataPoints = take n gd.gaugeDataPoints }
  HistogramPointData hd     -> HistogramPointData hd { histDataPoints = take n hd.histDataPoints }
  ExponentialHistogramPointData ehd ->
    ExponentialHistogramPointData ehd { expHistDataPoints = take n ehd.expHistDataPoints }


-- | Re-aggregate a Metric using a different aggregation type.
-- Supports: SumAggregation override on Histogram -> converts to Sum data.
-- Other overrides that match existing data type are no-ops.
applyAggregationOverride :: Aggregation -> Metric -> Metric
applyAggregationOverride agg m = case (agg, m.metricPointData) of
  (SumAggregation, HistogramPointData hd) ->
    m { metricPointData = SumPointData SumData
          { sumDataPoints  = map histToNumber hd.histDataPoints
          , sumTemporality = hd.histTemporality
          , sumIsMonotonic = False
          }
      }
  (LastValueAggregation, SumPointData sd) ->
    m { metricPointData = GaugePointData GaugeData
          { gaugeDataPoints = sd.sumDataPoints }
      }
  -- Already the right type, or unsupported override: leave unchanged
  _ -> m


histToNumber :: HistogramDataPoint -> NumberDataPoint
histToNumber hdp = NumberDataPoint
  { ndpAttributes = hdp.hdpAttributes
  , ndpStartTime  = hdp.hdpStartTime
  , ndpTime       = hdp.hdpTime
  , ndpValue      = fromMaybe 0.0 hdp.hdpSum
  , ndpExemplars  = hdp.hdpExemplars
  }
