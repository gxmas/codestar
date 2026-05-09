{-# LANGUAGE ExistentialQuantification #-}
-- | OpenTelemetry Logs SDK: LoggerProvider, Logger, log record limits,
-- and internal concrete log record type.
module OTel.SDK.Log
  ( -- * LogRecordLimits
    LogRecordLimits (..)
  , defaultLogRecordLimits

    -- * SdkLoggerProvider
  , SdkLoggerProvider
  , SdkLoggerProviderConfig (..)
  , defaultSdkLoggerProviderConfig
  , newSdkLoggerProvider
  , sdkLoggerProviderShutdown
  , sdkLoggerProviderForceFlush

    -- * Re-exports
  , module OTel.SDK.Log.Export
  , module OTel.SDK.Log.Processor
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, swapTVar, writeTVar)
import Control.Monad (forM_, unless)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO.Unsafe (unsafePerformIO)

import OTel.Attribute (AttributeValue (..), Attributes (..), InstrumentationScope)
import OTel.Attribute qualified as Attribute
import OTel.Context (Context, getCurrent)
import OTel.Log
  ( LogBody, LogRecord (..), Logger (..), LoggerProvider (..)
  , NoOpLogger (..), SeverityNumber, SomeLogger (..)
  )
import OTel.SDK.Export (FlushError, ShutdownError)
import OTel.SDK.Log.Export
import OTel.SDK.Log.Processor
import OTel.SDK.Resource (Resource)
import OTel.SDK.Resource qualified as Resource
import OTel.Timestamp (Duration, Timestamp)
import OTel.Timestamp qualified as Timestamp
import OTel.Trace (Span (..), SomeSpan (..), getSpanFromContext)
import OTel.Trace.SpanContext (SpanContext, isValid)


-------------------------------------------------------------------------------
-- LogRecordLimits
-------------------------------------------------------------------------------

-- | Limits applied to log records during creation. Controls maximum number
-- of attributes and optional truncation of string attribute values.
data LogRecordLimits = LogRecordLimits
  { lrlMaxAttributes        :: !Int
    -- ^ Maximum number of attributes per log record. Default: 128.
  , lrlMaxAttributeValueLen :: !(Maybe Int)
    -- ^ Maximum length for string attribute values. 'Nothing' means unlimited.
    -- Default: 'Nothing'.
  } deriving stock (Eq, Show)


-- | Spec-mandated defaults for log record limits.
defaultLogRecordLimits :: LogRecordLimits
defaultLogRecordLimits = LogRecordLimits
  { lrlMaxAttributes        = 128
  , lrlMaxAttributeValueLen = Nothing
  }


-------------------------------------------------------------------------------
-- SdkLogRecord (internal mutable log record)
-------------------------------------------------------------------------------

-- | Internal mutable log record. TVars allow processors to modify fields
-- during 'onEmit'. The 'ReadableLogRecord' instance uses 'unsafePerformIO'
-- to read TVars; this is safe because by the time exporters access the
-- record, all processor mutations are complete.
data SdkLogRecord = SdkLogRecord
  { _slrTimestamp         :: !(TVar (Maybe Timestamp))
  , _slrObservedTimestamp :: !(TVar Timestamp)
  , _slrSeverityNumber    :: !(TVar (Maybe SeverityNumber))
  , _slrSeverityText      :: !(TVar (Maybe Text))
  , _slrBody              :: !(TVar (Maybe LogBody))
  , _slrAttributes        :: !(TVar Attributes)
  , _slrDroppedAttributes :: !(TVar Int)
  , _slrSpanContext       :: !(Maybe SpanContext)
  , _slrResource          :: !Resource
  , _slrScope             :: !InstrumentationScope
  , _slrLimits            :: !LogRecordLimits
  }


instance ReadableLogRecord SdkLogRecord where
  rlrTimestamp         r = unsafePerformIO (readTVarIO r._slrTimestamp)
  rlrObservedTimestamp r = unsafePerformIO (readTVarIO r._slrObservedTimestamp)
  rlrSeverityNumber    r = unsafePerformIO (readTVarIO r._slrSeverityNumber)
  rlrSeverityText      r = unsafePerformIO (readTVarIO r._slrSeverityText)
  rlrBody              r = unsafePerformIO (readTVarIO r._slrBody)
  rlrAttributes        r = unsafePerformIO (readTVarIO r._slrAttributes)
  rlrDroppedAttributes r = unsafePerformIO (readTVarIO r._slrDroppedAttributes)
  rlrSpanContext       r = r._slrSpanContext
  rlrResource          r = r._slrResource
  rlrScope             r = r._slrScope


instance ReadWriteLogRecord SdkLogRecord where
  rwlrSetTimestamp      r ts = atomically $ writeTVar r._slrTimestamp (Just ts)
  rwlrSetObservedTime   r ts = atomically $ writeTVar r._slrObservedTimestamp ts
  rwlrSetSeverityNumber r sn = atomically $ writeTVar r._slrSeverityNumber (Just sn)
  rwlrSetSeverityText   r st = atomically $ writeTVar r._slrSeverityText (Just st)
  rwlrSetBody           r b  = atomically $ writeTVar r._slrBody (Just b)
  rwlrSetAttribute      r k v = atomically $ do
    attrs <- readTVar r._slrAttributes
    let currentSize = Attribute.size attrs
        maxAttrs    = lrlMaxAttributes r._slrLimits
    if currentSize >= maxAttrs
      then do
        dropped <- readTVar r._slrDroppedAttributes
        writeTVar r._slrDroppedAttributes (dropped + 1)
      else
        writeTVar r._slrAttributes (Attribute.insert k v attrs)


-- | Create an 'SdkLogRecord' from an API 'LogRecord'. Fills in the
-- observed timestamp if not provided, extracts span context for trace
-- correlation, and applies attribute limits.
mkSdkLogRecord
  :: LogRecordLimits
  -> Resource
  -> InstrumentationScope
  -> LogRecord
  -> IO SdkLogRecord
mkSdkLogRecord limits res scope lr = do
  observedTs <- case logObservedTimestamp lr of
    Just t  -> pure t
    Nothing -> Timestamp.now
  mSpanCtx <- case logContext lr of
    Nothing  -> pure Nothing
    Just ctx -> extractSpanContext ctx
  let (attrs, dropped) = applyAttrLimits limits (logAttributes lr)
  SdkLogRecord
    <$> newTVarIO (logTimestamp lr)
    <*> newTVarIO observedTs
    <*> newTVarIO (logSeverityNumber lr)
    <*> newTVarIO (logSeverityText lr)
    <*> newTVarIO (logBody lr)
    <*> newTVarIO attrs
    <*> newTVarIO dropped
    <*> pure mSpanCtx
    <*> pure res
    <*> pure scope
    <*> pure limits


-- | Extract a 'SpanContext' from a 'Context' for trace correlation.
-- Returns the span context if the span is valid; per spec, even unsampled
-- spans get their IDs attached to logs.
extractSpanContext :: Context -> IO (Maybe SpanContext)
extractSpanContext ctx =
  case getSpanFromContext ctx of
    Nothing -> pure Nothing
    Just (SomeSpan span_) -> do
      sc <- getSpanContext span_
      pure $ if isValid sc then Just sc else Nothing


-- | Apply attribute limits: truncate string values and drop excess attributes.
applyAttrLimits :: LogRecordLimits -> Attributes -> (Attributes, Int)
applyAttrLimits limits (Attributes m) =
  let pairs     = Map.toList m
      maxLen    = lrlMaxAttributeValueLen limits
      maxN      = lrlMaxAttributes limits
      truncated = map (\(k, v) -> (k, truncateAttrVal maxLen v)) pairs
      (kept, excess) = splitAt maxN truncated
  in (Attributes (Map.fromList kept), length excess)


-- | Truncate string attribute values to the given maximum length.
-- Non-string values are passed through unchanged.
truncateAttrVal :: Maybe Int -> AttributeValue -> AttributeValue
truncateAttrVal Nothing v = v
truncateAttrVal (Just n) v = case v of
  StringValue t        -> StringValue (Text.take n t)
  StringArrayValue arr -> StringArrayValue (fmap (Text.take n) arr)
  _                    -> v


-------------------------------------------------------------------------------
-- SdkLogger
-------------------------------------------------------------------------------

-- | An SDK-backed logger that creates mutable log records, runs them
-- through processors, and delegates to the configured exporter chain.
data SdkLogger = SdkLogger
  { _sdkLoggerScope      :: !InstrumentationScope
  , _sdkLoggerResource   :: !Resource
  , _sdkLoggerLimits     :: !LogRecordLimits
  , _sdkLoggerProcessors :: ![SomeLogRecordProcessor]
  , _sdkLoggerShutdown   :: !(TVar Bool)
  }


instance Logger SdkLogger where
  emit logger lr = do
    isShutdown <- readTVarIO logger._sdkLoggerShutdown
    unless isShutdown $ do
      sdkLr <- mkSdkLogRecord
        logger._sdkLoggerLimits
        logger._sdkLoggerResource
        logger._sdkLoggerScope
        lr
      let rwRecord = SomeReadWriteLogRecord sdkLr
      ctx <- maybe getCurrent pure (logContext lr)
      forM_ logger._sdkLoggerProcessors $ \p ->
        onEmit p rwRecord ctx

  isEnabled _ _ _ _ = pure True


-------------------------------------------------------------------------------
-- SdkLoggerProvider
-------------------------------------------------------------------------------

-- | Configuration for creating an 'SdkLoggerProvider'.
data SdkLoggerProviderConfig = SdkLoggerProviderConfig
  { llpResource   :: !Resource
  , llpProcessors :: ![SomeLogRecordProcessor]
  , llpLimits     :: !LogRecordLimits
  }


-- | Sensible defaults: empty resource, no processors, default limits.
defaultSdkLoggerProviderConfig :: SdkLoggerProviderConfig
defaultSdkLoggerProviderConfig = SdkLoggerProviderConfig
  { llpResource   = Resource.empty
  , llpProcessors = []
  , llpLimits     = defaultLogRecordLimits
  }


-- | An SDK-backed logger provider that creates 'SdkLogger' instances
-- configured with the provider's resource, processors, and limits.
data SdkLoggerProvider = SdkLoggerProvider
  { _sdkLLPResource   :: !Resource
  , _sdkLLPProcessors :: ![SomeLogRecordProcessor]
  , _sdkLLPLimits     :: !LogRecordLimits
  , _sdkLLPShutdown   :: !(TVar Bool)
  }


-- | Create a new 'SdkLoggerProvider' from the given configuration.
newSdkLoggerProvider :: SdkLoggerProviderConfig -> IO SdkLoggerProvider
newSdkLoggerProvider config = do
  shutdownVar <- newTVarIO False
  pure SdkLoggerProvider
    { _sdkLLPResource   = llpResource config
    , _sdkLLPProcessors = llpProcessors config
    , _sdkLLPLimits     = llpLimits config
    , _sdkLLPShutdown   = shutdownVar
    }


instance LoggerProvider SdkLoggerProvider where
  getLogger p scope = do
    isShutdown <- readTVarIO p._sdkLLPShutdown
    if isShutdown
      then pure (SomeLogger NoOpLogger)
      else pure $ SomeLogger SdkLogger
        { _sdkLoggerScope      = scope
        , _sdkLoggerResource   = p._sdkLLPResource
        , _sdkLoggerLimits     = p._sdkLLPLimits
        , _sdkLoggerProcessors = p._sdkLLPProcessors
        , _sdkLoggerShutdown   = p._sdkLLPShutdown
        }


-- | Shut down the logger provider. Shuts down all registered processors.
-- After shutdown, 'getLogger' returns no-op loggers.
sdkLoggerProviderShutdown :: SdkLoggerProvider -> IO (Either ShutdownError ())
sdkLoggerProviderShutdown p = do
  alreadyDown <- atomically $ swapTVar p._sdkLLPShutdown True
  if alreadyDown
    then pure (Right ())
    else do
      results <- mapM shutdownProcessor p._sdkLLPProcessors
      -- Return the first error, if any
      pure $ case filter isLeft results of
        (Left err : _) -> Left err
        _              -> Right ()
  where
    isLeft (Left _) = True
    isLeft _        = False


-- | Force-flush all registered processors.
sdkLoggerProviderForceFlush
  :: SdkLoggerProvider -> Maybe Duration -> IO (Either FlushError ())
sdkLoggerProviderForceFlush p mtimeout = do
  isDown <- readTVarIO p._sdkLLPShutdown
  if isDown
    then pure (Right ())
    else do
      results <- mapM (\proc -> forceFlushProcessor proc mtimeout) p._sdkLLPProcessors
      pure $ case filter isLeft results of
        (Left err : _) -> Left err
        _              -> Right ()
  where
    isLeft (Left _) = True
    isLeft _        = False
