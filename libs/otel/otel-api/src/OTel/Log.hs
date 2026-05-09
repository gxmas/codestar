-- | Core logs API: Logger and LoggerProvider type classes with existential
-- wrappers, no-op implementations, and global registration.
module OTel.Log
  ( -- * Severity
    SeverityNumber (..)
  , severityNumberValue

    -- * Log body
  , LogBody (..)

    -- * Log record
  , LogRecord (..)
  , defaultLogRecord

    -- * Logger
  , Logger (..)
  , NoOpLogger (..)
  , SomeLogger (..)

    -- * LoggerProvider
  , LoggerProvider (..)
  , NoOpLoggerProvider (..)
  , SomeLoggerProvider (..)

    -- * Global registration
  , setGlobalLoggerProvider
  , getGlobalLoggerProvider
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import OTel.Attribute (Attributes, InstrumentationScope, emptyAttributes)
import OTel.Context (Context)
import OTel.Timestamp (Timestamp)


-------------------------------------------------------------------------------
-- SeverityNumber
-------------------------------------------------------------------------------

-- | Severity level for log records, per the OTel Logs data model.
data SeverityNumber
  = SeverityTrace  | SeverityTrace2  | SeverityTrace3  | SeverityTrace4
  | SeverityDebug  | SeverityDebug2  | SeverityDebug3  | SeverityDebug4
  | SeverityInfo   | SeverityInfo2   | SeverityInfo3   | SeverityInfo4
  | SeverityWarn   | SeverityWarn2   | SeverityWarn3   | SeverityWarn4
  | SeverityError  | SeverityError2  | SeverityError3  | SeverityError4
  | SeverityFatal  | SeverityFatal2  | SeverityFatal3  | SeverityFatal4
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Convert a 'SeverityNumber' to its numeric value (1-24).
severityNumberValue :: SeverityNumber -> Int
severityNumberValue s = fromEnum s + 1


-------------------------------------------------------------------------------
-- LogBody
-------------------------------------------------------------------------------

-- | Structured log body value, per the OTel Logs data model.
data LogBody
  = LogBodyString  !Text
  | LogBodyBool    !Bool
  | LogBodyInt64   !Int64
  | LogBodyFloat64 !Double
  | LogBodyBytes   !ByteString
  | LogBodyList    ![LogBody]
  | LogBodyMap     !(Map Text LogBody)
  deriving stock (Eq, Show)


-------------------------------------------------------------------------------
-- LogRecord
-------------------------------------------------------------------------------

-- | A single log record with optional fields per the OTel data model.
data LogRecord = LogRecord
  { logTimestamp         :: !(Maybe Timestamp)
  , logObservedTimestamp :: !(Maybe Timestamp)
  , logContext           :: !(Maybe Context)
  , logSeverityNumber    :: !(Maybe SeverityNumber)
  , logSeverityText      :: !(Maybe Text)
  , logBody              :: !(Maybe LogBody)
  , logAttributes        :: !Attributes
  }

-- | An empty log record with all optional fields set to 'Nothing'.
defaultLogRecord :: LogRecord
defaultLogRecord = LogRecord
  { logTimestamp         = Nothing
  , logObservedTimestamp = Nothing
  , logContext           = Nothing
  , logSeverityNumber    = Nothing
  , logSeverityText      = Nothing
  , logBody              = Nothing
  , logAttributes        = emptyAttributes
  }


-------------------------------------------------------------------------------
-- Logger type class
-------------------------------------------------------------------------------

-- | Bridge API for emitting log records into the OTel pipeline.
-- Application code typically does not call this directly — logging
-- frameworks bridge into OTel via this interface.
class Logger l where
  emit      :: l -> LogRecord -> IO ()
  isEnabled :: l -> SeverityNumber -> Maybe Text -> Maybe Context -> IO Bool


-- | Existential wrapper for any 'Logger'.
data SomeLogger = forall l. Logger l => SomeLogger l

instance Logger SomeLogger where
  emit      (SomeLogger l) = emit l
  isEnabled (SomeLogger l) = isEnabled l


-- | No-op logger that discards all log records.
data NoOpLogger = NoOpLogger

instance Logger NoOpLogger where
  emit      _ _     = pure ()
  isEnabled _ _ _ _ = pure False


-------------------------------------------------------------------------------
-- LoggerProvider type class
-------------------------------------------------------------------------------

-- | A factory for 'Logger' instances.
class LoggerProvider p where
  getLogger :: p -> InstrumentationScope -> IO SomeLogger


-- | Existential wrapper for any 'LoggerProvider'.
data SomeLoggerProvider = forall p. LoggerProvider p => SomeLoggerProvider p

instance LoggerProvider SomeLoggerProvider where
  getLogger (SomeLoggerProvider p) = getLogger p


-- | No-op logger provider that always returns 'NoOpLogger'.
data NoOpLoggerProvider = NoOpLoggerProvider

instance LoggerProvider NoOpLoggerProvider where
  getLogger _ _ = pure (SomeLogger NoOpLogger)


-------------------------------------------------------------------------------
-- Global registration
-------------------------------------------------------------------------------

globalLoggerProviderRef :: IORef SomeLoggerProvider
globalLoggerProviderRef = unsafePerformIO (newIORef (SomeLoggerProvider NoOpLoggerProvider))
{-# NOINLINE globalLoggerProviderRef #-}

-- | Set the global logger provider.
setGlobalLoggerProvider :: SomeLoggerProvider -> IO ()
setGlobalLoggerProvider = writeIORef globalLoggerProviderRef

-- | Get the global logger provider.
getGlobalLoggerProvider :: IO SomeLoggerProvider
getGlobalLoggerProvider = readIORef globalLoggerProviderRef
