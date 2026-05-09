-- | Common export result and error types shared by all signal pipelines.
module OTel.SDK.Export
  ( ExportResult (..)
  , ShutdownError (..)
  , FlushError (..)
  ) where

import Control.Exception (SomeException)
import Data.Text (Text)


-- | Result of an export operation. Exporters return this to indicate whether
-- the batch was accepted ('ExportSuccess') or rejected ('ExportFailure').
data ExportResult
  = ExportSuccess
  | ExportFailure
  deriving stock (Eq, Show)


-- | Error produced when a component fails to shut down cleanly.
data ShutdownError = ShutdownError
  { shutdownComponent :: !Text
  , shutdownCause :: !SomeException
  } deriving stock (Show)


-- | Error produced when a flush operation fails or times out.
data FlushError = FlushError
  { flushComponent :: !Text
  , flushTimedOut :: !Bool
  , flushCause :: !(Maybe SomeException)
  } deriving stock (Show)
