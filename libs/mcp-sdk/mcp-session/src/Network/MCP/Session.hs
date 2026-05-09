-- |
-- Module      : Network.MCP.Session
-- Stability   : stable
--
-- Base session types shared by both client and server sides.
-- The 'Session' type is a record of IO actions rather than a
-- typeclass, so feature modules can hold a @Session@ without
-- carrying a type parameter.
module Network.MCP.Session
  ( -- * Session interface
    Session (..)

    -- * Request metadata
  , RequestMeta (..)
  , RequestOptions (..)

    -- * Handler types
  , RequestHandler
  , NotificationHandler

    -- * Close and error types
  , CloseReason (..)
  , SessionError (..)
  , SessionErrorKind (..)
  ) where

import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)

import Network.MCP.Types
  ( Implementation
  , ProgressToken
  , ProtocolVersion
  , RPCError
  , RequestId
  )
import Network.MCP.Types.Capabilities (NegotiatedCapabilities)

------------------------------------------------------------------------
-- Handler types
------------------------------------------------------------------------

-- | Handler for incoming JSON-RPC requests. Receives the params
-- value and request metadata; returns either an RPC error or a
-- result value.
type RequestHandler = Value -> RequestMeta -> IO (Either RPCError Value)

-- | Handler for incoming JSON-RPC notifications.
type NotificationHandler = Value -> IO ()

------------------------------------------------------------------------
-- Request metadata
------------------------------------------------------------------------

-- | Metadata extracted from an incoming request's @_meta@ field.
data RequestMeta = RequestMeta
  { requestMetaId :: !RequestId
  , requestMetaProgressToken :: !(Maybe ProgressToken)
  , requestMetaRelatedTask :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

-- | Options for outgoing requests.
data RequestOptions = RequestOptions
  { requestTimeoutMs :: !(Maybe Word)
  , requestProgressToken :: !(Maybe ProgressToken)
  , requestOnProgress :: !(Maybe (Double -> Maybe Double -> Maybe Text -> IO ()))
  }

------------------------------------------------------------------------
-- Close and error types
------------------------------------------------------------------------

-- | Reason a session was closed.
data CloseReason
  = LocalClose
  | RemoteClose
  | TransportError
  | SessionTimeout
  deriving stock (Eq, Show, Bounded, Enum, Generic)

-- | Classification of session errors.
data SessionErrorKind
  = VersionMismatch
  | CapabilityRequired
  | AlreadyConnected
  | NotConnected
  | SessionTimedOut
  | SessionTransportError
  deriving stock (Eq, Show, Bounded, Enum, Generic)

-- | A session error with kind and human-readable detail.
data SessionError = SessionError
  { sessionErrorKind :: !SessionErrorKind
  , sessionErrorDetail :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance Exception SessionError

------------------------------------------------------------------------
-- Session record
------------------------------------------------------------------------

-- | The base session interface — a record of functions rather than
-- a typeclass, so feature modules can hold a @Session@ without a
-- type parameter.
data Session = Session
  { sessionProtocolVersion :: !ProtocolVersion
  , sessionPeerInfo :: !Implementation
  , sessionCapabilities :: !NegotiatedCapabilities
  , sessionRequest :: Text -> Maybe Value -> Maybe RequestOptions -> IO (Either RPCError Value)
  , sessionNotify :: Text -> Maybe Value -> IO ()
  , sessionCancel :: RequestId -> Maybe Text -> IO ()
  , sessionOnRequest :: Text -> RequestHandler -> IO ()
  , sessionOnNotification :: Text -> NotificationHandler -> IO ()
  , sessionClose :: IO ()
  , sessionOnClose :: (CloseReason -> IO ()) -> IO ()
  }
