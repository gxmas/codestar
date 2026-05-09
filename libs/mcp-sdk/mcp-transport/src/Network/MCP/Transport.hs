-- |
-- Module      : Network.MCP.Transport
-- Stability   : stable
--
-- Abstract bidirectional message channel for MCP. Concrete transport
-- implementations (stdio, HTTP+SSE, etc.) implement the 'Transport'
-- typeclass. The session layer depends only on this abstraction.
module Network.MCP.Transport
  ( -- * Transport interface
    Transport (..)

    -- * Error types
  , TransportError (..)
  , TransportErrorKind (..)
  ) where

import Data.Text (Text)
import Streaming (Stream, Of)

import Network.MCP.Types (MCPMessage)

-- | Classification of transport-level errors.
data TransportErrorKind
  = TransportIoError
  | TransportClosed
  | TransportSessionExpired
  | TransportProtocolError
  deriving stock (Eq, Show, Bounded, Enum)

-- | A transport error with kind and human-readable detail.
data TransportError = TransportError
  { transportErrorKind :: !TransportErrorKind
  , transportErrorDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | Abstract bidirectional async message channel. All concrete
-- transports implement this typeclass. The session layer depends
-- only on this abstraction.
--
-- Concrete transports carry their own state (handles, queues, STM
-- vars) as fields of the implementing type. The typeclass has no
-- state of its own.
class Transport t where
  -- | Send a single MCP message. Returns 'Left' on unrecoverable
  -- send failure (e.g. transport already closed or I/O error).
  -- Concrete transports encode via 'McpCodec' internally.
  send :: t -> MCPMessage -> IO (Either TransportError ())

  -- | Pull-based receive: returns a stream of incoming messages.
  -- Each item is 'Right' for a successfully received message or
  -- 'Left' for a mid-stream transport error. The stream terminates
  -- when the transport is closed.
  messages :: t -> IO (Stream (Of (Either TransportError MCPMessage)) IO ())

  -- | Close the transport, releasing resources. Idempotent: calling
  -- 'close' on an already-closed transport is a no-op.
  close :: t -> IO ()
