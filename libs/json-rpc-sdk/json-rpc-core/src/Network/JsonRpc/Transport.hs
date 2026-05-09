{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.JsonRpc.Transport
-- Stability   : stable
-- Portability : GHC2021
--
-- Transport interface for JSON-RPC 2.0. Concrete transport
-- implementations (WebSocket, HTTP, stdio, etc.) implement the
-- 'Transport' typeclass.
--
-- Callback type aliases are provided for documentation; concrete
-- transports accept these in their constructors, not in the typeclass.
--
-- @
-- data StdioTransport = StdioTransport
--
-- instance Transport StdioTransport where
--   send _ bytes = do
--     LBS.putStr bytes
--     pure (Right ())
--   close _ = pure ()
-- @
module Network.JsonRpc.Transport
  ( -- * Transport interface
    Transport (..)

    -- * Error type
  , TransportError (..)

    -- * Callback type aliases
  , OnMessage
  , OnError
  , OnClose
  ) where

import Control.Exception (Exception, SomeException)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

-- | Typeclass for JSON-RPC message transport. Implementations are
-- responsible for the underlying I\/O (TCP sockets, WebSockets, HTTP,
-- stdio, etc.).
--
-- The typeclass covers sending and closing. Receiving is handled via
-- callbacks ('OnMessage', 'OnError', 'OnClose') configured in the
-- concrete transport's constructor.
class Transport t where
  -- | Send raw bytes over the transport. Returns @'Left'
  -- 'TransportError'@ if the send fails.
  send :: t -> ByteString -> IO (Either TransportError ())

  -- | Close the transport, releasing any resources.
  close :: t -> IO ()

-- | An error originating from the transport layer (socket errors, HTTP
-- failures, etc.). Carries a human-readable message and optionally the
-- underlying exception.
data TransportError = TransportError
  { -- | A human-readable description of the transport failure.
    transportMessage :: !Text
  , -- | The underlying exception, if one was caught.
    transportCause :: !(Maybe SomeException)
  }
  deriving (Show)

instance Exception TransportError

-- | Callback invoked when a message is received from the transport.
type OnMessage = ByteString -> IO ()

-- | Callback invoked when a transport error occurs.
type OnError = TransportError -> IO ()

-- | Callback invoked when the transport is closed.
type OnClose = IO ()
