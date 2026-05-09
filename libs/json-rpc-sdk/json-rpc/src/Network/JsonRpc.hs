-- |
-- Module      : Network.JsonRpc
-- Stability   : stable
-- Portability : GHC2021
--
-- Convenience module that re-exports the full public API from
-- @json-rpc-core@, @json-rpc-server@, and @json-rpc-client@.
--
-- @
-- import Network.JsonRpc
-- @
--
-- For selective imports, depend on the sub-packages directly and
-- import from "Network.JsonRpc.Types", "Network.JsonRpc.Codec",
-- "Network.JsonRpc.Transport", "Network.JsonRpc.Server", or
-- "Network.JsonRpc.Client".
module Network.JsonRpc
  ( -- * Re-exports from json-rpc-core
    module Network.JsonRpc.Types
  , module Network.JsonRpc.Codec
  , module Network.JsonRpc.Transport

    -- * Re-exports from json-rpc-server
  , module Network.JsonRpc.Server

    -- * Re-exports from json-rpc-client
  , module Network.JsonRpc.Client
  ) where

import Network.JsonRpc.Client
import Network.JsonRpc.Codec
import Network.JsonRpc.Server
import Network.JsonRpc.Transport
import Network.JsonRpc.Types
