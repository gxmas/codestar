-- | RPC semantic conventions (semconv 1.27.0).
module OTel.SemConv.RPC
  ( rpcSystem
  , rpcService
  , rpcMethod
  , rpcGrpcStatusCode
  , rpcJsonrpcVersion
  , rpcJsonrpcRequestId
  , rpcJsonrpcErrorCode
  , rpcJsonrpcErrorMessage
  ) where

import Data.Text (Text)

-- | @rpc.system@
rpcSystem :: Text
rpcSystem = "rpc.system"

-- | @rpc.service@
rpcService :: Text
rpcService = "rpc.service"

-- | @rpc.method@
rpcMethod :: Text
rpcMethod = "rpc.method"

-- | @rpc.grpc.status_code@
rpcGrpcStatusCode :: Text
rpcGrpcStatusCode = "rpc.grpc.status_code"

-- | @rpc.jsonrpc.version@
rpcJsonrpcVersion :: Text
rpcJsonrpcVersion = "rpc.jsonrpc.version"

-- | @rpc.jsonrpc.request_id@
rpcJsonrpcRequestId :: Text
rpcJsonrpcRequestId = "rpc.jsonrpc.request_id"

-- | @rpc.jsonrpc.error_code@
rpcJsonrpcErrorCode :: Text
rpcJsonrpcErrorCode = "rpc.jsonrpc.error_code"

-- | @rpc.jsonrpc.error_message@
rpcJsonrpcErrorMessage :: Text
rpcJsonrpcErrorMessage = "rpc.jsonrpc.error_message"
