-- | HTTP semantic conventions (semconv 1.27.0).
module OTel.SemConv.HTTP
  ( httpRequestMethod
  , httpRequestMethodOriginal
  , httpRequestBodySize
  , httpRequestResendCount
  , httpResponseStatusCode
  , httpResponseBodySize
  , httpRoute
  , urlFull
  , urlPath
  , urlQuery
  , urlScheme
  , urlFragment
  , serverAddress
  , serverPort
  , networkPeerAddress
  , networkPeerPort
  , networkProtocolName
  , networkProtocolVersion
  , networkLocalAddress
  , networkLocalPort
  , networkTransport
  , userAgentOriginal
  , clientAddress
  , clientPort
  , errorType
  ) where

import Data.Text (Text)

-- | @http.request.method@
httpRequestMethod :: Text
httpRequestMethod = "http.request.method"

-- | @http.request.method_original@
httpRequestMethodOriginal :: Text
httpRequestMethodOriginal = "http.request.method_original"

-- | @http.request.body.size@
httpRequestBodySize :: Text
httpRequestBodySize = "http.request.body.size"

-- | @http.request.resend_count@
httpRequestResendCount :: Text
httpRequestResendCount = "http.request.resend_count"

-- | @http.response.status_code@
httpResponseStatusCode :: Text
httpResponseStatusCode = "http.response.status_code"

-- | @http.response.body.size@
httpResponseBodySize :: Text
httpResponseBodySize = "http.response.body.size"

-- | @http.route@
httpRoute :: Text
httpRoute = "http.route"

-- | @url.full@
urlFull :: Text
urlFull = "url.full"

-- | @url.path@
urlPath :: Text
urlPath = "url.path"

-- | @url.query@
urlQuery :: Text
urlQuery = "url.query"

-- | @url.scheme@
urlScheme :: Text
urlScheme = "url.scheme"

-- | @url.fragment@
urlFragment :: Text
urlFragment = "url.fragment"

-- | @server.address@
serverAddress :: Text
serverAddress = "server.address"

-- | @server.port@
serverPort :: Text
serverPort = "server.port"

-- | @network.peer.address@
networkPeerAddress :: Text
networkPeerAddress = "network.peer.address"

-- | @network.peer.port@
networkPeerPort :: Text
networkPeerPort = "network.peer.port"

-- | @network.protocol.name@
networkProtocolName :: Text
networkProtocolName = "network.protocol.name"

-- | @network.protocol.version@
networkProtocolVersion :: Text
networkProtocolVersion = "network.protocol.version"

-- | @network.local.address@
networkLocalAddress :: Text
networkLocalAddress = "network.local.address"

-- | @network.local.port@
networkLocalPort :: Text
networkLocalPort = "network.local.port"

-- | @network.transport@
networkTransport :: Text
networkTransport = "network.transport"

-- | @user_agent.original@
userAgentOriginal :: Text
userAgentOriginal = "user_agent.original"

-- | @client.address@
clientAddress :: Text
clientAddress = "client.address"

-- | @client.port@
clientPort :: Text
clientPort = "client.port"

-- | @error.type@
errorType :: Text
errorType = "error.type"
