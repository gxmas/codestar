-- |
-- Module      : Network.MCP.Types
-- Stability   : stable
--
-- JSON-RPC 2.0 envelope types and scalar newtypes for the Model
-- Context Protocol (MCP).
module Network.MCP.Types
  ( -- * Envelope ADT
    MCPMessage (..)
  , JSONRPCRequest (..)
  , JSONRPCNotification (..)
  , JSONRPCResult (..)
  , JSONRPCError (..)
  , RPCError (..)

    -- * Scalar newtypes
  , RequestId (..)
  , ProgressToken (..)
  , Cursor (..)
  , ProtocolVersion (..)
  , Timestamp (..)

    -- * Meta
  , Meta

    -- * Enumerations
  , LoggingLevel (..)

    -- * Implementation info
  , Implementation (..)

    -- * Constants
  , jsonrpcVersion
  ) where

import Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, Pair, typeMismatch)
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.KeyMap as KM
import GHC.Generics (Generic)
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T

-- | The JSON-RPC version string, always @\"2.0\"@.
jsonrpcVersion :: Text
jsonrpcVersion = "2.0"

------------------------------------------------------------------------
-- Scalar newtypes
------------------------------------------------------------------------

-- | JSON-RPC request identifier. JSON numbers map to 'Right',
-- JSON strings to 'Left'.
newtype RequestId = RequestId (Either Text Int)
  deriving stock (Eq, Ord, Show)

instance Hashable RequestId where
  hashWithSalt s (RequestId (Left t)) = hashWithSalt s (0 :: Int, t)
  hashWithSalt s (RequestId (Right n)) = hashWithSalt s (1 :: Int, n)

instance Aeson.ToJSON RequestId where
  toJSON (RequestId (Left t)) = Aeson.String t
  toJSON (RequestId (Right n)) = Aeson.Number (fromIntegral n)
  toEncoding (RequestId (Left t)) = E.text t
  toEncoding (RequestId (Right n)) = E.int n

instance Aeson.FromJSON RequestId where
  parseJSON (Aeson.String t) = pure (RequestId (Left t))
  parseJSON (Aeson.Number n) = pure (RequestId (Right (truncate n)))
  parseJSON v = typeMismatch "RequestId (string or integer)" v

-- | Progress token for long-running operations.
newtype ProgressToken = ProgressToken (Either Text Int)
  deriving stock (Eq, Ord, Show)

instance Aeson.ToJSON ProgressToken where
  toJSON (ProgressToken (Left t)) = Aeson.String t
  toJSON (ProgressToken (Right n)) = Aeson.Number (fromIntegral n)
  toEncoding (ProgressToken (Left t)) = E.text t
  toEncoding (ProgressToken (Right n)) = E.int n

instance Aeson.FromJSON ProgressToken where
  parseJSON (Aeson.String t) = pure (ProgressToken (Left t))
  parseJSON (Aeson.Number n) = pure (ProgressToken (Right (truncate n)))
  parseJSON v = typeMismatch "ProgressToken (string or integer)" v

-- | Opaque cursor for pagination.
newtype Cursor = Cursor Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON, Hashable)

-- | MCP protocol version string.
newtype ProtocolVersion = ProtocolVersion Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

-- | ISO 8601 timestamp.
newtype Timestamp = Timestamp Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

------------------------------------------------------------------------
-- Meta
------------------------------------------------------------------------

-- | Metadata map, preserved as-is through encode/decode.
type Meta = HashMap Text Aeson.Value

------------------------------------------------------------------------
-- LoggingLevel
------------------------------------------------------------------------

-- | Syslog-style logging levels, ordered from least to most severe.
data LoggingLevel
  = LevelDebug
  | LevelInfo
  | LevelNotice
  | LevelWarning
  | LevelError
  | LevelCritical
  | LevelAlert
  | LevelEmergency
  deriving stock (Eq, Ord, Show, Bounded, Enum, Generic)

instance Aeson.ToJSON LoggingLevel where
  toJSON = \case
    LevelDebug -> "debug"
    LevelInfo -> "info"
    LevelNotice -> "notice"
    LevelWarning -> "warning"
    LevelError -> "error"
    LevelCritical -> "critical"
    LevelAlert -> "alert"
    LevelEmergency -> "emergency"
  toEncoding = \case
    LevelDebug -> E.text "debug"
    LevelInfo -> E.text "info"
    LevelNotice -> E.text "notice"
    LevelWarning -> E.text "warning"
    LevelError -> E.text "error"
    LevelCritical -> E.text "critical"
    LevelAlert -> E.text "alert"
    LevelEmergency -> E.text "emergency"

instance Aeson.FromJSON LoggingLevel where
  parseJSON = Aeson.withText "LoggingLevel" $ \case
    "debug" -> pure LevelDebug
    "info" -> pure LevelInfo
    "notice" -> pure LevelNotice
    "warning" -> pure LevelWarning
    "error" -> pure LevelError
    "critical" -> pure LevelCritical
    "alert" -> pure LevelAlert
    "emergency" -> pure LevelEmergency
    other -> fail $ "Unknown LoggingLevel: " ++ T.unpack other

------------------------------------------------------------------------
-- Implementation
------------------------------------------------------------------------

-- | Server or client implementation metadata.
data Implementation = Implementation
  { implName :: !Text
  , implVersion :: !Text
  , implTitle :: !(Maybe Text)
  , implDescription :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Implementation where
  toJSON impl =
    Aeson.object $
      [ "name" .= impl.implName
      , "version" .= impl.implVersion
      ]
        ++ maybe [] (\t -> ["title" .= t]) impl.implTitle
        ++ maybe [] (\d -> ["description" .= d]) impl.implDescription
  toEncoding impl =
    E.pairs $
      "name" .= impl.implName
        <> "version" .= impl.implVersion
        <> foldMap ("title" .=) impl.implTitle
        <> foldMap ("description" .=) impl.implDescription

instance Aeson.FromJSON Implementation where
  parseJSON = Aeson.withObject "Implementation" $ \o ->
    Implementation
      <$> o .: "name"
      <*> o .: "version"
      <*> o .:? "title"
      <*> o .:? "description"

------------------------------------------------------------------------
-- RPCError
------------------------------------------------------------------------

-- | JSON-RPC error object.
data RPCError = RPCError
  { rpcErrorCode :: !Int
  , rpcErrorMessage :: !Text
  , rpcErrorData :: !(Maybe Aeson.Value)
  }
  deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON RPCError where
  toJSON e =
    Aeson.object $
      [ "code" .= e.rpcErrorCode
      , "message" .= e.rpcErrorMessage
      ]
        ++ maybe [] (\d -> ["data" .= d]) e.rpcErrorData
  toEncoding e =
    E.pairs $
      "code" .= e.rpcErrorCode
        <> "message" .= e.rpcErrorMessage
        <> foldMap ("data" .=) e.rpcErrorData

instance Aeson.FromJSON RPCError where
  parseJSON = Aeson.withObject "RPCError" $ \o ->
    RPCError
      <$> o .: "code"
      <*> o .: "message"
      <*> explicitMaybe o "data"

------------------------------------------------------------------------
-- JSON-RPC envelope types
------------------------------------------------------------------------

-- | A JSON-RPC request with an @id@ field.
data JSONRPCRequest = JSONRPCRequest
  { requestId :: !RequestId
  , requestMethod :: !Text
  , requestParams :: !(Maybe Aeson.Value)
  , requestMeta :: !(Maybe Meta)
  }
  deriving stock (Eq, Show, Generic)

-- | A JSON-RPC notification (no @id@ field).
data JSONRPCNotification = JSONRPCNotification
  { notificationMethod :: !Text
  , notificationParams :: !(Maybe Aeson.Value)
  , notificationMeta :: !(Maybe Meta)
  }
  deriving stock (Eq, Show, Generic)

-- | A successful JSON-RPC result.
data JSONRPCResult = JSONRPCResult
  { resultId :: !RequestId
  , resultResult :: !Aeson.Value
  , resultMeta :: !(Maybe Meta)
  }
  deriving stock (Eq, Show, Generic)

-- | A JSON-RPC error response.
data JSONRPCError = JSONRPCError
  { errorId :: !(Maybe RequestId)
  , errorError :: !RPCError
  }
  deriving stock (Eq, Show, Generic)

------------------------------------------------------------------------
-- MCPMessage
------------------------------------------------------------------------

-- | Top-level MCP protocol message, discriminated by the presence of
-- @\"error\"@, @\"result\"@, or @\"id\"@ keys.
data MCPMessage
  = MCPRequest !JSONRPCRequest
  | MCPNotification !JSONRPCNotification
  | MCPResult !JSONRPCResult
  | MCPError !JSONRPCError
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- ToJSON instances for envelope types
------------------------------------------------------------------------

instance Aeson.ToJSON JSONRPCRequest where
  toJSON req =
    Aeson.object $
      [ "jsonrpc" .= jsonrpcVersion
      , "id" .= req.requestId
      , "method" .= req.requestMethod
      ]
        ++ encodeParams req.requestParams req.requestMeta
  toEncoding req =
    E.pairs $
      "jsonrpc" .= jsonrpcVersion
        <> "id" .= req.requestId
        <> "method" .= req.requestMethod
        <> encodeParamsPairs req.requestParams req.requestMeta

instance Aeson.ToJSON JSONRPCNotification where
  toJSON notif =
    Aeson.object $
      [ "jsonrpc" .= jsonrpcVersion
      , "method" .= notif.notificationMethod
      ]
        ++ encodeParams notif.notificationParams notif.notificationMeta
  toEncoding notif =
    E.pairs $
      "jsonrpc" .= jsonrpcVersion
        <> "method" .= notif.notificationMethod
        <> encodeParamsPairs notif.notificationParams notif.notificationMeta

instance Aeson.ToJSON JSONRPCResult where
  toJSON res =
    Aeson.object
      [ "jsonrpc" .= jsonrpcVersion
      , "id" .= res.resultId
      , "result" .= injectMeta res.resultResult res.resultMeta
      ]
  toEncoding res =
    E.pairs $
      "jsonrpc" .= jsonrpcVersion
        <> "id" .= res.resultId
        <> "result" .= injectMeta res.resultResult res.resultMeta

instance Aeson.ToJSON JSONRPCError where
  toJSON e =
    Aeson.object $
      [ "jsonrpc" .= jsonrpcVersion
      , "error" .= e.errorError
      ]
        ++ maybe [] (\i -> ["id" .= i]) e.errorId
  toEncoding e =
    E.pairs $
      "jsonrpc" .= jsonrpcVersion
        <> "error" .= e.errorError
        <> foldMap ("id" .=) e.errorId

instance Aeson.ToJSON MCPMessage where
  toJSON = \case
    MCPRequest req -> Aeson.toJSON req
    MCPNotification notif -> Aeson.toJSON notif
    MCPResult res -> Aeson.toJSON res
    MCPError e -> Aeson.toJSON e
  toEncoding = \case
    MCPRequest req -> Aeson.toEncoding req
    MCPNotification notif -> Aeson.toEncoding notif
    MCPResult res -> Aeson.toEncoding res
    MCPError e -> Aeson.toEncoding e

------------------------------------------------------------------------
-- FromJSON instances for envelope types
------------------------------------------------------------------------

instance Aeson.FromJSON MCPMessage where
  parseJSON = Aeson.withObject "MCPMessage" $ \o -> do
    validateVersion o
    if KM.member "error" o
      then MCPError <$> parseError' o
      else
        if KM.member "result" o
          then MCPResult <$> parseResult' o
          else
            if KM.member "id" o
              then MCPRequest <$> parseRequest' o
              else MCPNotification <$> parseNotification' o

instance Aeson.FromJSON JSONRPCRequest where
  parseJSON = Aeson.withObject "JSONRPCRequest" $ \o -> do
    validateVersion o
    parseRequest' o

instance Aeson.FromJSON JSONRPCNotification where
  parseJSON = Aeson.withObject "JSONRPCNotification" $ \o -> do
    validateVersion o
    parseNotification' o

instance Aeson.FromJSON JSONRPCResult where
  parseJSON = Aeson.withObject "JSONRPCResult" $ \o -> do
    validateVersion o
    parseResult' o

instance Aeson.FromJSON JSONRPCError where
  parseJSON = Aeson.withObject "JSONRPCError" $ \o -> do
    validateVersion o
    parseError' o

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

validateVersion :: Aeson.Object -> Parser ()
validateVersion o = do
  v <- o .: "jsonrpc" :: Parser Text
  if v /= jsonrpcVersion
    then fail $ "Expected jsonrpc \"2.0\", got " ++ show v
    else pure ()

parseRequest' :: Aeson.Object -> Parser JSONRPCRequest
parseRequest' o = do
  rid <- o .: "id"
  method <- o .: "method"
  mparams <- o .:? "params"
  let (params, meta) = extractMeta mparams
  pure (JSONRPCRequest rid method params meta)

parseNotification' :: Aeson.Object -> Parser JSONRPCNotification
parseNotification' o = do
  method <- o .: "method"
  mparams <- o .:? "params"
  let (params, meta) = extractMeta mparams
  pure (JSONRPCNotification method params meta)

parseResult' :: Aeson.Object -> Parser JSONRPCResult
parseResult' o = do
  rid <- o .: "id"
  result <- o .: "result"
  let (result', meta) = extractMetaFromResult result
  pure (JSONRPCResult rid result' meta)

parseError' :: Aeson.Object -> Parser JSONRPCError
parseError' o =
  JSONRPCError
    <$> o .:? "id"
    <*> o .: "error"

-- | Extract @_meta@ from params (an object). If params is an object
-- and contains @_meta@, extract it and return params without it.
extractMeta :: Maybe Aeson.Value -> (Maybe Aeson.Value, Maybe Meta)
extractMeta Nothing = (Nothing, Nothing)
extractMeta (Just (Aeson.Object o)) =
  case KM.lookup "_meta" o of
    Nothing -> (Just (Aeson.Object o), Nothing)
    Just metaVal ->
      let remaining = KM.delete "_meta" o
          params =
            if KM.null remaining
              then Nothing
              else Just (Aeson.Object remaining)
          meta = case Aeson.fromJSON metaVal of
            Aeson.Success m -> Just m
            _ -> Nothing
       in (params, meta)
extractMeta (Just v) = (Just v, Nothing)

-- | Extract @_meta@ from a result value (which may be an object).
extractMetaFromResult :: Aeson.Value -> (Aeson.Value, Maybe Meta)
extractMetaFromResult (Aeson.Object o) =
  case KM.lookup "_meta" o of
    Nothing -> (Aeson.Object o, Nothing)
    Just metaVal ->
      let remaining = KM.delete "_meta" o
          meta = case Aeson.fromJSON metaVal of
            Aeson.Success m -> Just m
            _ -> Nothing
       in (Aeson.Object remaining, meta)
extractMetaFromResult v = (v, Nothing)

-- | Inject @_meta@ into a JSON value. If the value is an object,
-- insert the key; otherwise leave unchanged.
injectMeta :: Aeson.Value -> Maybe Meta -> Aeson.Value
injectMeta v Nothing = v
injectMeta (Aeson.Object o) (Just meta) =
  Aeson.Object (KM.insert "_meta" (Aeson.toJSON meta) o)
injectMeta v (Just _) = v

-- | Encode params + meta as a @\"params\"@ field pair list.
encodeParams :: Maybe Aeson.Value -> Maybe Meta -> [Pair]
encodeParams Nothing Nothing = []
encodeParams Nothing (Just meta) = ["params" .= Aeson.object ["_meta" .= meta]]
encodeParams (Just p) meta = ["params" .= injectMeta p meta]

-- | Encode params + meta for 'E.pairs'-based encoding.
encodeParamsPairs :: Maybe Aeson.Value -> Maybe Meta -> E.Series
encodeParamsPairs Nothing Nothing = mempty
encodeParamsPairs Nothing (Just meta) = "params" .= Aeson.object ["_meta" .= meta]
encodeParamsPairs (Just p) meta = "params" .= injectMeta p meta

-- | Parse an optional field that distinguishes between absent and @null@.
-- Unlike '.:?', this returns @Just Null@ when the field is present and @null@.
explicitMaybe :: Aeson.FromJSON a => Aeson.Object -> Aeson.Key -> Parser (Maybe a)
explicitMaybe o k = case KM.lookup k o of
  Nothing -> pure Nothing
  Just v -> Just <$> Aeson.parseJSON v
