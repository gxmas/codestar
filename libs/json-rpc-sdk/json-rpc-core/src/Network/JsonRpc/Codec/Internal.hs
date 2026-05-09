{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Network.JsonRpc.Codec.Internal
-- Stability   : unstable
-- Portability : GHC2021
--
-- __This is an internal module.__ The API may change between minor
-- versions without notice.
--
-- Defines orphan 'FromJSON' and 'ToJSON' instances for all types in
-- "Network.JsonRpc.Types". These are orphan instances by design: the
-- architecture separates type definitions (Types) from serialization
-- logic (Codec). This is a narrow, well-documented orphan set.
--
-- Also exports internal parsing helpers used by "Network.JsonRpc.Codec".
module Network.JsonRpc.Codec.Internal
  ( -- * Internal parsing
    parseValue
  , parseSingle
  , parseBatch
  , extractId

    -- * Re-export instances (importing this module brings them into scope)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , (.=)
  , (.:)
  , (.:?)
  )
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Vector qualified as V
import Network.JsonRpc.Types.Internal

-- ====================================================================
-- ToJSON instances
-- ====================================================================

instance ToJSON Id where
  toJSON (IdText t) = toJSON t
  toJSON (IdNum n) = toJSON n
  {-# INLINE toJSON #-}

instance ToJSON Params where
  toJSON (ParamsByPosition v) = toJSON v
  toJSON (ParamsByName m) = toJSON m
  toJSON NoParams = Null
  {-# INLINE toJSON #-}

instance ToJSON ErrorObject where
  toJSON (ErrorObject code msg d) =
    Aeson.object $
      [ "code" .= code
      , "message" .= msg
      ]
        ++ maybe [] (\v -> ["data" .= v]) d
  {-# INLINE toJSON #-}

instance ToJSON Request where
  toJSON (Request m p i) =
    Aeson.object $
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= m
      , "id" .= i
      ]
        ++ paramsField p
  {-# INLINE toJSON #-}

instance ToJSON Notification where
  toJSON (Notification m p) =
    Aeson.object $
      [ "jsonrpc" .= ("2.0" :: Text)
      , "method" .= m
      ]
        ++ paramsField p
  {-# INLINE toJSON #-}

instance ToJSON Success where
  toJSON (Success r i) =
    Aeson.object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "result" .= r
      , "id" .= i
      ]
  {-# INLINE toJSON #-}

instance ToJSON Failure where
  toJSON (Failure e i) =
    Aeson.object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "error" .= e
      , "id" .= i
      ]
  {-# INLINE toJSON #-}

instance ToJSON Response where
  toJSON (ResponseSuccess s) = toJSON s
  toJSON (ResponseFailure f) = toJSON f
  {-# INLINE toJSON #-}

instance ToJSON Message where
  toJSON (MsgRequest r) = toJSON r
  toJSON (MsgNotification n) = toJSON n
  toJSON (MsgResponse r) = toJSON r
  {-# INLINE toJSON #-}

-- | Conditionally include the @params@ field. Omitted when 'NoParams'.
paramsField :: Params -> [(Aeson.Key, Value)]
paramsField NoParams = []
paramsField p = ["params" .= p]

-- ====================================================================
-- FromJSON instances
-- ====================================================================

instance FromJSON Id where
  parseJSON (String t) = pure (IdText t)
  parseJSON (Number n) = pure (IdNum n)
  parseJSON Null = fail "null is not a valid Id (use Maybe Id)"
  parseJSON _ = fail "Id must be a String or Number"

instance FromJSON Params where
  parseJSON (Array v) = pure (ParamsByPosition v)
  parseJSON (Object m) = pure (ParamsByName m)
  parseJSON _ = fail "params must be an Array or Object"

instance FromJSON ErrorObject where
  parseJSON = Aeson.withObject "ErrorObject" $ \o ->
    ErrorObject
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

instance FromJSON Request where
  parseJSON = Aeson.withObject "Request" $ \o -> do
    ver <- o .: "jsonrpc" :: Parser Text
    if ver /= "2.0"
      then fail "jsonrpc must be \"2.0\""
      else
        Request
          <$> o .: "method"
          <*> parseParamsField o
          <*> o .:? "id"

instance FromJSON Notification where
  parseJSON = Aeson.withObject "Notification" $ \o -> do
    ver <- o .: "jsonrpc" :: Parser Text
    if ver /= "2.0"
      then fail "jsonrpc must be \"2.0\""
      else
        Notification
          <$> o .: "method"
          <*> parseParamsField o

instance FromJSON Success where
  parseJSON = Aeson.withObject "Success" $ \o -> do
    ver <- o .: "jsonrpc" :: Parser Text
    if ver /= "2.0"
      then fail "jsonrpc must be \"2.0\""
      else
        Success
          <$> o .: "result"
          <*> o .:? "id"

instance FromJSON Failure where
  parseJSON = Aeson.withObject "Failure" $ \o -> do
    ver <- o .: "jsonrpc" :: Parser Text
    if ver /= "2.0"
      then fail "jsonrpc must be \"2.0\""
      else
        Failure
          <$> o .: "error"
          <*> o .:? "id"

-- | Parse the optional @params@ field from a JSON object.
-- Absent -> 'NoParams'. Present -> must be Array or Object.
parseParamsField :: KeyMap Value -> Parser Params
parseParamsField o = case KM.lookup "params" o of
  Nothing -> pure NoParams
  Just v -> parseJSON v

-- ====================================================================
-- Internal parsing helpers (used by Network.JsonRpc.Codec.parse)
-- ====================================================================

-- | Dispatch a parsed JSON 'Value' into a 'ParseResult'.
parseValue :: Value -> ParseResult
parseValue (Object o) = parseSingle o
parseValue (Array a)
  | V.null a = ParseFailed (invalidRequest Nothing)
  | otherwise = parseBatch a
parseValue _ = ParseFailed (invalidRequest Nothing)

-- | Parse a single JSON Object into a 'Message', wrapped in a
-- 'ParseResult'.
parseSingle :: KeyMap Value -> ParseResult
parseSingle o = case parseSingleObject o of
  Left failure -> SingleMessage (MsgResponse (ResponseFailure failure))
  Right msg -> SingleMessage msg

-- | Parse a JSON Array as a batch of messages.
parseBatch :: V.Vector Value -> ParseResult
parseBatch elems = BatchMessage (map parseElement (V.toList elems))
  where
    parseElement :: Value -> BatchElement
    parseElement (Object o) = case parseSingleObject o of
      Left failure -> InvalidElement failure
      Right msg -> ValidElement msg
    parseElement _ = InvalidElement (Failure (invalidRequest Nothing) Nothing)

-- | Try to extract an 'Id' from a JSON object's @\"id\"@ field.
-- Returns 'Nothing' if the field is absent or not a valid id type.
-- Returns @Just Nothing@ if the field is @null@.
-- Returns @Just (Just id)@ if the field is a valid non-null id.
extractId :: KeyMap Value -> Maybe (Maybe Id)
extractId o = case KM.lookup "id" o of
  Nothing -> Nothing
  Just Null -> Just Nothing
  Just (String t) -> Just (Just (IdText t))
  Just (Number n) -> Just (Just (IdNum n))
  Just _ -> Nothing

-- | Parse a single JSON Object into a 'Message'. Returns 'Left'
-- 'Failure' for invalid messages (with the extracted id if possible).
parseSingleObject :: KeyMap Value -> Either Failure Message
parseSingleObject o
  -- Check jsonrpc version
  | not hasValidVersion = Left (Failure (invalidRequest Nothing) mid)
  -- Response detection: has "result" or "error"
  | hasResult && hasError = Left (Failure (invalidRequest Nothing) mid)
  | hasResult = case parseSuccessFromObject o of
      Nothing -> Left (Failure (invalidRequest Nothing) mid)
      Just s -> Right (MsgResponse (ResponseSuccess s))
  | hasError = case parseFailureFromObject o of
      Nothing -> Left (Failure (invalidRequest Nothing) mid)
      Just f -> Right (MsgResponse (ResponseFailure f))
  -- Request/Notification detection: must have "method"
  | not hasMethod = Left (Failure (invalidRequest Nothing) mid)
  | not validMethod = Left (Failure (invalidRequest Nothing) mid)
  | not validParams = Left (Failure (invalidParams Nothing) mid)
  -- Distinguish Request from Notification by presence of "id" key
  | hasId = case parseRequestFromObject o of
      Nothing -> Left (Failure (invalidRequest Nothing) mid)
      Just r -> Right (MsgRequest r)
  | otherwise = case parseNotificationFromObject o of
      Nothing -> Left (Failure (invalidRequest Nothing) mid)
      Just n -> Right (MsgNotification n)
  where
    mid :: Maybe Id
    mid = case extractId o of
      Just i -> i
      Nothing -> Nothing

    hasValidVersion :: Bool
    hasValidVersion = case KM.lookup "jsonrpc" o of
      Just (String "2.0") -> True
      _ -> False

    hasResult :: Bool
    hasResult = KM.member "result" o

    hasError :: Bool
    hasError = KM.member "error" o

    hasMethod :: Bool
    hasMethod = KM.member "method" o

    validMethod :: Bool
    validMethod = case KM.lookup "method" o of
      Just (String _) -> True
      _ -> False

    validParams :: Bool
    validParams = case KM.lookup "params" o of
      Nothing -> True
      Just (Array _) -> True
      Just (Object _) -> True
      _ -> False

    hasId :: Bool
    hasId = KM.member "id" o

-- | Parse a Success response from a JSON object. The caller has
-- already verified that @\"result\"@ is present and @\"error\"@ is
-- absent.
parseSuccessFromObject :: KeyMap Value -> Maybe Success
parseSuccessFromObject o = do
  result <- KM.lookup "result" o
  let mid = case extractId o of
        Just i -> i
        Nothing -> Nothing
  pure (Success result mid)

-- | Parse a Failure response from a JSON object. The caller has
-- already verified that @\"error\"@ is present and @\"result\"@ is
-- absent.
parseFailureFromObject :: KeyMap Value -> Maybe Failure
parseFailureFromObject o = do
  errVal <- KM.lookup "error" o
  errObj <- parseErrorValue errVal
  let mid = case extractId o of
        Just i -> i
        Nothing -> Nothing
  pure (Failure errObj mid)

-- | Parse an 'ErrorObject' from a JSON 'Value'.
parseErrorValue :: Value -> Maybe ErrorObject
parseErrorValue (Object o) = do
  Number codeN <- KM.lookup "code" o
  let code = truncate codeN :: Int
  String msg <- KM.lookup "message" o
  let d = KM.lookup "data" o
  pure (ErrorObject code msg d)
parseErrorValue _ = Nothing

-- | Parse a Request from a JSON object. The caller has verified
-- version, method, params validity, and id presence.
parseRequestFromObject :: KeyMap Value -> Maybe Request
parseRequestFromObject o = do
  String m <- KM.lookup "method" o
  let params = case KM.lookup "params" o of
        Nothing -> NoParams
        Just (Array v) -> ParamsByPosition v
        Just (Object km) -> ParamsByName km
        _ -> NoParams -- already validated; this branch unreachable
  let mid = case extractId o of
        Just i -> i
        Nothing -> Nothing
  pure (Request m params mid)

-- | Parse a Notification from a JSON object. The caller has verified
-- version, method, params validity, and id absence.
parseNotificationFromObject :: KeyMap Value -> Maybe Notification
parseNotificationFromObject o = do
  String m <- KM.lookup "method" o
  let params = case KM.lookup "params" o of
        Nothing -> NoParams
        Just (Array v) -> ParamsByPosition v
        Just (Object km) -> ParamsByName km
        _ -> NoParams
  pure (Notification m params)
