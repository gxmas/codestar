-- | W3C Trace Context and Baggage propagators.
module OTel.Propagation.W3C
  ( W3CTraceContextPropagator (..)
  , W3CBaggagePropagator (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)

import OTel.Baggage
  ( BaggageEntry (..)
  , baggageFromList
  , baggageToList
  , getBaggage
  , setBaggage
  )
import OTel.Propagation
  ( TextMapGetter (..)
  , TextMapPropagator (..)
  , TextMapSetter (..)
  )
import OTel.Trace
  ( Span (..)
  , createNonRecordingSpan
  , getSpanFromContext
  , setSpanInContext
  )
import OTel.Trace.SpanContext
  ( SpanContext (..)
  , isValid
  , spanIdFromHex
  , spanIdToHex
  , traceFlagsFromByte
  , traceFlagsToByte
  , traceIdFromHex
  , traceIdToHex
  )
import OTel.Trace.TraceState qualified as TraceState


-------------------------------------------------------------------------------
-- W3CTraceContextPropagator
-------------------------------------------------------------------------------

-- | Propagator implementing W3C Trace Context (traceparent + tracestate).
data W3CTraceContextPropagator = W3CTraceContextPropagator

instance TextMapPropagator W3CTraceContextPropagator where
  inject _ ctx carrier = case getSpanFromContext ctx of
    Nothing -> pure carrier
    Just someSpan -> do
      sc <- getSpanContext someSpan
      if not (isValid sc)
        then pure carrier
        else do
          let tp =
                "00-"
                  <> traceIdToHex sc.traceId
                  <> "-"
                  <> spanIdToHex sc.spanId
                  <> "-"
                  <> word8ToHex (traceFlagsToByte sc.traceFlags)
              carrier1 = tmSet carrier "traceparent" tp
              ts = TraceState.toList sc.traceState
              carrier2
                | null ts = carrier1
                | otherwise =
                    tmSet carrier1 "tracestate" (serializeTraceState ts)
          pure carrier2

  extract _ ctx carrier = case tmGet carrier "traceparent" of
    Nothing -> pure ctx
    Just tp -> case parseTraceparent tp of
      Nothing -> pure ctx
      Just sc
        | not (isValid sc) -> pure ctx
        | otherwise -> do
            let tsRaw = tmGet carrier "tracestate"
                ts = case tsRaw of
                  Nothing -> TraceState.empty
                  Just raw -> parseTraceState raw
                sc' = sc {traceState = ts}
                span' = createNonRecordingSpan sc'
            pure (setSpanInContext span' ctx)

  fields _ = ["traceparent", "tracestate"]


parseTraceparent :: Text -> Maybe SpanContext
parseTraceparent raw = do
  let parts = Text.splitOn "-" raw
  case parts of
    (version : tid : sid : flags : rest)
      | Text.length version == 2
      , version /= "ff"
      , version /= "00" || null rest  -- spec §3.2.2: v00 must have exactly 4 fields
      , Text.length tid == 32
      , Text.length sid == 16
      , Text.length flags == 2 -> do
          flagsByte <- parseHexByte flags
          let sc =
                SpanContext
                  { traceId = traceIdFromHex tid
                  , spanId = spanIdFromHex sid
                  , traceFlags = traceFlagsFromByte flagsByte
                  , traceState = TraceState.empty
                  , _isRemote = True
                  }
          if isValid sc then Just sc else Nothing
    _ -> Nothing


serializeTraceState :: [(Text, Text)] -> Text
serializeTraceState entries =
  Text.intercalate "," [k <> "=" <> v | (k, v) <- entries]


parseTraceState :: Text -> TraceState.TraceState
parseTraceState raw =
  let entries = Text.splitOn "," raw
      parsed = concatMap parseEntry entries
   in foldr (\(k, v) ts -> TraceState.set k v ts) TraceState.empty parsed
  where
    parseEntry entry =
      let trimmed = Text.strip entry
       in case Text.breakOn "=" trimmed of
            (k, rest)
              | Text.null k -> []
              | Text.null rest -> []
              | otherwise -> [(k, Text.drop 1 rest)]


-------------------------------------------------------------------------------
-- W3CBaggagePropagator
-------------------------------------------------------------------------------

-- | Propagator implementing W3C Baggage.
data W3CBaggagePropagator = W3CBaggagePropagator

instance TextMapPropagator W3CBaggagePropagator where
  inject _ ctx carrier = do
    let entries = baggageToList (getBaggage ctx)
    if null entries
      then pure carrier
      else do
        let serialized = Text.intercalate "," (map serializeBaggageEntry entries)
        pure (tmSet carrier "baggage" serialized)

  extract _ ctx carrier = case tmGet carrier "baggage" of
    Nothing -> pure ctx
    Just raw -> do
      let entries = parseBaggageHeader raw
      if null entries
        then pure ctx
        else pure (setBaggage (baggageFromList entries) ctx)

  fields _ = ["baggage"]


serializeBaggageEntry :: (Text, BaggageEntry) -> Text
serializeBaggageEntry (k, entry) =
  let base = k <> "=" <> entry.entryValue
   in case entry.entryMetadata of
        Nothing -> base
        Just md -> base <> ";" <> md


parseBaggageHeader :: Text -> [(Text, BaggageEntry)]
parseBaggageHeader raw =
  concatMap parseEntry (Text.splitOn "," raw)
  where
    parseEntry part =
      let trimmed = Text.strip part
       in case Text.breakOn "=" trimmed of
            (k, rest)
              | Text.null k -> []
              | Text.null rest -> []
              | otherwise ->
                  let afterEq = Text.drop 1 rest
                   in case Text.breakOn ";" afterEq of
                        (val, meta)
                          | Text.null meta -> [(k, BaggageEntry val Nothing)]
                          | otherwise ->
                              [(k, BaggageEntry val (Just (Text.drop 1 meta)))]


-------------------------------------------------------------------------------
-- Hex helpers
-------------------------------------------------------------------------------

word8ToHex :: Word8 -> Text
word8ToHex w = Text.pack [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
  where
    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10 = toEnum (fromIntegral n + fromEnum '0')
      | otherwise = toEnum (fromIntegral n - 10 + fromEnum 'a')


parseHexByte :: Text -> Maybe Word8
parseHexByte t
  | Text.length t == 2 = do
      hi <- hexVal (Text.index t 0)
      lo <- hexVal (Text.index t 1)
      pure (hi * 16 + lo)
  | otherwise = Nothing
  where
    hexVal :: Char -> Maybe Word8
    hexVal c
      | c >= '0' && c <= '9' = Just (fromIntegral (fromEnum c - fromEnum '0'))
      | c >= 'a' && c <= 'f' = Just (fromIntegral (fromEnum c - fromEnum 'a' + 10))
      | c >= 'A' && c <= 'F' = Just (fromIntegral (fromEnum c - fromEnum 'A' + 10))
      | otherwise = Nothing
