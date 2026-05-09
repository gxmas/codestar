module Main where

import Data.Char (isSpace)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, hClose, hIsOpen, openTempFile)

import Test.Tasty
import Test.Tasty.HUnit

import OTel.Attribute
  ( Attributes, AttributeValue (..), InstrumentationScope (..)
  , emptyAttributes, fromList
  )
import OTel.Exporter.Console
import OTel.Log (LogBody (..), SeverityNumber (..))
import OTel.SDK.Export (ExportResult (..))
import OTel.SDK.Log.Export
  ( LogRecordExporter (..), ReadableLogRecord (..), SomeReadableLogRecord (..)
  )
import OTel.SDK.Metric.Export
  ( AggregationTemporality (..), InstrumentKind (..)
  , Metric (..), MetricData (..), MetricExporter (..)
  , MetricPointData (..), ScopeMetrics (..), SumData (..)
  )
import OTel.SDK.Resource qualified as Resource
import OTel.SDK.Trace.Export (ReadableSpan (..), SomeReadableSpan (..), SpanExporter (..))
import OTel.Timestamp (fromNanos)
import OTel.Trace (SpanKind (..), SpanStatus (..), StatusCode (..))
import OTel.Trace.SpanContext (invalidSpanContext)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "otel-exporter-console"
  [consoleExporterTests, consoleMetricExporterTests, consoleLogRecordExporterTests]


-------------------------------------------------------------------------------
-- Minimal test span
-------------------------------------------------------------------------------

data TestSpan = TestSpan
  { tsName   :: String
  , tsAttrs  :: Attributes
  , tsKind   :: SpanKind
  , tsStatus :: SpanStatus
  }

instance ReadableSpan TestSpan where
  readSpanContext _ = invalidSpanContext
  readParentSpanContext _ = Nothing
  readName s = read (show (tsName s))
  readKind s = tsKind s
  readStartTimestamp _ = fromNanos 1000000000
  readEndTimestamp _ = fromNanos 2000000000
  readAttributes s = tsAttrs s
  readEvents _ = []
  readLinks _ = []
  readStatus s = tsStatus s
  readResource _ = Resource.empty
  readInstrumentationScope _ =
    InstrumentationScope "test" Nothing Nothing Nothing
  readDroppedAttributesCount _ = 0
  readDroppedEventsCount _ = 0
  readDroppedLinksCount _ = 0

defaultTestSpan :: TestSpan
defaultTestSpan = TestSpan
  { tsName   = "test-span"
  , tsAttrs  = emptyAttributes
  , tsKind   = Internal
  , tsStatus = SpanStatus Unset Nothing
  }

withTempHandle :: (Handle -> FilePath -> IO a) -> IO a
withTempHandle action = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "otel-console-test.txt"
  result <- action h path
  open <- hIsOpen h
  if open then hClose h else pure ()
  removeFile path
  pure result

isSubstringOf :: String -> String -> Bool
isSubstringOf needle haystack = go haystack
  where
    go [] = null needle
    go xs@(_ : rest) = startsWith xs || go rest
    startsWith xs = and (zipWith (==) needle xs) && length needle <= length xs


-------------------------------------------------------------------------------
-- Metric test helpers
-------------------------------------------------------------------------------

emptyMetricData :: MetricData
emptyMetricData = MetricData Resource.empty []

oneMetricData :: MetricData
oneMetricData = MetricData Resource.empty
  [ ScopeMetrics (InstrumentationScope "test" Nothing Nothing Nothing)
      [ Metric "requests" "" "1" (SumPointData (SumData [] Cumulative True)) ]
  ]


-------------------------------------------------------------------------------
-- Log record test helper
-------------------------------------------------------------------------------

data TestLogRecord = TestLogRecord

instance ReadableLogRecord TestLogRecord where
  rlrTimestamp _ = Nothing
  rlrObservedTimestamp _ = fromNanos 0
  rlrSeverityNumber _ = Just SeverityInfo
  rlrSeverityText _ = Just "INFO"
  rlrBody _ = Just (LogBodyString "test message")
  rlrAttributes _ = emptyAttributes
  rlrDroppedAttributes _ = 0
  rlrSpanContext _ = Nothing
  rlrResource _ = Resource.empty
  rlrScope _ = InstrumentationScope "test" Nothing Nothing Nothing

testLogRecord :: SomeReadableLogRecord
testLogRecord = SomeReadableLogRecord TestLogRecord


-------------------------------------------------------------------------------
-- ConsoleSpanExporter tests
-------------------------------------------------------------------------------

consoleExporterTests :: TestTree
consoleExporterTests = testGroup "ConsoleSpanExporter"
  [ testCase "newConsoleSpanExporter creates an exporter" $ do
      _ <- newConsoleSpanExporter
      pure ()

  , testCase "exportSpans with empty list returns ExportSuccess" $ do
      e <- newConsoleSpanExporter
      result <- exportSpans e []
      result @?= ExportSuccess

  , testCase "exportSpans with one span returns ExportSuccess" $
      withTempHandle $ \h _path -> do
        e <- newConsoleSpanExporterWith h
        result <- exportSpans e [SomeReadableSpan defaultTestSpan]
        result @?= ExportSuccess

  , testCase "exportSpans with multiple spans returns ExportSuccess" $
      withTempHandle $ \h _path -> do
        e <- newConsoleSpanExporterWith h
        let spans = map (\n -> SomeReadableSpan defaultTestSpan { tsName = "s" <> show (n :: Int) }) [1..5]
        result <- exportSpans e spans
        result @?= ExportSuccess

  , testCase "output is non-empty when a span is exported" $
      withTempHandle $ \h path -> do
        e <- newConsoleSpanExporterWith h
        _ <- exportSpans e [SomeReadableSpan defaultTestSpan]
        hClose h
        content <- readFile path
        assertBool "output should be non-empty" (not (all isSpace content))

  , testCase "output contains span name" $
      withTempHandle $ \h path -> do
        e <- newConsoleSpanExporterWith h
        _ <- exportSpans e [SomeReadableSpan defaultTestSpan { tsName = "my-named-span" }]
        hClose h
        content <- readFile path
        assertBool "output should contain span name" ("my-named-span" `isSubstringOf` content)

  , testCase "output contains attribute key" $
      withTempHandle $ \h path -> do
        e <- newConsoleSpanExporterWith h
        let attrs = fromList [("http.method", StringValue "GET")]
        _ <- exportSpans e [SomeReadableSpan defaultTestSpan { tsAttrs = attrs }]
        hClose h
        content <- readFile path
        assertBool "output should contain attribute key" ("http.method" `isSubstringOf` content)

  , testCase "output contains Server kind" $
      withTempHandle $ \h path -> do
        e <- newConsoleSpanExporterWith h
        _ <- exportSpans e [SomeReadableSpan defaultTestSpan { tsKind = Server }]
        hClose h
        content <- readFile path
        assertBool "output should contain Server" ("Server" `isSubstringOf` content)

  , testCase "output contains Error status" $
      withTempHandle $ \h path -> do
        e <- newConsoleSpanExporterWith h
        _ <- exportSpans e [SomeReadableSpan defaultTestSpan { tsStatus = SpanStatus Error (Just "oops") }]
        hClose h
        content <- readFile path
        assertBool "output should contain Error" ("Error" `isSubstringOf` content)

  , testCase "shutdownExporter returns Right ()" $ do
      e <- newConsoleSpanExporter
      result <- shutdownExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushExporter returns Right ()" $ do
      e <- newConsoleSpanExporter
      result <- forceFlushExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)
  ]


-------------------------------------------------------------------------------
-- ConsoleMetricExporter tests
-------------------------------------------------------------------------------

consoleMetricExporterTests :: TestTree
consoleMetricExporterTests = testGroup "ConsoleMetricExporter"
  [ testCase "exportMetrics returns ExportSuccess for empty MetricData" $
      withTempHandle $ \h _path -> do
        e <- newConsoleMetricExporterWith h
        result <- exportMetrics e emptyMetricData
        result @?= ExportSuccess

  , testCase "exportMetrics returns ExportSuccess for MetricData with one SumData metric" $
      withTempHandle $ \h _path -> do
        e <- newConsoleMetricExporterWith h
        result <- exportMetrics e oneMetricData
        result @?= ExportSuccess

  , testCase "shutdownMetricExporter returns Right ()" $ do
      e <- newConsoleMetricExporter
      result <- shutdownMetricExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushMetricExporter returns Right ()" $ do
      e <- newConsoleMetricExporter
      result <- forceFlushMetricExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "exporterTemporality returns Cumulative for all instrument kinds" $
      withTempHandle $ \h _path -> do
        e <- newConsoleMetricExporterWith h
        let allKinds = [minBound .. maxBound] :: [InstrumentKind]
        mapM_ (\k -> exporterTemporality e k @?= Cumulative) allKinds

  , testCase "output is non-empty after exporting a metric" $
      withTempHandle $ \h path -> do
        e <- newConsoleMetricExporterWith h
        _ <- exportMetrics e oneMetricData
        hClose h
        content <- readFile path
        assertBool "output should be non-empty" (not (all isSpace content))
  ]


-------------------------------------------------------------------------------
-- ConsoleLogRecordExporter tests
-------------------------------------------------------------------------------

consoleLogRecordExporterTests :: TestTree
consoleLogRecordExporterTests = testGroup "ConsoleLogRecordExporter"
  [ testCase "exportLogRecords with empty list returns ExportSuccess" $
      withTempHandle $ \h _path -> do
        e <- newConsoleLogRecordExporterWith h
        result <- exportLogRecords e []
        result @?= ExportSuccess

  , testCase "exportLogRecords with a synthetic SomeReadableLogRecord returns ExportSuccess" $
      withTempHandle $ \h _path -> do
        e <- newConsoleLogRecordExporterWith h
        result <- exportLogRecords e [testLogRecord]
        result @?= ExportSuccess

  , testCase "shutdownLogExporter returns Right ()" $ do
      e <- newConsoleLogRecordExporter
      result <- shutdownLogExporter e
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)

  , testCase "forceFlushLogExporter returns Right ()" $ do
      e <- newConsoleLogRecordExporter
      result <- forceFlushLogExporter e Nothing
      case result of
        Right () -> pure ()
        Left err -> assertFailure ("expected Right (), got Left: " <> show err)
  ]
