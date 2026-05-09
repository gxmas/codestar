{-# OPTIONS_GHC -Wno-orphans #-}

module Network.MCP.Transport.HttpSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status200, status400)
import Network.Wai (Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout (timeout)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Transport (Transport (..), TransportError (..), TransportErrorKind (..))
import Network.MCP.Transport.Http (SseEvent (..), parseSseLines, renderSseEvent, new)
import Network.MCP.Types

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- Simple alphanumeric token: no whitespace, no control chars.
-- These constraints ensure parseSseLines (which strips whitespace)
-- is the identity on the text fields we generate.
genToken :: Gen T.Text
genToken = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '_']))

-- Multi-line data value: one or more tokens joined by '\n'.
genMultiLine :: Gen T.Text
genMultiLine = T.intercalate "\n" <$> listOf1 genToken

instance Arbitrary SseEvent where
  arbitrary =
    SseEvent
      <$> oneof [pure Nothing, Just <$> genToken]
      <*> oneof [pure Nothing, Just <$> genToken]
      <*> genMultiLine

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Build a streaming WAI response that writes SSE lines then returns.
sseResponse :: [BS8.ByteString] -> Wai.Response
sseResponse ls =
  Wai.responseStream status200 [("Content-Type", "text/event-stream")] $
    \write flush -> do
      mapM_ (\l -> write (Builder.byteString l <> Builder.char8 '\n')) ls
      flush

-- | WAI response that keeps the connection open indefinitely.
hangResponse :: Wai.Response
hangResponse =
  Wai.responseStream status200 [("Content-Type", "text/event-stream")] $
    \_write _flush -> threadDelay maxBound

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- ── P0: SSE roundtrip ────────────────────────────────────────────────
  describe "SSE roundtrip (P0)" $ do
    prop "renderSseEvent then parseSseLines is identity" $ \(event :: SseEvent) ->
      parseSseLines (renderSseEvent event) === [event]

  -- ── SSE parsing unit tests (existing, P2) ────────────────────────────
  describe "SSE parsing" $ do
    it "parses a simple data event" $ do
      let lines_ = [BS8.pack "data: hello"]
      parseSseLines lines_ `shouldBe`
        [SseEvent Nothing Nothing "hello"]

    it "parses event with id and event type" $ do
      let lines_ = map BS8.pack
            [ "id: 42"
            , "event: message"
            , "data: {\"test\":true}"
            ]
      parseSseLines lines_ `shouldBe`
        [SseEvent (Just "42") (Just "message") "{\"test\":true}"]

    it "multiple data lines are joined with newlines" $ do
      let lines_ = map BS8.pack
            [ "data: line1"
            , "data: line2"
            , "data: line3"
            ]
      parseSseLines lines_ `shouldBe`
        [SseEvent Nothing Nothing "line1\nline2\nline3"]

    it "empty line separates events" $ do
      let lines_ = map BS8.pack
            [ "data: first"
            , ""
            , "data: second"
            ]
      parseSseLines lines_ `shouldBe`
        [ SseEvent Nothing Nothing "first"
        , SseEvent Nothing Nothing "second"
        ]

    it "comment lines (starting with :) are ignored" $ do
      let lines_ = map BS8.pack
            [ ": this is a comment"
            , "data: hello"
            ]
      parseSseLines lines_ `shouldBe`
        [SseEvent Nothing Nothing "hello"]

    it "no data lines produces no event" $ do
      let lines_ = map BS8.pack
            [ "id: 42"
            , "event: ping"
            ]
      parseSseLines lines_ `shouldBe` []

    it "empty input produces no events" $ do
      parseSseLines [] `shouldBe` []

    it "endpoint event type is preserved" $ do
      let lines_ = map BS8.pack
            [ "event: endpoint"
            , "data: /message?session=abc"
            ]
      parseSseLines lines_ `shouldBe`
        [SseEvent Nothing (Just "endpoint") "/message?session=abc"]

    it "strips whitespace from field values" $ do
      let lines_ = [BS8.pack "data:   spaced  "]
      parseSseLines lines_ `shouldBe`
        [SseEvent Nothing Nothing "spaced"]

  -- ── P1: Last-Event-ID sent on reconnect ──────────────────────────────
  describe "Last-Event-ID reconnect (P1)" $ do
    it "includes Last-Event-ID header on reconnect after id-bearing event" $ do
      receivedId <- newEmptyMVar
      requestNum <- newIORef (0 :: Int)

      let app :: Application
          app req respond = do
            n <- atomicModifyIORef' requestNum (\x -> (x + 1, x))
            case n of
              0 ->
                -- First connection: serve one event with an id, then return.
                -- Returning ends the WAI stream, which closes the connection.
                respond $ sseResponse
                  [ "id: reconnect-42"
                  , "data: hello"
                  , ""
                  ]
              _ -> do
                -- Reconnect: capture Last-Event-ID and hang to hold the
                -- connection open so the test can read the MVar cleanly.
                let lid =
                      fmap TE.decodeUtf8
                        (lookup "Last-Event-ID" (Wai.requestHeaders req))
                _ <- tryPutMVar receivedId lid
                respond hangResponse

      testWithApplication (pure app) $ \port -> do
        let sseUrl = "http://localhost:" <> T.pack (show port) <> "/"
        ht <- new sseUrl
        result <- timeout 5_000_000 (takeMVar receivedId)
        close ht
        result `shouldBe` Just (Just "reconnect-42")

  -- ── HTTP POST: non-2xx surfaces as TransportError ───────────────────
  describe "HTTP POST error handling" $ do
    it "non-2xx POST response surfaces as TransportIoError" $ do
      -- endpointSent is filled by the server once it has flushed the
      -- endpoint event. The test waits for this before calling send,
      -- ensuring htPostUrl is set.
      endpointSent <- newEmptyMVar
      portRef <- newIORef (0 :: Int)

      let app :: Application
          app req respond = do
            p <- readIORef portRef
            case Wai.rawPathInfo req of
              "/sse" ->
                respond $
                  Wai.responseStream status200 [("Content-Type", "text/event-stream")] $
                    \write flush -> do
                      let epUrl =
                            "http://localhost:"
                              <> BS8.pack (show p)
                              <> "/post"
                      write $
                        Builder.byteString "event: endpoint\ndata: "
                          <> Builder.byteString epUrl
                          <> Builder.byteString "\n\n"
                      flush
                      -- Signal that the event has been sent and flushed.
                      putMVar endpointSent ()
                      threadDelay maxBound
              "/post" ->
                respond $ Wai.responseLBS status400 [] "bad request"
              _ ->
                respond $ Wai.responseLBS status400 [] "not found"

      testWithApplication (pure app) $ \port -> do
        writeIORef portRef port
        let sseUrl = "http://localhost:" <> T.pack (show port) <> "/sse"
        ht <- new sseUrl
        -- Wait for the server to flush the endpoint event, then give the
        -- SSE reader thread a moment to parse it and set htPostUrl.
        _ <- timeout 5_000_000 (takeMVar endpointSent)
        threadDelay 50_000
        let msg =
              MCPRequest
                JSONRPCRequest
                  { requestId = RequestId (Right 1)
                  , requestMethod = "ping"
                  , requestParams = Nothing
                  , requestMeta = Nothing
                  }
        result <- send ht msg
        close ht
        case result of
          Left err -> err.transportErrorKind `shouldBe` TransportIoError
          Right () -> expectationFailure "expected Left TransportIoError"
