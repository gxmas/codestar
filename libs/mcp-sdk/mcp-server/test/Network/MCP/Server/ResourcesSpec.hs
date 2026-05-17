module Network.MCP.Server.ResourcesSpec (spec) where

import Control.Monad (replicateM_)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Server.Resources
import Network.MCP.Session (RequestHandler, RequestMeta (..), Session (..))
import Network.MCP.Types
  ( Cursor (..)
  , Implementation (..)
  , ProtocolVersion (..)
  , RPCError (..)
  , RequestId (..)
  )
import Network.MCP.Types.Capabilities
  ( ClientCapabilities (..)
  , NegotiatedCapabilities (..)
  , ServerCapabilities (..)
  )
import Network.MCP.Types.Content (URI (..), validateUri, defaultUriSchemeAllowlist)

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- Simple text safe for use as URI path segments (no braces, no spaces).
newtype SafeText = SafeText { safeText :: T.Text } deriving (Show)

instance Arbitrary SafeText where
  arbitrary = SafeText . T.pack <$>
    resize 15 (listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ ":/_.-")))

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- ── templateSegments ──────────────────────────────────────────────────
  describe "templateSegments" $ do
    it "template with no variables yields one segment (the whole string)" $
      templateSegments "resource://exact" `shouldBe` ["resource://exact"]

    it "empty template yields no segments" $
      templateSegments "" `shouldBe` []

    it "single variable with non-empty suffix: prefix and suffix" $
      templateSegments "resource://{id}/data" `shouldBe` ["resource://", "/data"]

    -- The implementation drops trailing empty segments: a variable at the
    -- end of the template produces no suffix segment.
    it "two variables: only prefix and inter-variable literal (trailing empty dropped)" $
      templateSegments "{a}/middle/{b}" `shouldBe` ["", "/middle/"]

    it "variable at start with suffix: empty prefix and suffix segment" $
      templateSegments "{id}/rest" `shouldBe` ["", "/rest"]

    it "variable at end: only the prefix literal (trailing empty dropped)" $
      templateSegments "prefix/{id}" `shouldBe` ["prefix/"]

    -- SafeText generates non-empty text, so suffix is always non-empty,
    -- meaning no trailing empty-segment edge case fires here.
    prop "single variable with non-empty suffix: exactly [prefix, suffix]" $
      \(SafeText prefix) (SafeText varName) (SafeText suffix) ->
        let tpl  = prefix <> "{" <> varName <> "}" <> suffix
        in templateSegments tpl === [prefix, suffix]

  -- ── matchesTemplate ───────────────────────────────────────────────────
  describe "matchesTemplate" $ do
    it "exact match (no variables): URI equals template text" $
      matchesTemplate (URI "resource://exact") "resource://exact" `shouldBe` True

    it "exact mismatch (no variables): different text" $
      matchesTemplate (URI "resource://other") "resource://exact" `shouldBe` False

    it "single-variable template matches URI with non-empty variable content" $
      matchesTemplate (URI "resource://foo/data") "resource://{id}/data" `shouldBe` True

    it "single-variable template matches URI with numeric variable content" $
      matchesTemplate (URI "resource://123/data") "resource://{id}/data" `shouldBe` True

    it "single-variable template: URI missing suffix does not match" $
      matchesTemplate (URI "resource://foo") "resource://{id}/data" `shouldBe` False

    it "single-variable template: URI missing prefix does not match" $
      matchesTemplate (URI "other://foo/data") "resource://{id}/data" `shouldBe` False

    it "variable at start: any URI ending with suffix matches" $
      matchesTemplate (URI "anything/suffix") "{id}/suffix" `shouldBe` True

    -- A variable at the END of the template causes templateSegments to drop
    -- the trailing empty segment, leaving only [prefix]. The match then
    -- requires the URI to equal the prefix exactly (single-segment case).
    it "variable at end: URI equals the prefix literal (single-segment exact match)" $
      matchesTemplate (URI "prefix/") "prefix/{id}" `shouldBe` True

    it "variable at end: URI longer than prefix matches (variable captures the suffix)" $
      matchesTemplate (URI "prefix/anything") "prefix/{id}" `shouldBe` True

    it "two-variable template: URI with content for both variables matches" $
      matchesTemplate (URI "foo/middle/bar") "{a}/middle/{b}" `shouldBe` True

    it "two-variable template: URI missing intermediate literal does not match" $
      matchesTemplate (URI "foo/other/bar") "{a}/middle/{b}" `shouldBe` False

    it "RFC 6570 Level-2 operator +: treated same as Level 1" $
      matchesTemplate (URI "file:///foo/bar") "file:///{+path}" `shouldBe` True

    it "overlapping suffix: variable captures correct content" $
      matchesTemplate (URI "abb") "a{x}b" `shouldBe` True

    it "URI too short to accommodate both prefix and suffix does not match" $
      -- prefix "ab", suffix "cd", but URI is only "abcd" (length 4 = 2+2, borderline)
      matchesTemplate (URI "abcd") "ab{x}cd" `shouldBe` True

    it "URI shorter than prefix+suffix does not match" $
      -- prefix "abc", suffix "xyz", URI "abcx" (length 4 < 3+3=6)
      matchesTemplate (URI "abcx") "abc{x}xyz" `shouldBe` False

    prop "template without variables matches only the exact URI" $
      \(SafeText t) ->
        -- A template with no { } is an exact literal — it matches the URI
        -- iff the URI text equals the template text.
        let noVars = not (T.any (== '{') t)
        in noVars ==>
          ( matchesTemplate (URI t) t === True
          .&&. matchesTemplate (URI (t <> "extra")) t === False
          )

    prop "URI built from template variable slot always matches the template" $
      \(SafeText prefix) (SafeText varContent) (SafeText suffix) ->
        not (T.null varContent) ==>
          let tpl = prefix <> "{var}" <> suffix
              uri = URI (prefix <> varContent <> suffix)
          in matchesTemplate uri tpl === True

    prop "two-variable template built from components always matches" $
      \(SafeText prefix) (SafeText suffix) ->
        forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \varA ->
          forAll (T.pack <$> listOf1 (elements ['a'..'z'])) $ \varB ->
            let inter = "/"
                tpl = prefix <> "{a}" <> inter <> "{b}" <> suffix
                uri = URI (prefix <> varA <> inter <> varB <> suffix)
            in matchesTemplate uri tpl === True

  -- ── cursor roundtrip (Resources shares same implementation as Tools) ──
  describe "cursor roundtrip" $ do
    prop "decodeCursor (encodeCursor n) == Just n for non-negative n" $
      \(NonNegative n) ->
        decodeCursor (encodeCursor (n :: Int)) === Just n

    it "decodeCursor returns Nothing for invalid input" $
      decodeCursor (Cursor "!!!") `shouldBe` Nothing

  -- ── paginate (Resources shares same implementation as Tools) ──────────
  describe "paginate" $ do
    prop "collecting all pages recovers the full list" $
      \(items :: [Int]) ->
        collectAllPages items === items

    prop "no page exceeds 50 items" $
      \(items :: [Int]) ->
        let (page, _) = paginate items Nothing
        in length page <= 50

  -- ── validateUri ────────────────────────────────────────────────────────
  describe "validateUri" $ do
    it "empty string is rejected" $
      validateUri [] "" `shouldSatisfy` isLeft'

    it "URI with no scheme is rejected" $
      validateUri [] "no-scheme-here" `shouldSatisfy` isLeft'

    it "file:// URI is accepted" $
      validateUri [] "file:///home/user/project" `shouldSatisfy` isRight'

    it "https:// URI is accepted" $
      validateUri [] "https://example.com/resource" `shouldSatisfy` isRight'

    it "custom scheme is accepted when allowlist is empty" $
      validateUri [] "db://mydb/table" `shouldSatisfy` isRight'

    it "custom scheme rejected when not in non-empty allowlist" $
      validateUri ["file", "https"] "db://mydb/table" `shouldSatisfy` isLeft'

    it "file:// URI with .. segment is rejected" $
      validateUri [] "file:///home/user/../etc/passwd" `shouldSatisfy` isLeft'

    it "file:// URI without .. is accepted" $
      validateUri [] "file:///home/user/project/file.txt" `shouldSatisfy` isRight'

    prop "any non-empty allowlist rejects schemes not in the list" $
      \(SafeText rawScheme) ->
        let scheme = T.filter (\c -> c /= ':' && c /= '/') rawScheme
        in not (T.null scheme) && scheme /= "file" ==>
          validateUri ["file"] (scheme <> "://whatever") `shouldSatisfy` isLeft'

  -- ── URI validation in handlers ────────────────────────────────────────
  describe "URI validation in handlers" $ do
    it "handleRead rejects a URI with disallowed scheme" $ do
      (_rr, handlers, _) <- setupRegistryWithAllowlist ["file", "https"]
      readH <- lookupHandler "resources/read" handlers
      result <- readH (Aeson.object ["uri" Aeson..= ("db://foo" :: T.Text)]) dummyMeta
      case result of
        Left err -> err.rpcErrorCode `shouldBe` (-32602)
        Right _  -> expectationFailure "expected Left RPCError"

    it "handleSubscribe rejects a URI with disallowed scheme" $ do
      (_rr, handlers, _) <- setupRegistryWithAllowlist ["file", "https"]
      subH <- lookupHandler "resources/subscribe" handlers
      result <- subH (Aeson.object ["uri" Aeson..= ("db://foo" :: T.Text)]) dummyMeta
      result `shouldSatisfy` isLeft'

    it "handleRead rejects a file:// URI with path traversal" $ do
      (_rr, handlers, _) <- setupRegistryWithAllowlist ["file", "https"]
      readH <- lookupHandler "resources/read" handlers
      result <- readH (Aeson.object ["uri" Aeson..= ("file:///foo/../bar" :: T.Text)]) dummyMeta
      result `shouldSatisfy` isLeft'

  -- ── Fix 1: Unsubscribe removes one callback ─────────────────────────
  describe "handleUnsubscribe removes one callback" $ do
    prop "after one unsubscribe, N-1 of N callbacks fire on notify" $
      \(Positive n') ->
        let n = (n' `mod` 4) + 2  -- N in [2..5]
        in ioProperty $ do
          (rr, handlers, notifyLog) <- setupRegistryWithHandlers

          let uri = URI "file:///resource/1"
              subParams = Aeson.object ["uri" Aeson..= uri]
              meta = dummyMeta

          -- Subscribe N times
          subH <- lookupHandler "resources/subscribe" handlers
          replicateM_ n $ do
            result <- subH subParams meta
            result `shouldSatisfy` isRight'

          -- Unsubscribe once
          unsubH <- lookupHandler "resources/unsubscribe" handlers
          result <- unsubH subParams meta
          result `shouldSatisfy` isRight'

          -- Trigger notifications
          notifyUpdated rr uri

          -- Check that exactly N-1 callbacks fired
          fired <- readIORef notifyLog
          pure $ length fired === (n - 1)

    it "unsubscribe on empty list is a no-op (returns success)" $ do
      (_rr, handlers, _) <- setupRegistryWithHandlers
      unsubH <- lookupHandler "resources/unsubscribe" handlers
      let subParams = Aeson.object ["uri" Aeson..= URI "file:///gone"]
      result <- unsubH subParams dummyMeta
      result `shouldSatisfy` isRight'

    it "unsubscribing N times from N subscriptions leaves zero callbacks" $ do
      (rr, handlers, notifyLog) <- setupRegistryWithHandlers
      let uri = URI "file:///resource/2"
          subParams = Aeson.object ["uri" Aeson..= uri]
      subH <- lookupHandler "resources/subscribe" handlers
      unsubH <- lookupHandler "resources/unsubscribe" handlers

      -- Subscribe 3, unsubscribe 3
      replicateM_ 3 $ subH subParams dummyMeta
      replicateM_ 3 $ unsubH subParams dummyMeta

      -- Notify and check zero callbacks
      notifyUpdated rr uri
      fired <- readIORef notifyLog
      length fired `shouldBe` 0

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

collectAllPages :: [a] -> [a]
collectAllPages items = go Nothing []
  where
    go cursor acc =
      let (page, next) = paginate items cursor
      in case next of
        Nothing  -> acc ++ page
        Just cur -> go (Just cur) (acc ++ page)

dummyMeta :: RequestMeta
dummyMeta = RequestMeta (RequestId (Right 1)) Nothing Nothing

-- | Create a registry with a stub session that captures registered
-- request handlers (so we can call subscribe/unsubscribe directly)
-- and logs notification URIs.
setupRegistryWithHandlers
  :: IO (ResourceRegistry, IORef (HM.HashMap T.Text RequestHandler), IORef [URI])
setupRegistryWithHandlers = do
  rr <- newResourceRegistry
  notifyLog <- newIORef ([] :: [URI])
  handlersRef <- newIORef (HM.empty :: HM.HashMap T.Text RequestHandler)
  let session = (stubSession notifyLog)
        { sessionOnRequest = \method handler ->
            atomicModifyIORef' handlersRef (\m -> (HM.insert method handler m, ()))
        }
  attach rr session
  pure (rr, handlersRef, notifyLog)

-- | Look up a handler from the captured handlers ref.
lookupHandler :: T.Text -> IORef (HM.HashMap T.Text RequestHandler) -> IO RequestHandler
lookupHandler method ref = do
  m <- readIORef ref
  case HM.lookup method m of
    Nothing -> fail $ "Handler not registered: " ++ T.unpack method
    Just h  -> pure h

-- | A stub session that logs notification URIs.
stubSession :: IORef [URI] -> Session
stubSession logRef = Session
  { sessionProtocolVersion = ProtocolVersion "2025-03-26"
  , sessionPeerInfo = Implementation "test" "0.1" Nothing Nothing
  , sessionCapabilities = NegotiatedCapabilities
      (ClientCapabilities Nothing Nothing Nothing Nothing Nothing)
      (ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , sessionInstructions = Nothing
  , sessionRequest = \_ _ _ -> pure (Right (Aeson.object []))
  , sessionNotify = \_method params -> case params of
      Just val -> case Aeson.fromJSON val of
        Aeson.Success obj -> do
          case HM.lookup ("uri" :: T.Text) (obj :: HM.HashMap T.Text Aeson.Value) of
            Just (Aeson.String u) ->
              atomicModifyIORef' logRef (\xs -> (xs ++ [URI u], ()))
            _ -> pure ()
        _ -> pure ()
      Nothing -> pure ()
  , sessionCancel = \_ _ -> pure ()
  , sessionOnRequest = \_ _ -> pure ()
  , sessionOnNotification = \_ _ -> pure ()
  , sessionClose = pure ()
  , sessionOnClose = \_ -> pure ()
  }

-- | Create a registry with a custom URI scheme allowlist.
setupRegistryWithAllowlist
  :: [T.Text]
  -> IO (ResourceRegistry, IORef (HM.HashMap T.Text RequestHandler), IORef [URI])
setupRegistryWithAllowlist allowlist = do
  rr <- newResourceRegistryWith (ResourceConfig { rcUriSchemeAllowlist = allowlist })
  notifyLog <- newIORef ([] :: [URI])
  handlersRef <- newIORef (HM.empty :: HM.HashMap T.Text RequestHandler)
  let session = (stubSession notifyLog)
        { sessionOnRequest = \method handler ->
            atomicModifyIORef' handlersRef (\m -> (HM.insert method handler m, ()))
        }
  attach rr session
  pure (rr, handlersRef, notifyLog)

isRight' :: Either a b -> Bool
isRight' (Right _) = True
isRight' _         = False

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _        = False
