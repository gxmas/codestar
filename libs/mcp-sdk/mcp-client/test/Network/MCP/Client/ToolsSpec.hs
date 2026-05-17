{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.MCP.Client.ToolsSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Client.Resources
import Network.MCP.Client.Tools
import Network.MCP.Session
import Network.MCP.Types
import Network.MCP.Types.Capabilities

------------------------------------------------------------------------
-- ToJSON orphan instances (for mock server response encoding)
------------------------------------------------------------------------

instance Aeson.ToJSON ClientToolDef where
  toJSON t = Aeson.object $
    [ "name"        Aeson..= t.ctdName
    , "description" Aeson..= t.ctdDescription
    , "inputSchema" Aeson..= t.ctdInputSchema
    ]
      ++ maybe [] (\a -> ["annotations" Aeson..= a]) t.ctdAnnotations

instance Aeson.ToJSON ClientResourceDef where
  toJSON r = Aeson.object
    [ "uri"         Aeson..= r.crdUri
    , "name"        Aeson..= r.crdName
    , "description" Aeson..= r.crdDescription
    , "mimeType"    Aeson..= r.crdMimeType
    ]

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary ClientToolDef where
  arbitrary =
    ClientToolDef
      <$> (T.pack <$> listOf1 (elements ['a' .. 'z']))
      <*> oneof [pure Nothing, Just . T.pack <$> listOf1 (elements ['a' .. 'z'])]
      <*> pure (Aeson.object [])
      <*> pure Nothing

instance Arbitrary ClientResourceDef where
  arbitrary =
    ClientResourceDef
      <$> (T.pack <$> listOf1 (elements ['a' .. 'z']))
      <*> (T.pack <$> listOf1 (elements ['a' .. 'z']))
      <*> pure Nothing
      <*> pure Nothing

------------------------------------------------------------------------
-- Cursor helpers (mirrors the server's simple text-encoding)
------------------------------------------------------------------------

encodeOffset :: Int -> Cursor
encodeOffset n = Cursor (T.pack (show n))

decodeOffset :: Cursor -> Int
decodeOffset (Cursor t) = case reads (T.unpack t) of
  [(n, "")] -> n
  _         -> 0

extractCursor :: Maybe Aeson.Value -> Maybe Cursor
extractCursor mv = do
  v <- mv
  parseMaybe (Aeson.withObject "p" (Aeson..:? "cursor")) v >>= id

------------------------------------------------------------------------
-- Mock Session
------------------------------------------------------------------------

mockToolSession :: [ClientToolDef] -> Int -> Session
mockToolSession allTools pgSize = emptySession
  { sessionRequest = \method params _opts ->
      case method of
        "tools/list" -> do
          let offset  = maybe 0 decodeOffset (extractCursor params)
              page    = take pgSize (drop offset allTools)
              nextOff = offset + length page
              nextCur = if nextOff < length allTools
                          then Just (encodeOffset nextOff)
                          else Nothing
              obj = Aeson.object $
                ["tools" Aeson..= page]
                ++ maybe [] (\c -> ["nextCursor" Aeson..= c]) nextCur
          pure (Right obj)
        "tools/call" ->
          case params >>= parseMaybe (Aeson.withObject "p" (Aeson..: "name")) of
            Nothing   -> pure (Left (RPCError (-32602) "bad params" Nothing))
            Just name ->
              if any (\t -> t.ctdName == name) allTools
                then pure $ Right $ Aeson.object
                       [ "content" Aeson..= ([] :: [Aeson.Value])
                       , "isError"  Aeson..= False
                       ]
                else pure (Left (RPCError (-32602) ("not found: " <> name) Nothing))
        _ -> pure (Left (RPCError (-32601) "Method not found" Nothing))
  }

mockResourceSession :: [ClientResourceDef] -> Int -> Session
mockResourceSession allResources pgSize = emptySession
  { sessionRequest = \method params _opts ->
      case method of
        "resources/list" -> do
          let offset  = maybe 0 decodeOffset (extractCursor params)
              page    = take pgSize (drop offset allResources)
              nextOff = offset + length page
              nextCur = if nextOff < length allResources
                          then Just (encodeOffset nextOff)
                          else Nothing
              obj = Aeson.object $
                ["resources" Aeson..= page]
                ++ maybe [] (\c -> ["nextCursor" Aeson..= c]) nextCur
          pure (Right obj)
        "resources/read" ->
          case params >>= parseMaybe (Aeson.withObject "p" (Aeson..: "uri")) of
            Nothing  -> pure (Left (RPCError (-32602) "bad params" Nothing))
            Just uri ->
              if any (\r -> r.crdUri == uri) allResources
                then pure $ Right $ Aeson.object
                       [ "contents" Aeson..= ([] :: [Aeson.Value]) ]
                else pure (Left (RPCError (-32602) ("not found: " <> uri) Nothing))
        _ -> pure (Left (RPCError (-32601) "Method not found" Nothing))
  }

emptySession :: Session
emptySession = Session
  { sessionProtocolVersion  = ProtocolVersion "2025-03-26"
  , sessionPeerInfo         = Implementation "mock" "0.1" Nothing Nothing
  , sessionCapabilities     = NegotiatedCapabilities
      (ClientCapabilities Nothing Nothing Nothing Nothing Nothing)
      (ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , sessionInstructions     = Nothing
  , sessionRequest          = \_ _ _ -> pure (Left (RPCError (-32601) "not implemented" Nothing))
  , sessionNotify           = \_ _ -> pure ()
  , sessionCancel           = \_ _ -> pure ()
  , sessionOnRequest        = \_ _ -> pure ()
  , sessionOnNotification   = \_ _ -> pure ()
  , sessionClose            = pure ()
  , sessionOnClose          = \_ -> pure ()
  }

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "listTools" $ do
    prop "returns all tools registered on the server" $
      \(tools :: [ClientToolDef]) -> ioProperty $ do
        let session = mockToolSession tools 50
        result <- listTools session
        pure $ case result of
          Left _    -> property False
          Right got -> length got === length tools .&&. got === tools

    prop "with page-size-1 server, pagination accumulates all tools" $
      \(tools :: [ClientToolDef]) -> ioProperty $ do
        let session = mockToolSession tools 1
        result <- listTools session
        pure $ case result of
          Left _    -> property False
          Right got -> got === tools

    it "returns empty list when no tools registered" $ do
      result <- listTools (mockToolSession [] 50)
      result `shouldBe` Right []

  describe "callTool" $ do
    it "returns Right ToolCallResult for a registered tool" $ do
      let tool    = ClientToolDef "my_tool" Nothing (Aeson.object []) Nothing
          session = mockToolSession [tool] 50
      result <- callTool session "my_tool" (Aeson.object [])
      case result of
        Right r -> r.tcrIsError `shouldBe` False
        Left e  -> expectationFailure $ "expected Right, got Left: " <> show e

    it "returns Left RPCError for an unregistered tool name" $ do
      result <- callTool (mockToolSession [] 50) "nonexistent" (Aeson.object [])
      case result of
        Left err -> err.rpcErrorCode `shouldBe` (-32602)
        Right _  -> expectationFailure "expected Left RPCError"

    prop "any registered tool can be called successfully" $
      \(tool :: ClientToolDef) -> ioProperty $ do
        let session = mockToolSession [tool] 50
        result <- callTool session tool.ctdName (Aeson.object [])
        pure $ case result of
          Right r -> r.tcrIsError === False
          Left _  -> property False

  describe "listResources" $ do
    prop "returns all resources registered on the server" $
      \(resources :: [ClientResourceDef]) -> ioProperty $ do
        let session = mockResourceSession resources 50
        result <- listResources session
        pure $ case result of
          Left _    -> property False
          Right got -> length got === length resources .&&. got === resources

    prop "with page-size-1 server, pagination accumulates all resources" $
      \(resources :: [ClientResourceDef]) -> ioProperty $ do
        let session = mockResourceSession resources 1
        result <- listResources session
        pure $ case result of
          Left _    -> property False
          Right got -> got === resources

  describe "readResource" $ do
    it "returns Right for a known URI" $ do
      let resource = ClientResourceDef "file:///test" "test" Nothing Nothing
      result <- readResource (mockResourceSession [resource] 50) "file:///test"
      case result of
        Right _  -> pure ()
        Left err -> expectationFailure $ "expected Right: " <> show err

    it "returns Left RPCError for an unknown URI" $ do
      result <- readResource (mockResourceSession [] 50) "file:///unknown"
      case result of
        Left err -> err.rpcErrorCode `shouldBe` (-32602)
        Right _  -> expectationFailure "expected Left RPCError"
