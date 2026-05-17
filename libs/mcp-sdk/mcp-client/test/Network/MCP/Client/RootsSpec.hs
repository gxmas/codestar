module Network.MCP.Client.RootsSpec (spec) where

import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Client.Roots
import Network.MCP.Session (RequestHandler, RequestMeta (..), Session (..))
import Network.MCP.Types
  ( Implementation (..)
  , ProtocolVersion (..)
  , RequestId (..)
  )
import Network.MCP.Types.Capabilities
  ( ClientCapabilities (..)
  , NegotiatedCapabilities (..)
  , ServerCapabilities (..)
  )
import Network.MCP.Types.Content (URI (..))

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Create a roots feature, attach to a stub session, and return
-- the handler registered for "roots/list".
setupRoots :: [Root] -> IO RequestHandler
setupRoots roots = do
  feat <- newRootsFeature (RootsProvider (pure roots))
  handlersRef <- newIORef (HM.empty :: HM.HashMap T.Text RequestHandler)
  let session = stubSession
        { sessionOnRequest = \method handler ->
            atomicModifyIORef' handlersRef (\m -> (HM.insert method handler m, ()))
        }
  attach feat session
  m <- readIORef handlersRef
  case HM.lookup "roots/list" m of
    Nothing -> fail "roots/list handler was not registered"
    Just h  -> pure h

stubSession :: Session
stubSession = Session
  { sessionProtocolVersion = ProtocolVersion "2025-03-26"
  , sessionPeerInfo = Implementation "test" "0.1" Nothing Nothing
  , sessionCapabilities = NegotiatedCapabilities
      (ClientCapabilities Nothing Nothing Nothing Nothing Nothing)
      (ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  , sessionInstructions = Nothing
  , sessionRequest = \_ _ _ -> pure (Right (Aeson.object []))
  , sessionNotify = \_ _ -> pure ()
  , sessionCancel = \_ _ -> pure ()
  , sessionOnRequest = \_ _ -> pure ()
  , sessionOnNotification = \_ _ -> pure ()
  , sessionClose = pure ()
  , sessionOnClose = \_ -> pure ()
  }

dummyMeta :: RequestMeta
dummyMeta = RequestMeta (RequestId (Right 1)) Nothing Nothing

-- | Extract the list of root URI texts from a successful handler response.
extractRootUris :: Either a Aeson.Value -> [T.Text]
extractRootUris (Left _) = []
extractRootUris (Right val) =
  case val of
    Aeson.Object o ->
      case KM.lookup "roots" o of
        Just (Aeson.Array arr) ->
          [ uri | Aeson.Object r <- foldr (:) [] arr
                , Just (Aeson.String uri) <- [KM.lookup "uri" r]
          ]
        _ -> []
    _ -> []

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "roots/list filtering" $ do
  it "returns only file:// roots" $ do
    h <- setupRoots
      [ Root (URI "file:///home") Nothing
      , Root (URI "https://example.com") Nothing
      ]
    result <- h (Aeson.object []) dummyMeta
    let uris = extractRootUris result
    uris `shouldBe` ["file:///home"]

  it "filters out path-traversal roots" $ do
    h <- setupRoots
      [ Root (URI "file:///home/../etc") Nothing
      , Root (URI "file:///home/user") Nothing
      ]
    result <- h (Aeson.object []) dummyMeta
    let uris = extractRootUris result
    uris `shouldBe` ["file:///home/user"]

  it "returns empty list when all roots are invalid" $ do
    h <- setupRoots
      [ Root (URI "https://example.com") Nothing
      , Root (URI "http://other.org") Nothing
      ]
    result <- h (Aeson.object []) dummyMeta
    let uris = extractRootUris result
    uris `shouldBe` []

  it "returns all roots when all are valid file:// URIs" $ do
    h <- setupRoots
      [ Root (URI "file:///a") Nothing
      , Root (URI "file:///b") Nothing
      , Root (URI "file:///c") Nothing
      ]
    result <- h (Aeson.object []) dummyMeta
    let uris = extractRootUris result
    length uris `shouldBe` 3

  prop "any non-file:// root is always filtered" $
    \(NonEmptyScheme scheme) ->
      scheme /= "file" ==> ioProperty $ do
        let uri = URI (T.pack scheme <> "://whatever")
        h <- setupRoots [Root uri Nothing, Root (URI "file:///valid") Nothing]
        result <- h (Aeson.object []) dummyMeta
        let uris = extractRootUris result
        pure $ uris === ["file:///valid"]

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- | Non-empty alphabetic scheme text (no colons).
newtype NonEmptyScheme = NonEmptyScheme String deriving (Show)

instance Arbitrary NonEmptyScheme where
  arbitrary = NonEmptyScheme <$>
    oneof [ pure s | s <- ["http", "https", "ftp", "db", "git"] ]
