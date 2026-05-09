module CodeStar.StorageSpec (spec) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (IOException, catch)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import CodeStar.Storage
import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Model-based command type
-- --------------------------------------------------------------------

-- Small fixed alphabets force collisions and state interactions.
namespaces :: [Text]
namespaces = ["a", "b"]

keys :: [Text]
keys = ["x", "y", "z"]

data Cmd
  = CmdPut    Text Text ByteString
  | CmdGet    Text Text
  | CmdDelete Text Text
  | CmdList   Text
  deriving (Show, Eq)

data Obs
  = ObsPutOk
  | ObsGetOk    ByteString
  | ObsGetMiss
  | ObsDeleteOk
  | ObsListOk   [Text]       -- always sorted
  deriving (Show, Eq)

instance Arbitrary Cmd where
  arbitrary = oneof
    [ CmdPut    <$> genNs' <*> genKey' <*> genPayload
    , CmdGet    <$> genNs' <*> genKey'
    , CmdDelete <$> genNs' <*> genKey'
    , CmdList   <$> genNs'
    ]
    where
      genNs'  = elements namespaces
      genKey' = elements keys

  -- Shrink toward simpler commands and smaller alphabets.
  shrink (CmdPut ns k v) =
    -- A read or delete is structurally simpler than a write.
    [CmdGet ns k, CmdDelete ns k] ++
    [CmdPut ns' k  v  | ns' <- shrinkNs ns] ++
    [CmdPut ns  k' v  | k'  <- shrinkKey k] ++
    [CmdPut ns  k  v' | v'  <- shrinkBS v]
  shrink (CmdGet ns k) =
    [CmdGet ns' k | ns' <- shrinkNs ns] ++
    [CmdGet ns k' | k'  <- shrinkKey k]
  shrink (CmdDelete ns k) =
    [CmdGet ns k] ++
    [CmdDelete ns' k | ns' <- shrinkNs ns] ++
    [CmdDelete ns k' | k'  <- shrinkKey k]
  shrink (CmdList ns) =
    [CmdList ns' | ns' <- shrinkNs ns]

shrinkNs :: Text -> [Text]
shrinkNs "b" = ["a"]
shrinkNs _   = []

shrinkKey :: Text -> [Text]
shrinkKey "z" = ["x", "y"]
shrinkKey "y" = ["x"]
shrinkKey _   = []

shrinkBS :: ByteString -> [ByteString]
shrinkBS = map BS8.pack . filter (not . null) . shrink . BS8.unpack

-- --------------------------------------------------------------------
-- Map-based oracle model
-- --------------------------------------------------------------------

type Model = Map (Text, Text) ByteString

-- Pure model step: returns the observable response and the new state.
modelStep :: Model -> Cmd -> (Obs, Model)
modelStep m (CmdPut ns k v)  = (ObsPutOk, Map.insert (ns, k) v m)
modelStep m (CmdGet ns k)    =
  case Map.lookup (ns, k) m of
    Just v  -> (ObsGetOk v, m)
    Nothing -> (ObsGetMiss, m)
modelStep m (CmdDelete ns k) = (ObsDeleteOk, Map.delete (ns, k) m)
modelStep m (CmdList ns)     =
  let ks = sort [k | (n, k) <- Map.keys m, n == ns]
  in (ObsListOk ks, m)

-- Real backend step: normalise errors so that missing-key deletes
-- still produce ObsDeleteOk (matching the model's always-succeeds
-- delete semantics).
realStep :: StorageBackend -> Cmd -> IO Obs
realStep b (CmdPut ns k v)  = put b ns k v    >> pure ObsPutOk
realStep b (CmdGet ns k)    = get b ns k      >>= \case
  Right v -> pure (ObsGetOk v)
  Left _  -> pure ObsGetMiss
realStep b (CmdDelete ns k) = delete b ns k   >> pure ObsDeleteOk
realStep b (CmdList ns)     = ObsListOk . sort <$> list b ns

-- Run the command sequence against both the real backend and the model.
-- Returns the first disagreement, if any.
runSequence
  :: StorageBackend
  -> [Cmd]
  -> IO (Maybe (Int, Cmd, Obs, Obs))   -- (step index, cmd, expected, actual)
runSequence backend = go 0 Map.empty
  where
    go _ _ [] = pure Nothing
    go i model (cmd : rest) = do
      actual <- realStep backend cmd
      let (expected, model') = modelStep model cmd
      if actual == expected
        then go (i + 1) model' rest
        else pure (Just (i, cmd, expected, actual))

-- --------------------------------------------------------------------
-- Generators (used by both model and existing tests)
-- --------------------------------------------------------------------

genNs :: Gen Text
genNs = Text.pack <$> vectorOf 6 (elements ['a' .. 'z'])

genKey :: Gen Text
genKey = Text.pack <$> vectorOf 8 (elements (['a' .. 'z'] ++ ['0' .. '9']))

genPayload :: Gen ByteString
genPayload = BS8.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['0' .. '9']))

-- Two distinct namespaces.
genTwoNs :: Gen (Text, Text)
genTwoNs = do
  ns1 <- genNs
  ns2 <- genNs `suchThat` (/= ns1)
  pure (ns1, ns2)

-- Two distinct keys.
genTwoKeys :: Gen (Text, Text)
genTwoKeys = do
  k1 <- genKey
  k2 <- genKey `suchThat` (/= k1)
  pure (k1, k2)

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Storage" $ do
  describe "namespace helpers" $ do
    it "builds stable repo/user namespace" $ do
      repoUserNamespace "/tmp/my/repo" (UserId "u1")
        `shouldBe` "repo:-tmp-my-repo/user:u1"
      sessionNamespace (SessionId "s1") `shouldBe` "session:s1"

    it "normalizes windows-style separators" $ do
      repoUserNamespace "C:\\work\\repo" (UserId "u1")
        `shouldBe` "repo:C:-work-repo/user:u1"

  describe "model-based command sequences" $ do

    prop "sequential commands agree with Map oracle" $
      forAll (resize 25 (listOf1 arbitrary)) $ \cmds ->
        monadicIO $ do
          result <- run $ withBackend $ \backend ->
            runSequence backend cmds
          case result of
            Nothing ->
              assert True
            Just (stepIdx, cmd, expected, actual) ->
              fail $ "Disagreement at step " <> show stepIdx
                  <> "\n  command:  " <> show cmd
                  <> "\n  expected: " <> show expected
                  <> "\n  actual:   " <> show actual

    prop "list reflects all preceding puts" $
      forAll (resize 10 (listOf1 arbitrary)) $ \cmds ->
        monadicIO $ do
          run $ withBackend $ \backend -> do
            let model = foldl (\m c -> snd (modelStep m c)) Map.empty cmds
            mapM_ (realStep backend) cmds
            -- For each namespace in the model, the real list matches.
            mapM_ (\ns -> do
              let expected = sort [k | (n, k) <- Map.keys model, n == ns]
              actual <- sort <$> list backend ns
              actual `shouldBe` expected) namespaces

  describe "basic CRUD" $ do
    it "put/get/list/delete roundtrip" $
      withBackend $ \backend -> do
        let ns = "spec-ns"
            key = "k1"
            payload = BS8.pack "hello"
        put backend ns key payload `shouldReturn` Right ()
        get backend ns key `shouldReturn` Right payload
        listedKeys <- list backend ns
        listedKeys `shouldContain` [key]
        delete backend ns key `shouldReturn` Right ()
        getResult <- get backend ns key
        getResult `shouldSatisfy` isLeftStorageError

  describe "namespace isolation" $ do
    prop "data written to ns-A is invisible in ns-B" $
      forAll genTwoNs $ \(ns1, ns2) ->
        forAll genKey $ \key ->
          forAll genPayload $ \payload ->
            monadicIO $ do
              run $ withBackend $ \backend -> do
                _ <- put backend ns1 key payload
                result <- get backend ns2 key
                result `shouldSatisfy` isLeftStorageError

    prop "list for ns-A does not include keys from ns-B" $
      forAll genTwoNs $ \(ns1, ns2) ->
        forAll genKey $ \key ->
          forAll genPayload $ \payload ->
            monadicIO $ do
              run $ withBackend $ \backend -> do
                _ <- put backend ns1 key payload
                keys2 <- list backend ns2
                keys2 `shouldSatisfy` notElem key

    prop "delete in ns-A does not affect ns-B" $
      forAll genTwoNs $ \(ns1, ns2) ->
        forAll genKey $ \key ->
          forAll genPayload $ \payload ->
            monadicIO $ do
              run $ withBackend $ \backend -> do
                _ <- put backend ns1 key payload
                _ <- put backend ns2 key payload
                _ <- delete backend ns1 key
                result <- get backend ns2 key
                result `shouldSatisfy` either (const False) (== payload)

  describe "key isolation" $ do
    prop "writing key-A does not affect key-B in the same namespace" $
      forAll genTwoKeys $ \(k1, k2) ->
        forAll genNs $ \ns ->
          forAll genPayload $ \p1 ->
            forAll genPayload $ \p2 ->
              monadicIO $ do
                run $ withBackend $ \backend -> do
                  _ <- put backend ns k1 p1
                  _ <- put backend ns k2 p2
                  r1 <- get backend ns k1
                  r2 <- get backend ns k2
                  r1 `shouldBe` Right p1
                  r2 `shouldBe` Right p2

    prop "deleting key-A does not delete key-B" $
      forAll genTwoKeys $ \(k1, k2) ->
        forAll genNs $ \ns ->
          forAll genPayload $ \payload ->
            monadicIO $ do
              run $ withBackend $ \backend -> do
                _ <- put backend ns k1 payload
                _ <- put backend ns k2 payload
                _ <- delete backend ns k1
                r2 <- get backend ns k2
                r2 `shouldBe` Right payload

    prop "list returns all and only present keys" $
      forAll genNs $ \ns ->
        forAll (listOf1 genKey) $ \rawKeys ->
          let ks = take 5 (dedup rawKeys)
           in length ks > 0 ==>
                monadicIO $ do
                  run $ withBackend $ \backend -> do
                    mapM_ (\k -> put backend ns k (BS8.pack "v")) ks
                    listed <- list backend ns
                    sort listed `shouldBe` sort ks

  describe "concurrent access" $ do
    it "concurrent puts to distinct keys in the same namespace all succeed" $
      withBackend $ \backend -> do
        let ns = "conc-ns"
            pairs = [("key-" <> Text.pack (show i), BS8.pack ("val-" <> show (i :: Int))) | i <- [1 .. 50]]
        mapConcurrently_ (\(k, v) -> put backend ns k v) pairs
        results <- mapM (\(k, v) -> get backend ns k >>= \r -> pure (r == Right v)) pairs
        all id results `shouldBe` True

    it "concurrent puts to the same key do not corrupt the backend" $
      -- Last write wins, but no crash and result must be one of the
      -- written values — no partial/corrupt bytes.
      withBackend $ \backend -> do
        let ns = "same-key-ns"
            key = "shared"
            values = [BS8.pack ("v" <> show (i :: Int)) | i <- [1 .. 30]]
        mapConcurrently_ (\v -> put backend ns key v) values
        result <- get backend ns key
        case result of
          Left _ -> expectationFailure "key missing after concurrent puts"
          Right v -> v `shouldSatisfy` (`elem` values)

    it "concurrent puts to distinct namespaces do not interfere" $
      withBackend $ \backend -> do
        let pairs = [("ns-" <> Text.pack (show i), "key", BS8.pack ("v" <> show (i :: Int))) | i <- [1 .. 20 :: Int]]
        mapConcurrently_ (\(ns, k, v) -> put backend ns k v) pairs
        results <- mapM (\(ns, k, v) -> get backend ns k >>= \r -> pure (r == Right v)) pairs
        all id results `shouldBe` True

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x : xs) = x : dedup (filter (/= x) xs)

withBackend :: (StorageBackend -> IO a) -> IO a
withBackend action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-storage-spec"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  backend <- newBackend root
  out <- action backend
  ignoreIO (removePathForcibly root)
  pure out

isLeftStorageError :: Either StorageError a -> Bool
isLeftStorageError (Left _) = True
isLeftStorageError _ = False

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
