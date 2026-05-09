module CodeStar.MemorySpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, catch)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.Memory

spec :: Spec
spec = describe "CodeStar.Memory" $ do
  it "write then read returns same content" $
    withMemoryStore $ \store -> do
      let entry = mkEntry "m1" Convention "prefer point-free style"
      saveMemory store entry
      entries <- loadMemory store
      entries `shouldContain` [entry]

  it "delete then read does not include deleted entry" $
    withMemoryStore $ \store -> do
      let entry = mkEntry "m2" KnownPitfall "avoid O(n^2) scan"
      saveMemory store entry
      deleteMemory store "m2"
      entries <- loadMemory store
      entries `shouldNotContain` [entry]

  it "list after write includes new entry" $
    withMemoryStore $ \store -> do
      let entry = mkEntry "m3" UserPreference "user prefers concise output"
      saveMemory store entry
      map meId <$> loadMemory store `shouldReturn` ["m3"]

  it "concurrent writes do not corrupt store" $
    withMemoryStore $ \store -> do
      let entries =
            [ mkEntry (Text.pack ("c" <> show i)) SuccessfulApproach (Text.pack ("content-" <> show i))
            | i <- [1 :: Int .. 20]
            ]
      done <- mapM (\_ -> newEmptyMVar) entries
      mapM_
        ( \(entry, gate) -> do
            _ <- forkIO $ saveMemory store entry >> putMVar gate ()
            pure ()
        )
        (zip entries done)
      mapM_ takeMVar done
      loaded <- loadMemory store
      map meId loaded `shouldSatisfy` \ids -> all (`elem` ids) (map meId entries)

mkEntry :: Text -> MemoryCategory -> Text -> MemoryEntry
mkEntry eid cat content =
  MemoryEntry
    { meId = eid
    , meCategory = cat
    , meContent = content
    , meSourceHash = "hash-" <> eid
    }

withMemoryStore :: (MemoryStore -> IO a) -> IO a
withMemoryStore action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "codestar-memory-spec"
  ignoreIO (removePathForcibly root)
  createDirectoryIfMissing True root
  store <- newMemoryStore root
  out <- action store
  ignoreIO (removePathForcibly root)
  pure out

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
