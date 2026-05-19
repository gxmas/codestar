module CodeStar.RepoMap.WorkerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

import CodeStar.RepoMap.Cache (RepoMapCache (..))
import CodeStar.RepoMap.Worker
  ( enqueueFile
  , enqueueAll
  , forceRebuild
  , getCurrentMap
  , getIndexedFiles
  , getWorkerStatus
  , newRepoMapWorker
  , RepoMapWorker
  , stopWorker
  , WorkerStatus (..)
  )
import CodeStar.TreeSitter (GrammarRegistry (..))

spec :: Spec
spec = describe "CodeStar.RepoMap.Worker" $ do
  it "starts with empty rendered map and no indexed files" $
    withWorkerEnv $ \worker _workspace -> do
      rendered <- getCurrentMap worker
      indexed <- getIndexedFiles worker
      rendered `shouldBe` Text.empty
      indexed `shouldBe` []

  it "forceRebuild updates worker status without errors" $
    withWorkerEnv $ \worker _workspace -> do
      forceRebuild worker
      status <- getWorkerStatus worker
      status.pendingFileCount `shouldBe` 0

  it "indexes enqueued files (non-code files become empty-tag entries)" $
    withWorkerEnv $ \worker workspace -> do
      let target = workspace </> "notes.txt"
      writeFile target "hello"
      enqueueFile worker target
      seen <- waitUntil 20 50000 $ do
        indexed <- getIndexedFiles worker
        pure (target `elem` indexed)
      seen `shouldBe` True
      status <- getWorkerStatus worker
      status.indexedFileCount `shouldSatisfy` (>= 1)

  it "collapses duplicate enqueue events into one indexed file entry" $
    withWorkerEnv $ \worker workspace -> do
      let target = workspace </> "dup.txt"
      writeFile target "hello"
      mapM_ (\_ -> enqueueFile worker target) [1 :: Int .. 5]
      seen <- waitUntil 40 50000 $ do
        indexed <- getIndexedFiles worker
        pure (countEq target indexed == 1)
      seen `shouldBe` True

  it "removes previously indexed code file after delete and re-enqueue" $
    withWorkerEnv $ \worker workspace -> do
      let target = workspace </> "gone.hs"
      writeFile target "module Gone where\nx = 1\n"
      enqueueFile worker target
      initiallyIndexed <- waitUntil 40 50000 $ do
        indexed <- getIndexedFiles worker
        pure (target `elem` indexed)
      initiallyIndexed `shouldBe` True

      removePathForcibly target
      enqueueFile worker target
      removed <- waitUntil 40 50000 $ do
        indexed <- getIndexedFiles worker
        pure (target `notElem` indexed)
      removed `shouldBe` True

  it "enqueueAll ignores hidden and excluded directories" $
    withWorkerEnv $ \worker workspace -> do
      let visible = workspace </> "src" </> "Main.txt"
          hidden = workspace </> ".hidden.txt"
          excluded = workspace </> "dist-newstyle" </> "out.txt"
      createDirectoryIfMissing True (workspace </> "src")
      createDirectoryIfMissing True (workspace </> "dist-newstyle")
      writeFile visible "ok"
      writeFile hidden "hidden"
      writeFile excluded "excluded"

      enqueueAll worker
      visibleIndexed <- waitUntil 40 50000 $ do
        indexed <- getIndexedFiles worker
        pure (visible `elem` indexed)
      visibleIndexed `shouldBe` True

      indexed <- getIndexedFiles worker
      indexed `shouldSatisfy` (hidden `notElem`)
      indexed `shouldSatisfy` (excluded `notElem`)

  it "stopWorker is safe to call more than once" $
    withWorkerEnvNoCleanup $ \worker workspace -> do
      stopWorker worker
      stopWorker worker
      createDirectoryIfMissing True workspace

withWorkerEnv :: (RepoMapWorker -> FilePath -> IO a) -> IO a
withWorkerEnv action = do
  tmp <- getTemporaryDirectory
  let workspace = tmp </> "codestar-worker-spec"
  ignoreIO (removePathForcibly workspace)
  createDirectoryIfMissing True workspace
  worker <- newRepoMapWorker emptyRegistry noCache workspace
  out <- action worker workspace
  stopWorker worker
  ignoreIO (removePathForcibly workspace)
  pure out
 where
  emptyRegistry = GrammarRegistry Map.empty
  noCache =
    RepoMapCache
      { getTags = \_ _ -> pure Nothing
      , putTags = \_ _ _ -> pure ()
      , getMap = \_ -> pure Nothing
      , putMap = \_ _ -> pure ()
      , invalidate = \_ -> pure ()
      }

withWorkerEnvNoCleanup :: (RepoMapWorker -> FilePath -> IO a) -> IO a
withWorkerEnvNoCleanup action = do
  tmp <- getTemporaryDirectory
  let workspace = tmp </> "codestar-worker-spec"
  ignoreIO (removePathForcibly workspace)
  createDirectoryIfMissing True workspace
  worker <- newRepoMapWorker emptyRegistry noCache workspace
  out <- action worker workspace
  ignoreIO (removePathForcibly workspace)
  pure out
 where
  emptyRegistry = GrammarRegistry Map.empty
  noCache =
    RepoMapCache
      { getTags = \_ _ -> pure Nothing
      , putTags = \_ _ _ -> pure ()
      , getMap = \_ -> pure Nothing
      , putMap = \_ _ -> pure ()
      , invalidate = \_ -> pure ()
      }

countEq :: Eq a => a -> [a] -> Int
countEq x = length . filter (== x)

waitUntil :: Int -> Int -> IO Bool -> IO Bool
waitUntil attempts delayUs check
  | attempts <= 0 = pure False
  | otherwise = do
      ok <- check
      if ok
        then pure True
        else threadDelay delayUs >> waitUntil (attempts - 1) delayUs check

ignoreIO :: IO () -> IO ()
ignoreIO io = io `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()
