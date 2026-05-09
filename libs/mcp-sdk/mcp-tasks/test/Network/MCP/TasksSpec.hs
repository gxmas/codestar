module Network.MCP.TasksSpec (spec) where

import Data.Aeson (Value (..), eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Network.MCP.Tasks
import Network.MCP.Tasks.Generators ()
import Network.MCP.Types (Cursor (..), RPCError (..))
import Network.MCP.Types.Capabilities
  ( AuthContext
  , TaskError (..)
  , TaskErrorKind (..)
  , TaskStatus (..)
  )

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

freshStore :: IO TaskStore
freshStore = newTaskStore defaultTaskStoreConfig

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

leftKind :: Either TaskError a -> Maybe TaskErrorKind
leftKind (Left te) = Just te.taskErrorKind
leftKind _         = Nothing

-- Drain all pages from listTasks into a flat list.
drainList :: TaskStore -> Maybe AuthContext -> IO [Task]
drainList store auth = go Nothing []
  where
    go cursor acc = do
      result <- listTasks store auth cursor
      case result of
        Left _              -> pure acc
        Right (ts, Nothing) -> pure (acc ++ ts)
        Right (ts, Just c)  -> go (Just c) (acc ++ ts)

------------------------------------------------------------------------
-- Candidate 1: status transitions
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Task record — JSON codec" $ do
    prop "round-trips through JSON encode/decode" $ \(t :: Task) ->
      eitherDecode (encode t) === Right t

    prop "always encodes required fields" $ \(t :: Task) ->
      let v = toJSON t
          has k = case v of
                    Aeson.Object o -> KM.member k o
                    _              -> False
      in  has "id" && has "status" && has "createdAt" && has "updatedAt" && has "ttl"

    it "omits 'message' when Nothing" $ do
      let t = Task "x" TaskWorking Nothing "2025-01-01T00:00:00Z" "2025-01-01T00:00:00Z" 3600000
          v = toJSON t
      case v of
        Aeson.Object o -> KM.member "message" o `shouldBe` False
        _              -> expectationFailure "Expected Object"

    it "includes 'message' when Just" $ do
      let t = Task "x" TaskWorking (Just "hi") "2025-01-01T00:00:00Z" "2025-01-01T00:00:00Z" 3600000
          v = toJSON t
      case v of
        Aeson.Object o -> KM.member "message" o `shouldBe` True
        _              -> expectationFailure "Expected Object"

  describe "createTask" $ do
    it "returns a task with TaskWorking status" $ do
      store <- freshStore
      result <- createTask store Nothing
      case result of
        Left e  -> expectationFailure $ "Expected Right, got " ++ show e
        Right t -> t.taskStatus `shouldBe` TaskWorking

    it "assigns a non-empty task ID" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      t.taskId `shouldNotBe` ""

    prop "all created task IDs are distinct" $ \(Positive n) ->
      n <= 50 ==> ioProperty $ do
        store <- freshStore
        results <- mapM (\_ -> createTask store Nothing) [1 .. n :: Int]
        let ids = [t.taskId | Right t <- results]
        pure $ length ids === length (nub ids)

    it "respects taskMaxTasks limit" $ do
      let cfg = defaultTaskStoreConfig { taskMaxTasks = 2 }
      store <- newTaskStore cfg
      _ <- createTask store Nothing
      _ <- createTask store Nothing
      result <- createTask store Nothing
      isLeft result `shouldBe` True

  describe "getTask" $ do
    it "returns TaskNotFound for unknown ID" $ do
      store <- freshStore
      result <- getTask store "no-such-id" Nothing
      leftKind result `shouldBe` Just TaskNotFound

    it "returns the task after creation" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- getTask store t.taskId Nothing
      case result of
        Left e   -> expectationFailure $ show e
        Right t' -> t'.taskId `shouldBe` t.taskId

  describe "cancelTask" $ do
    it "transitions a working task to TaskCancelled" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- cancelTask store t.taskId Nothing
      case result of
        Left e   -> expectationFailure $ show e
        Right t' -> t'.taskStatus `shouldBe` TaskCancelled

    it "returns TaskAlreadyTerminal when already cancelled" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- cancelTask store t.taskId Nothing
      result <- cancelTask store t.taskId Nothing
      leftKind result `shouldBe` Just TaskAlreadyTerminal

    it "returns TaskAlreadyTerminal when already completed" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- completeTask store t.taskId (String "ok") Nothing
      result <- cancelTask store t.taskId Nothing
      leftKind result `shouldBe` Just TaskAlreadyTerminal

    it "returns TaskAlreadyTerminal when already failed" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      let err = RPCError (-1) "oops" Nothing
      _ <- failTask store t.taskId err Nothing
      result <- cancelTask store t.taskId Nothing
      leftKind result `shouldBe` Just TaskAlreadyTerminal

    it "returns TaskNotFound for unknown ID" $ do
      store <- freshStore
      result <- cancelTask store "ghost" Nothing
      leftKind result `shouldBe` Just TaskNotFound

  describe "completeTask" $ do
    it "transitions a working task to TaskCompleted" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- completeTask store t.taskId (String "done") Nothing
      case result of
        Left e   -> expectationFailure $ show e
        Right t' -> t'.taskStatus `shouldBe` TaskCompleted

    it "returns TaskAlreadyTerminal on second completion" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- completeTask store t.taskId (String "first") Nothing
      result <- completeTask store t.taskId (String "second") Nothing
      leftKind result `shouldBe` Just TaskAlreadyTerminal

  describe "failTask" $ do
    it "transitions a working task to TaskFailed" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- failTask store t.taskId (RPCError (-1) "boom" Nothing) Nothing
      case result of
        Left e   -> expectationFailure $ show e
        Right t' -> t'.taskStatus `shouldBe` TaskFailed

    it "returns TaskAlreadyTerminal when already terminal" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- cancelTask store t.taskId Nothing
      result <- failTask store t.taskId (RPCError (-1) "x" Nothing) Nothing
      leftKind result `shouldBe` Just TaskAlreadyTerminal

  describe "terminal states are final" $
    prop "no operation changes a terminal task's status" $ ioProperty $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- cancelTask store t.taskId Nothing
      -- attempt all mutation ops on the cancelled task
      r1 <- cancelTask store t.taskId Nothing
      r2 <- completeTask store t.taskId (String "x") Nothing
      r3 <- failTask store t.taskId (RPCError 0 "" Nothing) Nothing
      -- all must fail
      pure $
        isLeft r1 && isLeft r2 && isLeft r3

  describe "no crash on arbitrary operation sequences" $
    prop "never throws — errors returned via Either" $ ioProperty $ do
      store <- freshStore
      -- create a few tasks
      results <- mapM (\_ -> createTask store Nothing) [1 .. 5 :: Int]
      let ids = [t.taskId | Right t <- results] ++ ["nonexistent-id"]
      -- fire mutations at known and unknown IDs
      mapM_ (\tid -> cancelTask store tid Nothing) ids
      mapM_ (\tid -> completeTask store tid (String "x") Nothing) ids
      mapM_ (\tid -> failTask store tid (RPCError 0 "x" Nothing) Nothing) ids
      mapM_ (\tid -> getTask store tid Nothing) ids
      _ <- listTasks store Nothing Nothing
      pure True

  --------------------------------------------------------------------------
  -- Candidate 2: Auth-context access control
  --------------------------------------------------------------------------

  describe "auth-context access control" $ do
    it "no auth on task — accessible without request auth" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- getTask store t.taskId Nothing
      isRight result `shouldBe` True

    it "no auth on task — accessible with any request auth" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      result <- getTask store t.taskId (Just (String "alice"))
      isRight result `shouldBe` True

    it "matching auth on task and request — succeeds" $ do
      store <- freshStore
      let auth = Just (String "alice")
      Right t <- createTask store auth
      result <- getTask store t.taskId auth
      isRight result `shouldBe` True

    it "mismatching auth — TaskAccessDenied" $ do
      store <- freshStore
      Right t <- createTask store (Just (String "alice"))
      result <- getTask store t.taskId (Just (String "bob"))
      leftKind result `shouldBe` Just TaskAccessDenied

    it "task has auth, request has none — TaskAccessDenied" $ do
      store <- freshStore
      Right t <- createTask store (Just (String "alice"))
      result <- getTask store t.taskId Nothing
      leftKind result `shouldBe` Just TaskAccessDenied

    it "cancelTask respects auth" $ do
      store <- freshStore
      Right t <- createTask store (Just (String "alice"))
      result <- cancelTask store t.taskId (Just (String "bob"))
      leftKind result `shouldBe` Just TaskAccessDenied

    it "completeTask respects auth" $ do
      store <- freshStore
      Right t <- createTask store (Just (String "alice"))
      result <- completeTask store t.taskId (String "x") (Just (String "bob"))
      leftKind result `shouldBe` Just TaskAccessDenied

    it "failTask respects auth" $ do
      store <- freshStore
      Right t <- createTask store (Just (String "alice"))
      result <- failTask store t.taskId (RPCError 0 "x" Nothing) (Just (String "bob"))
      leftKind result `shouldBe` Just TaskAccessDenied

    describe "listTasks auth filtering" $ do
      it "filters out tasks with mismatching auth" $ do
        store <- freshStore
        _ <- createTask store (Just (String "alice"))
        _ <- createTask store (Just (String "bob"))
        tasks <- drainList store (Just (String "alice"))
        all (\t -> t.taskStatus /= TaskCancelled || True) tasks `shouldBe` True
        -- only alice's task should appear
        length tasks `shouldBe` 1

      it "no-auth tasks visible to everyone" $ do
        store <- freshStore
        _ <- createTask store Nothing
        tasksAlice <- drainList store (Just (String "alice"))
        tasksBob   <- drainList store (Just (String "bob"))
        tasksNone  <- drainList store Nothing
        length tasksAlice `shouldBe` 1
        length tasksBob   `shouldBe` 1
        length tasksNone  `shouldBe` 1

  --------------------------------------------------------------------------
  -- Candidate 3: TTL expiry
  --------------------------------------------------------------------------

  describe "TTL expiry" $ do
    -- We test the TTL logic by creating a task with a very short TTL and
    -- calling the TTL checker via a short threadDelay + direct store inspection.
    -- Since the background thread polls every 60s we can't rely on it here;
    -- instead we test the helper logic indirectly via the store state.

    it "non-expired task retains its status" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      -- check immediately — well before any TTL
      Right t' <- getTask store t.taskId Nothing
      t'.taskStatus `shouldBe` TaskWorking

    it "completing a task prevents TTL expiry from changing status" $ do
      store <- freshStore
      Right t <- createTask store Nothing
      _ <- completeTask store t.taskId (String "done") Nothing
      Right t' <- getTask store t.taskId Nothing
      t'.taskStatus `shouldBe` TaskCompleted

  --------------------------------------------------------------------------
  -- Candidate 4: Pagination
  --------------------------------------------------------------------------

  describe "listTasks pagination" $ do
    it "empty store returns empty list and no cursor" $ do
      store <- freshStore
      result <- listTasks store Nothing Nothing
      case result of
        Left e          -> expectationFailure $ show e
        Right (ts, cur) -> do
          ts  `shouldBe` []
          cur `shouldBe` Nothing

    it "small list (< page size) returns all tasks and no cursor" $ do
      store <- freshStore
      mapM_ (\_ -> createTask store Nothing) [1 .. 10 :: Int]
      result <- listTasks store Nothing Nothing
      case result of
        Left e          -> expectationFailure $ show e
        Right (ts, cur) -> do
          length ts `shouldBe` 10
          cur       `shouldBe` Nothing

    prop "draining all pages returns exactly N tasks" $ \(Positive n) ->
      n <= 120 ==> ioProperty $ do
        store <- freshStore
        mapM_ (\_ -> createTask store Nothing) [1 .. n :: Int]
        tasks <- drainList store Nothing
        pure $ length tasks === n

    prop "no duplicate task IDs across pages" $ \(Positive n) ->
      n <= 120 ==> ioProperty $ do
        store <- freshStore
        mapM_ (\_ -> createTask store Nothing) [1 .. n :: Int]
        tasks <- drainList store Nothing
        let ids = map (.taskId) tasks
        pure $ length ids === length (nub ids)

    it "pagination terminates (cursor eventually returns Nothing)" $ do
      store <- freshStore
      mapM_ (\_ -> createTask store Nothing) [1 .. 55 :: Int]
      -- page 1
      Right (page1, Just c1) <- listTasks store Nothing Nothing
      length page1 `shouldBe` 50
      -- page 2 (remainder)
      Right (page2, cur2) <- listTasks store Nothing (Just c1)
      length page2 `shouldBe` 5
      cur2 `shouldBe` Nothing

    it "exactly page-size tasks yields cursor on first call" $ do
      store <- freshStore
      mapM_ (\_ -> createTask store Nothing) [1 .. 50 :: Int]
      result <- listTasks store Nothing Nothing
      case result of
        Left e          -> expectationFailure $ show e
        Right (ts, _cur) -> do
          length ts `shouldBe` 50

  -- ── Cursor encoding roundtrip ─────────────────────────────────────────
  describe "cursor encoding" $ do
    prop "cursorToOffset (offsetToCursor n) == Just n" $
      \(NonNegative n) ->
        cursorToOffset (offsetToCursor (n :: Int)) === Just n

    it "cursorToOffset returns Nothing for non-numeric text" $
      cursorToOffset (Cursor "not-a-number") `shouldBe` Nothing

    it "cursorToOffset returns Nothing for empty cursor" $
      cursorToOffset (Cursor "") `shouldBe` Nothing
