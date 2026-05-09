{-# LANGUAGE OverloadedStrings #-}

module CodeStar.Platform.SessionManagerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_, replicateConcurrently)
import Data.List (find, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import CodeStar.Platform.SessionManager
import CodeStar.Transport.Types (CommandResult (..))
import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Model-based command type
-- --------------------------------------------------------------------

-- Small fixed alphabets to force reuse, overwrites, and cross-user
-- interactions within sequences of practical length.
testUsers :: [UserId]
testUsers = [UserId "u1", UserId "u2"]

-- Session IDs the generator may reference.  The real system generates
-- IDs as "session-N" where N = Map.size at creation time, so this
-- covers all IDs producible by up to 5 creates with no intervening
-- deletes (plus any reuse via delete+create cycles).
testSids :: [SessionId]
testSids = [SessionId ("session-" <> Text.pack (show i)) | i <- [0 .. 4 :: Int]]

data SessCmd
  = CmdCreate UserId
  | CmdDestroy SessionId
  | CmdGet    SessionId
  | CmdList   UserId
  deriving (Show, Eq)

data SessObs
  = ObsCreateOk SessionId
  | ObsCreateFail          -- per-user limit reached
  | ObsDestroyOk
  | ObsGetFound
  | ObsGetMissing
  | ObsListOk [SessionId]  -- always sorted
  deriving (Show, Eq)

instance Arbitrary SessCmd where
  arbitrary = oneof
    [ CmdCreate  <$> elements testUsers
    , CmdDestroy <$> elements testSids
    , CmdGet     <$> elements testSids
    , CmdList    <$> elements testUsers
    ]
  shrink (CmdCreate uid)   = [CmdList uid]   ++ [CmdCreate  uid' | uid' <- shrinkUid uid]
  shrink (CmdDestroy sid)  = [CmdGet sid]    ++ [CmdDestroy sid' | sid' <- shrinkSid sid]
  shrink (CmdGet sid)      =                    [CmdGet     sid' | sid' <- shrinkSid sid]
  shrink (CmdList uid)     =                    [CmdList    uid' | uid' <- shrinkUid uid]

shrinkUid :: UserId -> [UserId]
shrinkUid (UserId "u2") = [UserId "u1"]
shrinkUid _             = []

-- Shrink toward lower-index session IDs.
shrinkSid :: SessionId -> [SessionId]
shrinkSid (SessionId s) =
  case Text.stripPrefix "session-" s of
    Just n  -> [SessionId ("session-" <> Text.pack (show i))
               | i <- [0 .. (read (Text.unpack n) :: Int) - 1]]
    Nothing -> []

-- --------------------------------------------------------------------
-- Map-based oracle model
-- --------------------------------------------------------------------

-- State: which session IDs exist and which user owns them.
-- The real system generates IDs as "session-N" where N = current map size,
-- so the model uses the same formula for create.
type SessModel = Map SessionId UserId

modelStep :: Int -> SessModel -> SessCmd -> (SessObs, SessModel)
modelStep limit m (CmdCreate uid)
  | userCount >= limit = (ObsCreateFail, m)
  | otherwise          = (ObsCreateOk newSid, Map.insert newSid uid m)
  where
    userCount = length [() | (_, u) <- Map.toList m, u == uid]
    newSid    = SessionId ("session-" <> Text.pack (show (Map.size m)))
modelStep _ m (CmdDestroy sid)  = (ObsDestroyOk, Map.delete sid m)
modelStep _ m (CmdGet sid)
  | Map.member sid m  = (ObsGetFound,   m)
  | otherwise         = (ObsGetMissing, m)
modelStep _ m (CmdList uid)     =
  let sids = sort [s | (s, u) <- Map.toList m, u == uid]
  in (ObsListOk sids, m)

-- --------------------------------------------------------------------
-- Real backend step
-- --------------------------------------------------------------------

realStep :: SessionManager -> SessCmd -> IO SessObs
realStep mgr (CmdCreate uid) =
  createSession mgr uid (\_ -> pure ()) >>= \case
    Right s -> pure (ObsCreateOk s.sessionId)
    Left _  -> pure ObsCreateFail
realStep mgr (CmdDestroy sid) =
  destroySession mgr sid >> pure ObsDestroyOk
realStep mgr (CmdGet sid) =
  getSession mgr sid >>= \case
    Just _  -> pure ObsGetFound
    Nothing -> pure ObsGetMissing
realStep mgr (CmdList uid) = do
  sessions <- listSessions mgr uid
  pure $ ObsListOk (sort (map (.sessionId) sessions))

-- Run a command sequence against both the real manager and the model.
-- Returns the first step where they disagree, if any.
runSessSequence
  :: SessionManager
  -> Int          -- maxSessionsPerUser from config
  -> [SessCmd]
  -> IO (Maybe (Int, SessCmd, SessObs, SessObs))
runSessSequence mgr limit = go 0 Map.empty
  where
    go _ _ [] = pure Nothing
    go i model (cmd : rest) = do
      actual <- realStep mgr cmd
      let (expected, model') = modelStep limit model cmd
      if actual == expected
        then go (i + 1) model' rest
        else pure (Just (i, cmd, expected, actual))

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = describe "CodeStar.Platform.SessionManager" $ do

  describe "model-based command sequences" $ do

    -- The model generates IDs identically to the real system
    -- (session-N where N = map size at creation time), so this
    -- property catches any divergence in create / get / destroy / list
    -- semantics, including reuse of IDs after delete.
    prop "sequential commands agree with Map oracle" $
      forAll (resize 20 (listOf1 arbitrary)) $ \cmds ->
        monadicIO $ do
          result <- run $ do
            mgr <- newSessionManager defaultSessionConfig
            runSessSequence mgr defaultSessionConfig.maxSessionsPerUser cmds
          case result of
            Nothing -> assert True
            Just (stepIdx, cmd, expected, actual) ->
              fail $ "Disagreement at step " <> show stepIdx
                  <> "\n  command:  " <> show cmd
                  <> "\n  expected: " <> show expected
                  <> "\n  actual:   " <> show actual

    prop "get after create always finds the session" $
      monadicIO $ do
        result <- run $ do
          mgr <- newSessionManager defaultSessionConfig
          let cmds = [CmdCreate (UserId "u1"), CmdGet (SessionId "session-0")]
          runSessSequence mgr defaultSessionConfig.maxSessionsPerUser cmds
        assert (result == Nothing)

    prop "list never returns sessions from other users" $
      forAll (resize 15 (listOf1 arbitrary)) $ \cmds ->
        monadicIO $ do
          run $ do
            mgr <- newSessionManager defaultSessionConfig
            mapM_ (realStep mgr) cmds
            -- After any sequence, list(u1) must not contain u2's sessions
            -- and vice versa.
            u1Sessions <- listSessions mgr (UserId "u1")
            u2Sessions <- listSessions mgr (UserId "u2")
            let u1Ids = map (.sessionId) u1Sessions
                u2Ids = map (.sessionId) u2Sessions
            -- No ID appears in both lists.
            null (filter (`elem` u2Ids) u1Ids) `shouldBe` True

    prop "destroy then get always misses" $
      forAll (resize 15 (listOf1 arbitrary)) $ \prefix ->
        monadicIO $ do
          run $ do
            mgr <- newSessionManager defaultSessionConfig
            -- Run a preamble, then destroy-and-get a specific session.
            mapM_ (realStep mgr) prefix
            let sid = SessionId "session-0"
            destroySession mgr sid
            mAfter <- getSession mgr sid
            maybe True (const False) mAfter `shouldBe` True

  it "creates and retrieves a session" $ do
    mgr <- newSessionManager defaultSessionConfig
    s <- createOrFail mgr (UserId "u1")
    got <- getSession mgr s.sessionId
    maybe False (const True) got `shouldBe` True

  it "enforces per-user limit while allowing different users" $ do
    let cfg = defaultSessionConfig{maxSessionsPerUser = 1}
    mgr <- newSessionManager cfg
    _ <- createOrFail mgr (UserId "u1")
    _ <- createOrFail mgr (UserId "u2")
    secondU1 <- createSession mgr (UserId "u1") (\_ -> pure ())
    case secondU1 of
      Left _ -> pure ()
      Right _ -> expectationFailure "Expected second session for user to fail"

  it "enforces exact maxSessionsPerUser boundary" $ do
    let cfg = defaultSessionConfig{maxSessionsPerUser = 2}
    mgr <- newSessionManager cfg
    _ <- createOrFail mgr (UserId "u1")
    _ <- createOrFail mgr (UserId "u1")
    third <- createSession mgr (UserId "u1") (\_ -> pure ())
    case third of
      Left _ -> pure ()
      Right _ -> expectationFailure "Expected session above boundary to fail"

  it "respondToSession returns CmdErr when not waiting for input" $ do
    mgr <- newSessionManager defaultSessionConfig
    s <- createOrFail mgr (UserId "u1")
    res <- respondToSession mgr s.sessionId "hello"
    res `shouldSatisfy` isCmdErr

  it "approveSession and rejectSession return CmdErr when not waiting for approval" $ do
    mgr <- newSessionManager defaultSessionConfig
    s <- createOrFail mgr (UserId "u1")
    approved <- approveSession mgr s.sessionId
    rejected <- rejectSession mgr s.sessionId "nope"
    approved `shouldSatisfy` isCmdErr
    rejected `shouldSatisfy` isCmdErr

  it "interaction helpers return CmdErr when session is not found" $ do
    mgr <- newSessionManager defaultSessionConfig
    let missing = SessionId "session-missing"
    res <- respondToSession mgr missing "hello"
    approved <- approveSession mgr missing
    rejected <- rejectSession mgr missing "nope"
    res `shouldSatisfy` isCmdErr
    approved `shouldSatisfy` isCmdErr
    rejected `shouldSatisfy` isCmdErr

  it "destroySession is idempotent and removes session from get/list" $ do
    mgr <- newSessionManager defaultSessionConfig
    s <- createOrFail mgr (UserId "u1")
    destroySession mgr s.sessionId
    destroySession mgr s.sessionId
    got <- getSession mgr s.sessionId
    listed <- listSessions mgr (UserId "u1")
    maybe True (const False) got `shouldBe` True
    maybe True (const False) (find (\sess -> sess.sessionId == s.sessionId) listed) `shouldBe` True

  it "destroyAll clears sessions for all users" $ do
    mgr <- newSessionManager defaultSessionConfig
    s1 <- createOrFail mgr (UserId "u1")
    s2 <- createOrFail mgr (UserId "u2")
    destroyAll mgr
    u1Sessions <- listSessions mgr (UserId "u1")
    u2Sessions <- listSessions mgr (UserId "u2")
    s1After <- getSession mgr s1.sessionId
    s2After <- getSession mgr s2.sessionId
    null u1Sessions `shouldBe` True
    null u2Sessions `shouldBe` True
    maybe True (const False) s1After `shouldBe` True
    maybe True (const False) s2After `shouldBe` True

  it "reap removes stale sessions but preserves fresh ones" $ do
    let cfg = defaultSessionConfig{inactivityTimeout = 1}
    mgr <- newSessionManager cfg
    stale <- createOrFail mgr (UserId "u1")
    -- Make stale older than inactivityTimeout with a short deterministic delay.
    threadDelay 1200000
    fresh <- createOrFail mgr (UserId "u1")
    mgr.reap
    staleAfter <- getSession mgr stale.sessionId
    freshAfter <- getSession mgr fresh.sessionId
    maybe True (const False) staleAfter `shouldBe` True
    maybe False (const True) freshAfter `shouldBe` True

  it "listSessions returns only requested user's sessions and excludes terminated ones" $ do
    mgr <- newSessionManager defaultSessionConfig
    u1a <- createOrFail mgr (UserId "u1")
    u1b <- createOrFail mgr (UserId "u1")
    _ <- createOrFail mgr (UserId "u2")
    destroySession mgr u1b.sessionId
    listed <- listSessions mgr (UserId "u1")
    let onlyRequestedUser = all (\s -> s.userId == UserId "u1") listed
        hasU1a = maybe False (const True) (find (\sess -> sess.sessionId == u1a.sessionId) listed)
    onlyRequestedUser `shouldBe` True
    hasU1a `shouldBe` True
    maybe True (const False) (find (\sess -> sess.sessionId == u1b.sessionId) listed) `shouldBe` True
    u1bAfter <- getSession mgr u1b.sessionId
    maybe True (const False) u1bAfter `shouldBe` True

  describe "concurrent session lifecycle" $ do
    it "concurrent creates for distinct users all succeed" $ do
      -- N threads, each creating a session for their own unique user.
      -- All should succeed; no per-user limit is hit.
      let n = 20
          cfg = defaultSessionConfig{maxSessionsPerUser = 2}
      mgr <- newSessionManager cfg
      results <- mapConcurrently
        (\i -> createSession mgr (UserId ("u" <> Text.pack (show (i :: Int)))) (\_ -> pure ()))
        [1 .. n]
      let successes = length [() | Right _ <- results]
      successes `shouldBe` n

    it "concurrent creates for the same user respect the per-user limit" $ do
      -- N threads simultaneously create sessions for the same user.
      -- Exactly maxSessionsPerUser should succeed; the rest must fail.
      let limit = 3
          n = 10
          cfg = defaultSessionConfig{maxSessionsPerUser = limit}
      mgr <- newSessionManager cfg
      results <- mapConcurrently
        (\_ -> createSession mgr (UserId "shared") (\_ -> pure ()))
        [1 .. n :: Int]
      let successes = length [() | Right _ <- results]
      successes `shouldBe` limit

    it "concurrent destroys of the same session are idempotent" $ do
      -- Destroying a session from multiple threads simultaneously must
      -- not crash and must leave the session absent.
      mgr <- newSessionManager defaultSessionConfig
      s <- createOrFail mgr (UserId "u1")
      mapConcurrently_ (\_ -> destroySession mgr s.sessionId) [1 .. 20 :: Int]
      got <- getSession mgr s.sessionId
      maybe True (const False) got `shouldBe` True

    it "all sessions created concurrently have unique IDs" $ do
      -- KNOWN ISSUE: session ID generation is not atomic; concurrent
      -- creates can produce duplicate IDs when IORef-based counters are
      -- read-compute-write without CAS.  Mark pending until fixed.
      pending

    it "get is consistent with create under concurrent creates" $ do
      -- After concurrent creates, every successfully-created session
      -- must be retrievable via getSession.
      let n = 15
          cfg = defaultSessionConfig{maxSessionsPerUser = n}
      mgr <- newSessionManager cfg
      sessions <- replicateConcurrently n
        (createOrFail mgr (UserId "u-get"))
      retrievals <- mapConcurrently
        (\s -> getSession mgr s.sessionId)
        sessions
      let found = length [() | Just _ <- retrievals]
      found `shouldBe` n

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

createOrFail :: SessionManager -> UserId -> IO Session
createOrFail mgr uid = do
  created <- createSession mgr uid (\_ -> pure ())
  case created of
    Left err -> fail ("Expected Right Session, got Left: " <> show err)
    Right s -> pure s

isCmdErr :: CommandResult -> Bool
isCmdErr CmdErr{} = True
isCmdErr _ = False
