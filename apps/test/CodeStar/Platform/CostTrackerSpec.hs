{-# OPTIONS_GHC -Wno-orphans #-}

module CodeStar.Platform.CostTrackerSpec (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)

import CodeStar.Platform.CostTracker
  ( CostTracker (..)
  , RecordResult (..)
  , estimateCost
  , getCost
  , newCostTracker
  , record
  , resetSession
  )
import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Generators
-- --------------------------------------------------------------------

genSessionId :: Gen SessionId
genSessionId = SessionId . Text.pack . show <$> chooseInt (1, 10)

genUserId :: Gen UserId
genUserId = UserId . Text.pack . show <$> chooseInt (1, 5)

genModel :: Gen Text
genModel = elements ["claude-haiku-3", "claude-sonnet-4", "claude-opus-4", "gpt-4o", "unknown"]

genTokens :: Gen Word64
genTokens = choose (0, 5_000)

-- --------------------------------------------------------------------
-- Spec
-- --------------------------------------------------------------------

spec :: Spec
spec = do
  describe "estimateCost" $ do
    it "is non-negative" $
      property $
        forAll genModel $ \model ->
          forAll genTokens $ \inp ->
            forAll genTokens $ \out ->
              estimateCost model inp out >= 0.0

    it "is monotone in input tokens" $
      property $
        forAll genModel $ \model ->
          forAll genTokens $ \inp1 ->
            forAll (choose (inp1, inp1 + 5000)) $ \inp2 ->
              forAll genTokens $ \out ->
                estimateCost model inp1 out <= estimateCost model inp2 out

    it "is monotone in output tokens" $
      property $
        forAll genModel $ \model ->
          forAll genTokens $ \inp ->
            forAll genTokens $ \out1 ->
              forAll (choose (out1, out1 + 5000)) $ \out2 ->
                estimateCost model inp out1 <= estimateCost model inp out2

    it "zero tokens gives zero cost" $
      property $
        forAll genModel $ \model ->
          estimateCost model 0 0 === 0.0

    it "is linear: cost(a+b) == cost(a) + cost(b) for same model" $
      property $
        forAll genModel $ \model ->
          forAll genTokens $ \a ->
            forAll genTokens $ \b ->
              forAll genTokens $ \out ->
                let combined = estimateCost model (a + b) out
                    separate = estimateCost model a out + estimateCost model b 0
                 in counterexample ("combined=" <> show combined <> " separate=" <> show separate) $
                      abs (combined - separate) < 1e-10

  describe "CostTracker state machine" $ do
    it "getCost after reset returns zero" $
      monadicIO $ do
        ct <- run (newCostTracker Nothing Nothing)
        let sid = SessionId "s1"; uid = UserId "u1"
        _ <- run (record ct sid uid "claude-sonnet" 100 50)
        run (resetSession ct sid)
        (i, o, c) <- run (getCost ct sid)
        assert (i == 0 && o == 0 && c == 0.0)

    it "cumulative accumulation of tokens" $
      property $
        forAll (resize 8 (listOf1 genTokens)) $ \inputToks ->
          monadicIO $ do
            ct <- run (newCostTracker Nothing Nothing)
            let sid = SessionId "acc"; uid = UserId "u1"
            mapM_ (\tok -> run (record ct sid uid "unknown" tok 0)) inputToks
            (totalIn, _, _) <- run (getCost ct sid)
            assert (totalIn == sum inputToks)

    it "session limit triggers BudgetExhausted" $
      property $
        forAll (chooseInt (10, 100)) $ \limit ->
          monadicIO $ do
            ct <- run (newCostTracker (Just (fromIntegral limit)) Nothing)
            let sid = SessionId "lim"; uid = UserId "u1"
            r <- run (record ct sid uid "unknown" (fromIntegral limit + 1) 0)
            assert (isBudgetExhausted r)

    it "sessions are isolated" $
      monadicIO $ do
        ct <- run (newCostTracker Nothing Nothing)
        let sid1 = SessionId "iso1"; sid2 = SessionId "iso2"; uid = UserId "u1"
        _ <- run (record ct sid1 uid "unknown" 500 200)
        (i2, o2, _) <- run (getCost ct sid2)
        assert (i2 == 0 && o2 == 0)

    it "user-level daily cap is enforced across sessions" $
      monadicIO $ do
        ct <- run (newCostTracker Nothing (Just 100))
        let uid = UserId "u-cap"
            sid1 = SessionId "cap-1"
            sid2 = SessionId "cap-2"
        r1 <- run (record ct sid1 uid "unknown" 40 10)
        r2 <- run (record ct sid2 uid "unknown" 60 0)
        assert (r1 == Recorded && isBudgetExhausted r2)

    it "session reset does not reset user-level accumulator" $
      monadicIO $ do
        ct <- run (newCostTracker Nothing Nothing)
        let uid = UserId "u-acc"
            sid = SessionId "s-acc"
        _ <- run (record ct sid uid "unknown" 25 5)
        run (resetSession ct sid)
        daily <- run (ct.userDailyTokens uid)
        assert (daily == 30)

    it "model-based: random command sequences match pure model" $
      property $
        forAllShrink genCommandSequence shrink $ \cmds ->
          let len = length cmds
              touchedSessions = countTouchedSessions cmds
           in checkCoverage $
                classify (len <= 3) "short command sequence" $
                  classify (len > 3 && len <= 7) "medium command sequence" $
                    classify (len > 7) "long command sequence" $
                      classify (touchedSessions <= 1) "single-session commands" $
                        classify (touchedSessions >= 2) "multi-session commands" $
                          cover 20 (len > 7) "long sequences" $
                            cover 20 (touchedSessions >= 2) "multiple sessions touched" $
                              monadicIO $ do
                                ct <- run (newCostTracker Nothing Nothing)
                                model <- run (newIORef (Map.empty :: Map.Map SessionId (Word64, Word64)))
                                mapM_ (executeCmd ct model) cmds
                                finalModel <- run (readIORef model)
                                results <-
                                  run $
                                    mapM
                                      ( \(sid, (expIn, expOut)) -> do
                                          (actIn, actOut, _) <- getCost ct sid
                                          pure (actIn == expIn && actOut == expOut)
                                      )
                                      (Map.toList finalModel)
                                assert (and results)

-- --------------------------------------------------------------------
-- State machine helpers
-- --------------------------------------------------------------------

data CostCmd
  = CmdRecord SessionId UserId Text Word64 Word64
  | CmdReset SessionId
  deriving stock (Show)

instance Arbitrary CostCmd where
  arbitrary =
    oneof
      [ CmdRecord <$> genSessionId <*> genUserId <*> genModel <*> genTokens <*> genTokens
      , CmdReset <$> genSessionId
      ]
  shrink (CmdReset sid) =
    [CmdReset sid' | sid' <- shrinkSessionId sid]
  shrink (CmdRecord sid uid mdl inp out) =
    [CmdReset sid]
      ++ [ CmdRecord sid' uid mdl inp out
         | sid' <- shrinkSessionId sid
         ]
      ++ [ CmdRecord sid uid' mdl inp out
         | uid' <- shrinkUserId uid
         ]
      ++ [ CmdRecord sid uid mdl' inp out
         | mdl' <- shrinkModel mdl
         ]
      ++ [ CmdRecord sid uid mdl inp' out
         | inp' <- shrink inp
         ]
      ++ [ CmdRecord sid uid mdl inp out'
         | out' <- shrink out
         ]

genCommandSequence :: Gen [CostCmd]
genCommandSequence = resize 10 (listOf arbitrary)

executeCmd :: CostTracker -> IORef (Map.Map SessionId (Word64, Word64)) -> CostCmd -> PropertyM IO ()
executeCmd ct model cmd = case cmd of
  CmdRecord sid uid mdl inp out -> do
    _ <- run (record ct sid uid mdl inp out)
    run $ modifyIORef' model (Map.insertWith addPair sid (inp, out))
  CmdReset sid -> do
    run (resetSession ct sid)
    run $ modifyIORef' model (Map.delete sid)
 where
  addPair (a, b) (c, d) = (a + c, b + d)

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

isBudgetExhausted :: RecordResult -> Bool
isBudgetExhausted (BudgetExhausted _) = True
isBudgetExhausted _ = False

shrinkSessionId :: SessionId -> [SessionId]
shrinkSessionId (SessionId s) =
  [ SessionId (Text.pack sid')
  | sid' <- shrink (Text.unpack s)
  , not (null sid')
  ]

shrinkUserId :: UserId -> [UserId]
shrinkUserId (UserId u) =
  [ UserId (Text.pack uid')
  | uid' <- shrink (Text.unpack u)
  , not (null uid')
  ]

shrinkModel :: Text -> [Text]
shrinkModel mdl = [m | m <- models, m /= mdl]
 where
  models = ["unknown", "gpt-4o", "claude-haiku-3", "claude-sonnet-4", "claude-opus-4"]

countTouchedSessions :: [CostCmd] -> Int
countTouchedSessions cmds =
  Set.size (Set.fromList (map cmdSessionId cmds))
 where
  cmdSessionId (CmdRecord sid _ _ _ _) = sid
  cmdSessionId (CmdReset sid) = sid
