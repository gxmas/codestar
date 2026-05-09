module CodeStar.Platform.CostTracker
  ( -- * Tracker
    CostTracker (..)
  , newCostTracker

    -- * Recording
  , RecordResult (..)
  , record
  , getCost
  , resetSession

    -- * Internal (Testing)
  , estimateCost
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)

import CodeStar.Types (SessionId (..), UserId (..))

-- --------------------------------------------------------------------
-- Cost model (very rough per-token estimates)
-- --------------------------------------------------------------------

-- USD per million tokens (input / output) by provider prefix
tokenCostUsd :: Text -> (Double, Double)
tokenCostUsd modelName
  | "claude-haiku" `Text.isPrefixOf` modelName = (0.25, 1.25)
  | "claude-sonnet" `Text.isPrefixOf` modelName = (3.0, 15.0)
  | "claude-opus" `Text.isPrefixOf` modelName = (15.0, 75.0)
  | "gpt-4" `Text.isPrefixOf` modelName = (10.0, 30.0)
  | otherwise = (1.0, 3.0)

estimateCost :: Text -> Word64 -> Word64 -> Double
estimateCost model inputTok outputTok =
  let (inRate, outRate) = tokenCostUsd model
   in (fromIntegral inputTok * inRate + fromIntegral outputTok * outRate) / 1_000_000

-- --------------------------------------------------------------------
-- State
-- --------------------------------------------------------------------

data SessionCost = SessionCost
  { scInputTokens :: !Word64
  , scOutputTokens :: !Word64
  , scCostUsd :: !Double
  }

emptySessionCost :: SessionCost
emptySessionCost = SessionCost 0 0 0.0

data TrackerState = TrackerState
  { tsSessions :: !(Map SessionId SessionCost)
  , tsUsers :: !(Map UserId Word64) -- daily input token totals
  }

emptyState :: TrackerState
emptyState = TrackerState Map.empty Map.empty

-- --------------------------------------------------------------------
-- Result
-- --------------------------------------------------------------------

data RecordResult
  = Recorded
  | -- | reason: session or daily limit
    BudgetExhausted Text
  deriving stock (Eq, Show)

-- --------------------------------------------------------------------
-- Tracker
-- --------------------------------------------------------------------

data CostTracker = CostTracker
  { recordTokens :: SessionId -> UserId -> Text -> Word64 -> Word64 -> IO RecordResult
  , sessionCost :: SessionId -> IO (Word64, Word64, Double)
  , userDailyTokens :: UserId -> IO Word64
  , clearSession :: SessionId -> IO ()
  }

-- | Create a new in-memory cost tracker with optional budget limits.
newCostTracker ::
  -- | per-session token limit
  Maybe Word64 ->
  -- | per-user daily token limit
  Maybe Word64 ->
  IO CostTracker
newCostTracker sessionLimit dailyLimit = do
  ref <- newIORef emptyState
  pure
    CostTracker
      { recordTokens = doRecord ref sessionLimit dailyLimit
      , sessionCost = doGetCost ref
      , userDailyTokens = doDailyToks ref
      , clearSession = doClear ref
      }

-- --------------------------------------------------------------------
-- Public operations
-- --------------------------------------------------------------------

record ::
  CostTracker ->
  SessionId ->
  UserId ->
  -- | model name for cost estimation
  Text ->
  -- | input tokens
  Word64 ->
  -- | output tokens
  Word64 ->
  IO RecordResult
record ct = ct.recordTokens

getCost :: CostTracker -> SessionId -> IO (Word64, Word64, Double)
getCost ct = ct.sessionCost

resetSession :: CostTracker -> SessionId -> IO ()
resetSession ct = ct.clearSession

-- --------------------------------------------------------------------
-- Implementation
-- --------------------------------------------------------------------

doRecord ::
  IORef TrackerState ->
  Maybe Word64 ->
  Maybe Word64 ->
  SessionId ->
  UserId ->
  Text ->
  Word64 ->
  Word64 ->
  IO RecordResult
doRecord ref sessionLimit dailyLimit sid uid model inputTok outputTok = do
  state <- readIORef ref
  let sc = Map.findWithDefault emptySessionCost sid state.tsSessions
      newIn = sc.scInputTokens + inputTok
      newOut = sc.scOutputTokens + outputTok
      newCost = sc.scCostUsd + estimateCost model inputTok outputTok
      newSc = SessionCost newIn newOut newCost
      daily = Map.findWithDefault 0 uid state.tsUsers + inputTok + outputTok

  case sessionLimit of
    Just lim
      | newIn + newOut > lim ->
          pure (BudgetExhausted ("Session token limit " <> Text.pack (show lim) <> " exceeded"))
    _ -> case dailyLimit of
      Just lim
        | daily > lim ->
            pure (BudgetExhausted ("Daily token limit " <> Text.pack (show lim) <> " exceeded"))
      _ -> do
        atomicModifyIORef'
          ref
          ( \s ->
              ( s
                  { tsSessions = Map.insert sid newSc s.tsSessions
                  , tsUsers = Map.insert uid daily s.tsUsers
                  }
              , ()
              )
          )
        pure Recorded

doGetCost :: IORef TrackerState -> SessionId -> IO (Word64, Word64, Double)
doGetCost ref sid = do
  state <- readIORef ref
  let sc = Map.findWithDefault emptySessionCost sid state.tsSessions
  pure (sc.scInputTokens, sc.scOutputTokens, sc.scCostUsd)

doDailyToks :: IORef TrackerState -> UserId -> IO Word64
doDailyToks ref uid = do
  state <- readIORef ref
  pure (Map.findWithDefault 0 uid state.tsUsers)

doClear :: IORef TrackerState -> SessionId -> IO ()
doClear ref sid =
  atomicModifyIORef'
    ref
    ( \s ->
        (s{tsSessions = Map.delete sid s.tsSessions}, ())
    )
