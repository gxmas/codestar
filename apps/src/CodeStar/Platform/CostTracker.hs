{- |
= Platform.CostTracker — token usage and budget enforcement

Tracks the number of tokens consumed per session and per user (daily),
and enforces configurable budget limits to prevent runaway costs.

== Design

'CostTracker' is a record-of-functions backed by an in-memory 'IORef'.
All state updates are done with 'atomicModifyIORef'' to avoid races when
multiple sessions update the tracker concurrently.

== Cost model

Token costs are estimated using hard-coded per-million-token rates for
known model families.  These are rough heuristics — the actual cost
depends on pricing tiers, discounts, and prompt caching.  The estimates
are useful for soft budget warnings, not billing.

== Budget enforcement

When 'record' detects that a session or daily limit would be exceeded, it
returns 'BudgetExhausted' without updating the counts.  The agent loop
treats this as a terminal error and stops the session.
-}
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

-- | Estimate the cost in USD for a single API call.
-- Uses hard-coded per-million-token rates keyed by model name prefix.
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

-- | A token-usage tracker, implemented as a record of functions over a
-- shared in-memory state.  Thread-safe via 'atomicModifyIORef''.
data CostTracker = CostTracker
  { recordTokens    :: SessionId -> UserId -> Text -> Word64 -> Word64 -> IO RecordResult
  -- ^ Record token usage for a turn.  Returns 'BudgetExhausted' if either
  --   the session or daily limit would be exceeded; does not update state
  --   in that case.
  , sessionCost     :: SessionId -> IO (Word64, Word64, Double)
  -- ^ Return @(inputTokens, outputTokens, estimatedCostUsd)@ for a session.
  , userDailyTokens :: UserId -> IO Word64
  -- ^ Return the total tokens consumed by a user today.
  , clearSession    :: SessionId -> IO ()
  -- ^ Remove all session cost data (called when a session is destroyed).
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
