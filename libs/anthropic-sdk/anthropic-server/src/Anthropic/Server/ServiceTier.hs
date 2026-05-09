-- | Service tier routing for Anthropic API-conforming servers.
module Anthropic.Server.ServiceTier
  ( -- * Service Tier Router
    ServiceTierRouter
  , newServiceTierRouter

    -- * Assignment
  , ServiceTierAssignment (..)
  , AssignedTier (..)
  , assignTier
  ) where

import Control.Concurrent.STM
import Anthropic.Types (ServiceTierPreference (..))
import Anthropic.Protocol.Message (MessageRequest)

-- | Assigned service tier.
data AssignedTier
  = TierStandard
  | TierPriority
  | TierBatch
  deriving stock (Eq, Show)

-- | Result of service tier assignment.
data ServiceTierAssignment = ServiceTierAssignment
  { requested           :: !ServiceTierPreference
  , assigned            :: !AssignedTier
  , capacityBurndownRate :: !(Maybe Double)
  }
  deriving stock (Eq, Show)

-- | Internal capacity state.
data CapacityState = CapacityState
  { standardCapacity :: !Double
  , priorityCapacity :: !Double
  , batchCapacity    :: !Double
  }

-- | Opaque service tier router.
data ServiceTierRouter = ServiceTierRouter
  { routerCapacity :: !(TVar CapacityState)
  }

-- | Create a new service tier router with initial capacity.
newServiceTierRouter :: IO ServiceTierRouter
newServiceTierRouter = do
  capacity <- newTVarIO CapacityState
    { standardCapacity = 100.0
    , priorityCapacity = 100.0
    , batchCapacity = 100.0
    }
  pure ServiceTierRouter { routerCapacity = capacity }

-- | Assign a service tier to a request based on capacity and preference.
assignTier
  :: ServiceTierRouter
  -> MessageRequest
  -> ServiceTierPreference
  -> IO ServiceTierAssignment
assignTier router _req preference = atomically $ do
  capacity <- readTVar router.routerCapacity

  case preference of
    ServiceTierAuto -> do
      -- Auto: assign based on available capacity
      -- Priority: priority > standard > batch
      let (tier, burndownRate) =
            if capacity.priorityCapacity > 10
            then (TierPriority, Just 1.5)
            else if capacity.standardCapacity > 10
            then (TierStandard, Just 1.0)
            else (TierBatch, Just 0.5)

      -- Burn down capacity
      let newCapacity = case tier of
            TierPriority -> capacity { priorityCapacity = capacity.priorityCapacity - 1 }
            TierStandard -> capacity { standardCapacity = capacity.standardCapacity - 1 }
            TierBatch    -> capacity { batchCapacity = capacity.batchCapacity - 1 }

      writeTVar router.routerCapacity newCapacity

      pure ServiceTierAssignment
        { requested = preference
        , assigned = tier
        , capacityBurndownRate = burndownRate
        }

    StandardOnly -> do
      -- Standard only: always assign standard
      let newCapacity = capacity { standardCapacity = capacity.standardCapacity - 1 }
      writeTVar router.routerCapacity newCapacity

      pure ServiceTierAssignment
        { requested = preference
        , assigned = TierStandard
        , capacityBurndownRate = Just 1.0
        }
