{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Lib.TimelocksLib
Description : Timelocks library for cross-chain swap escrows
Copyright   : (c) 2025
License     : MIT

This module provides timelock functionality for cross-chain swaps.
Instead of using bit manipulation for gas efficiency (like in Solidity),
we use clear record structures for better readability and maintainability.

Timelocks define different time periods during which specific actions
can be performed on escrow contracts.
-}

module Lib.TimelocksLib 
    ( -- * Types
      Timelocks(..)
    , Stage(..)
    , SrcTimelocks(..)
    , DstTimelocks(..)
      -- * Construction
    , mkTimelocks
    , setDeployedAt
      -- * Queries
    , rescueStart
    , getTimelock
    , isStageActive
    , getCurrentStage
      -- * Utilities
    , addSeconds
    ) where

import GHC.Generics (Generic)
import Data.List (maximumBy)

-- | Enumeration of all possible stages in the escrow lifecycle
-- Each stage represents a different time period with different permissions
data Stage 
    = SrcWithdrawal        -- ^ Only taker can withdraw on source chain (with secret)
    | SrcPublicWithdrawal  -- ^ Anyone can withdraw on source chain (with secret)  
    | SrcCancellation      -- ^ Only taker can cancel escrow
    | SrcPublicCancellation -- ^ Anyone can cancel escrow
    | DstWithdrawal        -- ^ Only maker can withdraw on destination chain (with secret)
    | DstPublicWithdrawal  -- ^ Anyone can withdraw on destination chain (with secret)
    | DstCancellation      -- ^ Escrow can be cancelled on destination chain
    deriving (Show, Eq, Ord, Enum, Generic)

-- | Source chain timelock periods (in seconds from deployment)
-- These define when different actions become available on the source chain
data SrcTimelocks = SrcTimelocks
    { srcWithdrawal :: Integer        -- ^ When taker can withdraw (with secret)
    , srcPublicWithdrawal :: Integer  -- ^ When anyone can withdraw (with secret)
    , srcCancellation :: Integer      -- ^ When taker can cancel
    , srcPublicCancellation :: Integer -- ^ When anyone can cancel
    } deriving (Show, Eq, Generic)

-- | Destination chain timelock periods (in seconds from deployment)  
-- These define when different actions become available on the destination chain
data DstTimelocks = DstTimelocks
    { dstWithdrawal :: Integer       -- ^ When maker can withdraw (with secret)
    , dstPublicWithdrawal :: Integer -- ^ When anyone can withdraw (with secret)
    , dstCancellation :: Integer     -- ^ When escrow can be cancelled
    } deriving (Show, Eq, Generic)

-- | Complete timelock configuration for an escrow
-- Contains all timing information for both source and destination chains
data Timelocks = Timelocks
    { deployedAt :: Integer      -- ^ Unix timestamp when escrow was deployed
    , srcTimelocks :: SrcTimelocks -- ^ Source chain timelock periods
    , dstTimelocks :: DstTimelocks -- ^ Destination chain timelock periods
    } deriving (Show, Eq, Generic)

-- | Create a new Timelocks structure with all timelock periods
-- 
-- Parameters:
-- - deploymentTime: Unix timestamp when the escrow is deployed
-- - srcWith: Seconds from deployment when taker can withdraw on source
-- - srcPubWith: Seconds from deployment when anyone can withdraw on source  
-- - srcCancel: Seconds from deployment when taker can cancel
-- - srcPubCancel: Seconds from deployment when anyone can cancel
-- - dstWith: Seconds from deployment when maker can withdraw on destination
-- - dstPubWith: Seconds from deployment when anyone can withdraw on destination
-- - dstCancel: Seconds from deployment when destination can be cancelled
mkTimelocks :: Integer -> Integer -> Integer -> Integer -> Integer 
           -> Integer -> Integer -> Integer -> Timelocks
mkTimelocks deploymentTime srcWith srcPubWith srcCancel srcPubCancel 
           dstWith dstPubWith dstCancel = 
    Timelocks
        { deployedAt = deploymentTime
        , srcTimelocks = SrcTimelocks
            { srcWithdrawal = srcWith
            , srcPublicWithdrawal = srcPubWith  
            , srcCancellation = srcCancel
            , srcPublicCancellation = srcPubCancel
            }
        , dstTimelocks = DstTimelocks
            { dstWithdrawal = dstWith
            , dstPublicWithdrawal = dstPubWith
            , dstCancellation = dstCancel
            }
        }

-- | Update the deployment timestamp of an existing Timelocks
-- This is useful when the actual deployment time differs from the planned time
setDeployedAt :: Timelocks -> Integer -> Timelocks  
setDeployedAt timelocks newDeployedAt = 
    timelocks { deployedAt = newDeployedAt }

-- | Calculate when the rescue period starts
-- The rescue period is an additional safety mechanism that starts after
-- a specified delay from deployment
--
-- Parameters:
-- - timelocks: The timelock configuration
-- - rescueDelay: Additional seconds to wait before rescue can start
--
-- Returns: Unix timestamp when rescue period begins
rescueStart :: Timelocks -> Integer -> Integer
rescueStart timelocks rescueDelay = 
    deployedAt timelocks + rescueDelay

-- | Get the absolute timestamp when a specific stage becomes active
-- This converts relative timelock periods to absolute timestamps
--
-- Parameters:  
-- - timelocks: The timelock configuration
-- - stage: Which stage to get the timestamp for
--
-- Returns: Unix timestamp when the stage becomes active
getTimelock :: Timelocks -> Stage -> Integer
getTimelock timelocks stage = 
    let baseTime = deployedAt timelocks
        src = srcTimelocks timelocks
        dst = dstTimelocks timelocks
    in baseTime + case stage of
        SrcWithdrawal        -> srcWithdrawal src
        SrcPublicWithdrawal  -> srcPublicWithdrawal src
        SrcCancellation      -> srcCancellation src  
        SrcPublicCancellation -> srcPublicCancellation src
        DstWithdrawal        -> dstWithdrawal dst
        DstPublicWithdrawal  -> dstPublicWithdrawal dst
        DstCancellation      -> dstCancellation dst

-- | Check if a specific stage is currently active
-- A stage is active if the current time is past its start time
--
-- Parameters:
-- - timelocks: The timelock configuration  
-- - stage: Which stage to check
-- - currentTime: Current Unix timestamp
--
-- Returns: True if the stage is currently active
isStageActive :: Timelocks -> Stage -> Integer -> Bool
isStageActive timelocks stage currentTime = 
    currentTime >= getTimelock timelocks stage

-- | Determine which stage is currently active based on current time
-- Returns the most advanced stage that is currently active
--
-- Parameters:
-- - timelocks: The timelock configuration
-- - currentTime: Current Unix timestamp  
--
-- Returns: The currently active stage (if any)
getCurrentStage :: Timelocks -> Integer -> Maybe Stage
getCurrentStage timelocks currentTime = 
    -- Find the latest stage that is currently active
    -- We need to find the stage with the highest absolute timestamp that's still <= currentTime
    let allStages = [SrcWithdrawal, SrcPublicWithdrawal, SrcCancellation, 
                    SrcPublicCancellation, DstWithdrawal, DstPublicWithdrawal, 
                    DstCancellation]
        -- Get (stage, absoluteTime) pairs for active stages
        stageWithTimes = [(stage, getTimelock timelocks stage) | stage <- allStages]
        activeStageWithTimes = [(stage, time) | (stage, time) <- stageWithTimes, time <= currentTime]
    in if null activeStageWithTimes 
       then Nothing  -- No stage is active yet
       else Just $ fst $ maximumBy (\(_, t1) (_, t2) -> compare t1 t2) activeStageWithTimes

-- | Utility function to add seconds to a timestamp
-- This is a helper function for time calculations
--
-- Parameters:
-- - timestamp: Base Unix timestamp
-- - seconds: Number of seconds to add
--
-- Returns: New timestamp with seconds added
addSeconds :: Integer -> Integer -> Integer  
addSeconds timestamp seconds = timestamp + seconds
