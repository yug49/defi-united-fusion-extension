{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Contracts.EscrowSrc
Description : Source Escrow contract for cross-chain atomic swap on Cardano
Copyright   : (c) 2025
License     : MIT

This module provides the source escrow functionality for cross-chain atomic swaps.
It handles the initial locking of funds and their unlock with secret verification.
Inherits from BaseEscrow and implements source-chain specific timing rules.

Key functionality:
- Lock ADA at deployment time
- Allow withdrawal with secret during source withdrawal window
- Allow cancellation during source cancellation window
- Support public operations via resolver registry
- Handle safety deposits appropriately
-}

module Contracts.EscrowSrc
    ( -- * Types
      EscrowSrcState(..)
    , EscrowSrcAction(..)
      -- * Core Functions
    , createEscrowSrc
    , validateEscrowSrcAction
      -- * Specific Source Chain Functions
    , withdraw
    , withdrawTo
    , cancel
    , publicWithdraw
    , publicCancel
      -- * Constructor
    , mkEscrowSrc
    ) where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

-- Import base functionality
import Contracts.BaseEscrow
import Lib.TimelocksLib (Stage(..), getTimelock, isStageActive, mkTimelocks)

-- | Extended state for EscrowSrc with source-specific fields
data EscrowSrcState = EscrowSrcState
    { baseState :: EscrowState          -- Base escrow state
    , isSourceChain :: Bool             -- Always True for source escrow
    } deriving (Show, Eq, Generic)

-- | Actions specific to source escrow
data EscrowSrcAction
    = SrcWithdraw 
        { secret :: ByteString          -- Secret to unlock funds
        , caller :: ByteString          -- Transaction sender
        }
    | SrcWithdrawTo
        { secret :: ByteString          -- Secret to unlock funds
        , caller :: ByteString          -- Transaction sender (for safety deposit)
        , target :: ByteString          -- Target address for main amount
        }
    | SrcCancel 
        { caller :: ByteString          -- Transaction sender
        }
    | SrcPublicWithdraw 
        { secret :: ByteString          -- Secret to unlock funds
        , resolver :: ByteString        -- Authorized resolver
        , target :: ByteString          -- Target address for main amount
        }
    | SrcPublicCancel 
        { resolver :: ByteString        -- Authorized resolver
        , refundTarget :: ByteString    -- Target for refund
        }
    deriving (Show, Eq, Generic)

-- | Create a new EscrowSrc state with source-specific initialization
createEscrowSrc :: Immutables -> ByteString -> Integer -> ResolverRegistry 
                -> Either BaseEscrowError EscrowSrcState
createEscrowSrc immuts factory rescueDelay registry = do
    -- Validate immutables first
    validateImmutables immuts
    
    -- Calculate required balance (main amount + safety deposit)
    let requiredBalance = totalAmount immuts
    
    -- Create base escrow state
    let baseEscrowState = EscrowState
            { immutables = immuts
            , factory = factory
            , rescueDelay = rescueDelay
            , resolverRegistry = registry
            , isActive = True
            , currentBalance = requiredBalance
            }
    
    -- Create source escrow state
    return $ EscrowSrcState
        { baseState = baseEscrowState
        , isSourceChain = True
        }

-- | Constructor helper function
mkEscrowSrc :: ByteString -> ByteString -> ByteString -> ByteString 
            -> Integer -> Integer -> Integer -> ResolverRegistry
            -> Either BaseEscrowError EscrowSrcState
mkEscrowSrc orderHash secret makerAddr takerAddr amount safetyDeposit 
           rescueDelay registry = do
    -- Create timelocks with source-appropriate timing
    -- For source chain: earlier withdrawal, later cancellation
    let deployTime = 0  -- Will be set when actually deployed
    let srcWithdrawTime = 1000      -- 1000 seconds after deployment
    let srcPublicWithdrawTime = 1500 -- 1500 seconds (public access)
    let srcCancelTime = 2000        -- 2000 seconds (cancellation allowed)
    let srcPublicCancelTime = 2200  -- 2200 seconds (public cancellation)
    let dstWithdrawTime = 1200      -- Destination withdrawal time
    let dstPublicWithdrawTime = 1600 -- Destination public withdrawal
    let dstCancelTime = 1800        -- Destination cancellation
    
    let timelocks = mkTimelocks deployTime srcWithdrawTime srcPublicWithdrawTime 
                               srcCancelTime srcPublicCancelTime dstWithdrawTime 
                               dstPublicWithdrawTime dstCancelTime
    
    -- Create immutables
    immutables <- mkImmutables orderHash secret makerAddr takerAddr 
                              amount safetyDeposit timelocks
    
    -- Create escrow src state
    createEscrowSrc immutables (BC.pack "factory_address") rescueDelay registry

-- | Main validation function for source escrow actions
validateEscrowSrcAction :: EscrowSrcState -> EscrowSrcAction -> Integer 
                        -> Either BaseEscrowError EscrowSrcState
validateEscrowSrcAction srcState action currentTime = do
    case action of
        SrcWithdraw secret caller -> do
            withdraw srcState secret caller currentTime
            
        SrcWithdrawTo secret caller target -> do
            withdrawTo srcState secret caller target currentTime
            
        SrcCancel caller -> do
            cancel srcState caller currentTime
            
        SrcPublicWithdraw secret resolver target -> do
            publicWithdraw srcState secret resolver target currentTime
            
        SrcPublicCancel resolver refundTarget -> do
            publicCancel srcState resolver refundTarget currentTime

-- | Private withdrawal function (taker only, during source withdrawal window)
-- Transfers main amount to taker, safety deposit to taker (same person)
withdraw :: EscrowSrcState -> ByteString -> ByteString -> Integer 
         -> Either BaseEscrowError EscrowSrcState
withdraw srcState secret caller currentTime = do
    let base = baseState srcState
    let immuts = immutables base
    
    -- Validate caller is taker
    validateTaker caller immuts
    
    -- Validate secret
    validateSecret secret immuts
    
    -- Validate timing (source withdrawal window)
    validateSrcWithdrawTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Calculate transfer amounts
    let mainAmount = amount immuts
    let safetyAmount = safetyDeposit immuts
    let totalTransfer = mainAmount + safetyAmount
    
    -- Validate sufficient funds
    validateSufficientFunds base totalTransfer
    
    -- Transfer both amounts to caller (taker)
    transferAda totalTransfer caller
    
    -- Update state
    let updatedBase = base 
            { currentBalance = currentBalance base - totalTransfer
            , isActive = False
            }
    
    return $ srcState { baseState = updatedBase }

-- | Private withdrawal to specific target (taker only, during source withdrawal window)  
-- Transfers main amount to target, safety deposit to caller
withdrawTo :: EscrowSrcState -> ByteString -> ByteString -> ByteString -> Integer
           -> Either BaseEscrowError EscrowSrcState
withdrawTo srcState secret caller target currentTime = do
    let base = baseState srcState
    let immuts = immutables base
    
    -- Validate caller is taker
    validateTaker caller immuts
    
    -- Validate secret
    validateSecret secret immuts
    
    -- Validate timing (source withdrawal window)
    validateSrcWithdrawTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Validate target is not empty
    if BS.null target
        then Left InvalidCaller
        else do
            -- Calculate transfer amounts
            let mainAmount = amount immuts
            let safetyAmount = safetyDeposit immuts
            let totalTransfer = mainAmount + safetyAmount
            
            -- Validate sufficient funds
            validateSufficientFunds base totalTransfer
            
            -- Transfer main amount to target
            transferAda mainAmount target
            
            -- Transfer safety deposit to caller
            transferAda safetyAmount caller
            
            -- Update state
            let updatedBase = base 
                    { currentBalance = currentBalance base - totalTransfer
                    , isActive = False
                    }
            
            return $ srcState { baseState = updatedBase }

-- | Private cancellation function (taker only, after source cancellation time)
-- Returns both main amount and safety deposit to caller (taker)
cancel :: EscrowSrcState -> ByteString -> Integer 
       -> Either BaseEscrowError EscrowSrcState
cancel srcState caller currentTime = do
    let base = baseState srcState
    let immuts = immutables base
    
    -- Validate caller is taker
    validateTaker caller immuts
    
    -- Validate timing (source cancellation window)
    validateSrcCancelTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Calculate refund amount (total funds)
    let refundAmount = totalAmount immuts
    
    -- Validate sufficient funds
    validateSufficientFunds base refundAmount
    
    -- Transfer full refund to caller (taker)
    transferAda refundAmount caller
    
    -- Update state
    let updatedBase = base 
            { currentBalance = currentBalance base - refundAmount
            , isActive = False
            }
    
    return $ srcState { baseState = updatedBase }

-- | Public withdrawal function (resolver only, during public withdrawal window)
-- Transfers main amount to target, safety deposit to resolver as fee
publicWithdraw :: EscrowSrcState -> ByteString -> ByteString -> ByteString -> Integer
               -> Either BaseEscrowError EscrowSrcState
publicWithdraw srcState secret resolver target currentTime = do
    let base = baseState srcState
    let immuts = immutables base
    
    -- Validate resolver is authorized
    validateResolver resolver (resolverRegistry base)
    
    -- Validate secret
    validateSecret secret immuts
    
    -- Validate timing (source public withdrawal window)
    validatePublicWithdrawTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Validate target is not empty
    if BS.null target
        then Left InvalidCaller
        else do
            -- Calculate transfer amounts
            let mainAmount = amount immuts
            let safetyAmount = safetyDeposit immuts
            let totalTransfer = mainAmount + safetyAmount
            
            -- Validate sufficient funds
            validateSufficientFunds base totalTransfer
            
            -- Transfer main amount to target
            transferAda mainAmount target
            
            -- Transfer safety deposit to resolver as fee
            transferAda safetyAmount resolver
            
            -- Update state
            let updatedBase = base 
                    { currentBalance = currentBalance base - totalTransfer
                    , isActive = False
                    }
            
            return $ srcState { baseState = updatedBase }

-- | Public cancellation function (resolver only, after public cancellation time)
-- Returns full amount to specified refund target
publicCancel :: EscrowSrcState -> ByteString -> ByteString -> Integer
             -> Either BaseEscrowError EscrowSrcState
publicCancel srcState resolver refundTarget currentTime = do
    let base = baseState srcState
    let immuts = immutables base
    
    -- Validate resolver is authorized
    validateResolver resolver (resolverRegistry base)
    
    -- Validate timing (source public cancellation window)
    validatePublicCancelTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Validate refund target is not empty
    if BS.null refundTarget
        then Left InvalidCaller
        else do
            -- Calculate refund amount (total funds)
            let refundAmount = totalAmount immuts
            
            -- Validate sufficient funds
            validateSufficientFunds base refundAmount
            
            -- Transfer full refund to specified target
            transferAda refundAmount refundTarget
            
            -- Update state
            let updatedBase = base 
                    { currentBalance = currentBalance base - refundAmount
                    , isActive = False
                    }
            
            return $ srcState { baseState = updatedBase }
