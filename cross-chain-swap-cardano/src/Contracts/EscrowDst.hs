{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Contracts.EscrowDst
Description : Destination Escrow contract for cross-chain atomic swap on Cardano
Copyright   : (c) 2025
License     : MIT

This module provides the destination escrow functionality for cross-chain atomic swaps.
It handles the locking of funds on the destination chain and their unlock with secret verification.
Inherits from BaseEscrow and implements destination-chain specific timing rules.

Key functionality:
- Lock ADA at deployment time (taker calls createDstEscrow)
- Allow withdrawal with secret during destination withdrawal window (transfers to maker)
- Allow cancellation during destination cancellation window (refunds to taker)
- Support public operations via resolver registry
- Handle safety deposits appropriately
-}

module Contracts.EscrowDst
    ( -- * Types
      EscrowDstState(..)
    , EscrowDstAction(..)
      -- * Core Functions
    , createEscrowDst
    , validateEscrowDstAction
      -- * Specific Destination Chain Functions
    , withdraw
    , cancel
    , publicWithdraw
    , publicCancel
      -- * Constructor
    , mkEscrowDst
      -- * Display Function
    , displayEscrowDst
    ) where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

-- Import base functionality
import qualified Contracts.BaseEscrow as Base
import Contracts.BaseEscrow (EscrowState(..), Immutables(..), BaseEscrowError(..), 
                            ResolverRegistry, validateImmutables, validateSecret, 
                            validateTaker, validateSufficientFunds, validateResolver,
                            validatePublicWithdrawTime, validatePublicCancelTime,
                            mkImmutables)
import Lib.TimelocksLib (Stage(..), getTimelock, isStageActive, mkTimelocks)

-- | Extended state for EscrowDst with destination-specific fields
data EscrowDstState = EscrowDstState
    { baseState :: EscrowState          -- Base escrow state
    , isDestinationChain :: Bool        -- Always True for destination escrow
    } deriving (Show, Eq, Generic)

-- | Actions specific to destination escrow
data EscrowDstAction
    = DstWithdraw 
        { secret :: ByteString          -- Secret to unlock funds
        , caller :: ByteString          -- Transaction sender (should be maker)
        }
    | DstCancel 
        { caller :: ByteString          -- Transaction sender (should be taker)
        }
    | DstPublicWithdraw 
        { secret :: ByteString          -- Secret to unlock funds
        , resolver :: ByteString        -- Authorized resolver
        }
    | DstPublicCancel 
        { resolver :: ByteString        -- Authorized resolver
        , refundTarget :: ByteString    -- Target for refund
        }
    deriving (Show, Eq, Generic)

-- | Create a new EscrowDst state with destination-specific initialization
createEscrowDst :: Immutables -> ByteString -> Integer -> ResolverRegistry 
                -> Either BaseEscrowError EscrowDstState
createEscrowDst immuts factory rescueDelay registry = do
    -- Validate immutables first
    validateImmutables immuts
    
    -- Calculate required balance (main amount + safety deposit)
    let requiredBalance = Base.totalAmount immuts
    
    -- Create base escrow state
    let baseEscrowState = EscrowState
            { immutables = immuts
            , factory = factory
            , rescueDelay = rescueDelay
            , resolverRegistry = registry
            , isActive = True
            , currentBalance = requiredBalance
            }
    
    -- Create destination escrow state
    return $ EscrowDstState
        { baseState = baseEscrowState
        , isDestinationChain = True
        }

-- | Constructor helper function
mkEscrowDst :: ByteString -> ByteString -> ByteString -> ByteString 
            -> Integer -> Integer -> Integer -> ResolverRegistry
            -> Either BaseEscrowError EscrowDstState
mkEscrowDst orderHash secret makerAddr takerAddr amount safetyDeposit 
           rescueDelay registry = do
    -- Create timelocks with destination-appropriate timing
    -- For destination chain: later withdrawal, earlier cancellation than source
    let deployTime = 0  -- Will be set when actually deployed
    let srcWithdrawTime = 1000      -- Source withdrawal starts first
    let srcPublicWithdrawTime = 1500 -- Source public withdrawal
    let srcCancelTime = 2000        -- Source cancellation
    let srcPublicCancelTime = 2200  -- Source public cancellation
    let dstWithdrawTime = 1200      -- Destination withdrawal (after source starts)
    let dstPublicWithdrawTime = 1600 -- Destination public withdrawal
    let dstCancelTime = 1800        -- Destination cancellation (before source)
    
    let timelocks = mkTimelocks deployTime srcWithdrawTime srcPublicWithdrawTime 
                               srcCancelTime srcPublicCancelTime dstWithdrawTime 
                               dstPublicWithdrawTime dstCancelTime
    
    -- Create immutables
    immutables <- mkImmutables orderHash secret makerAddr takerAddr 
                              amount safetyDeposit timelocks
    
    -- Create escrow dst state
    createEscrowDst immutables (BC.pack "factory_address") rescueDelay registry

-- | Main validation function for destination escrow actions
validateEscrowDstAction :: EscrowDstState -> EscrowDstAction -> Integer 
                        -> Either BaseEscrowError EscrowDstState
validateEscrowDstAction dstState action currentTime = do
    case action of
        DstWithdraw secret caller -> do
            withdraw dstState secret caller currentTime
            
        DstCancel caller -> do
            cancel dstState caller currentTime
            
        DstPublicWithdraw secret resolver -> do
            publicWithdraw dstState secret resolver currentTime
            
        DstPublicCancel resolver refundTarget -> do
            publicCancel dstState resolver refundTarget currentTime

-- | Private withdrawal function (maker only, during destination withdrawal window)
-- Transfers main amount to maker (caller), safety deposit to maker (same person)
withdraw :: EscrowDstState -> ByteString -> ByteString -> Integer 
         -> Either BaseEscrowError EscrowDstState
withdraw dstState secret caller currentTime = do
    let base = baseState dstState
    let immuts = immutables base
    
    -- Validate caller is maker (different from source where it's taker)
    Base.validateMaker caller immuts
    
    -- Validate secret
    validateSecret secret immuts
    
    -- Validate timing (destination withdrawal window)
    Base.validateDstWithdrawTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Calculate transfer amounts
    let mainAmount = amount immuts
    let safetyAmount = safetyDeposit immuts
    let totalTransfer = mainAmount + safetyAmount
    
    -- Validate sufficient funds
    validateSufficientFunds base totalTransfer
    
    -- Transfer both amounts to caller (maker)
    Base.transferAda totalTransfer caller
    
    -- Update state
    let updatedBase = base 
            { currentBalance = currentBalance base - totalTransfer
            , isActive = False
            }
    
    return $ dstState { baseState = updatedBase }

-- | Private cancellation function (taker only, after destination cancellation time)
-- Returns main amount to taker, safety deposit to caller (taker, same person)
cancel :: EscrowDstState -> ByteString -> Integer 
       -> Either BaseEscrowError EscrowDstState
cancel dstState caller currentTime = do
    let base = baseState dstState
    let immuts = immutables base
    
    -- Validate caller is taker
    validateTaker caller immuts
    
    -- Validate timing (destination cancellation window)
    Base.validateDstCancelTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Calculate transfer amounts
    -- Main amount goes to taker, safety deposit goes to caller (taker)
    let mainAmount = amount immuts
    let safetyAmount = safetyDeposit immuts
    let totalTransfer = mainAmount + safetyAmount
    
    -- Validate sufficient funds
    validateSufficientFunds base totalTransfer
    
    -- Transfer main amount to taker (same as caller in this case)
    Base.transferAda mainAmount (taker immuts)
    
    -- Transfer safety deposit to caller
    Base.transferAda safetyAmount caller
    
    -- Update state
    let updatedBase = base 
            { currentBalance = currentBalance base - totalTransfer
            , isActive = False
            }
    
    return $ dstState { baseState = updatedBase }

-- | Public withdrawal function (resolver only, during public withdrawal window)
-- Transfers main amount to maker, safety deposit to resolver as fee
publicWithdraw :: EscrowDstState -> ByteString -> ByteString -> Integer
               -> Either BaseEscrowError EscrowDstState
publicWithdraw dstState secret resolver currentTime = do
    let base = baseState dstState
    let immuts = immutables base
    
    -- Validate resolver is authorized
    validateResolver resolver (resolverRegistry base)
    
    -- Validate secret
    validateSecret secret immuts
    
    -- Validate timing (destination public withdrawal window)
    validatePublicWithdrawTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Calculate transfer amounts
    let mainAmount = amount immuts
    let safetyAmount = safetyDeposit immuts
    let totalTransfer = mainAmount + safetyAmount
    
    -- Validate sufficient funds
    validateSufficientFunds base totalTransfer
    
    -- Transfer main amount to maker
    Base.transferAda mainAmount (maker immuts)
    
    -- Transfer safety deposit to resolver as fee
    Base.transferAda safetyAmount resolver
    
    -- Update state
    let updatedBase = base 
            { currentBalance = currentBalance base - totalTransfer
            , isActive = False
            }
    
    return $ dstState { baseState = updatedBase }

-- | Public cancellation function (resolver only, after public cancellation time)
-- Returns main amount to taker, safety deposit can be distributed as resolver fee
publicCancel :: EscrowDstState -> ByteString -> ByteString -> Integer
             -> Either BaseEscrowError EscrowDstState
publicCancel dstState resolver refundTarget currentTime = do
    let base = baseState dstState
    let immuts = immutables base
    
    -- Validate resolver is authorized
    validateResolver resolver (resolverRegistry base)
    
    -- Validate timing (destination public cancellation window)
    validatePublicCancelTime currentTime immuts
    
    -- Validate immutables
    validateImmutables immuts
    
    -- Validate refund target is not empty
    if BS.null refundTarget
        then Left InvalidCaller
        else do
            -- Calculate transfer amounts
            let mainAmount = amount immuts
            let safetyAmount = safetyDeposit immuts
            let totalTransfer = mainAmount + safetyAmount
            
            -- Validate sufficient funds
            validateSufficientFunds base totalTransfer
            
            -- Transfer main amount to taker
            Base.transferAda mainAmount (taker immuts)
            
            -- Transfer safety deposit to resolver as fee
            Base.transferAda safetyAmount resolver
            
            -- Update state
            let updatedBase = base 
                    { currentBalance = currentBalance base - totalTransfer
                    , isActive = False
                    }
            
            return $ dstState { baseState = updatedBase }

-- | Display EscrowDst state for debugging
displayEscrowDst :: EscrowDstState -> String
displayEscrowDst dstState = unlines
    [ "📋 EscrowDst State:"
    , "  Destination Chain: " ++ show (isDestinationChain dstState)
    , "  Base State Active: " ++ show (isActive (baseState dstState))
    , "  Current Balance: " ++ show (currentBalance (baseState dstState))
    ]
