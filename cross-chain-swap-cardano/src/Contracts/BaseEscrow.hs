{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Contracts.BaseEscrow
Description : Base abstract Escrow contract for cross-chain atomic swap on Cardano
Copyright   : (c) 2025
License     : MIT

This module provides the base escrow functionality for cross-chain atomic swaps.
It handles ADA transfers, timelock management, and resolver access control.
Similar to the Solidity version but adapted for Cardano's UTXO model.

Key differences from Solidity version:
- Uses resolver registry instead of access tokens
- Only handles ADA (no ERC20 tokens)
- Uses Blake2b_256 hashing instead of Keccak256
- Adapted for UTXO model instead of account model
-}

module Contracts.BaseEscrow
    ( -- * Types
      BaseEscrowError(..)
    , Immutables(..)
    , EscrowState(..)
    , EscrowAction(..)
    , ResolverRegistry(..)
      -- * Core Functions
    , validateEscrowAction
    , rescueFunds
    , transferAda
      -- * Validation Functions
    , validateImmutables
    , validateSecret
    , validateTaker
    , validateMaker
    , validateResolver
    , validateTimeConstraints
      -- * Utility Functions
    , hashSecret
    , mkImmutables
    ) where

import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash (Blake2b_256, Digest, hashWith)
import Crypto.Hash.Algorithms (Blake2b_256(..))

import Lib.TimelocksLib (Timelocks, Stage(..), getTimelock, rescueStart, isStageActive)

-- | Custom error types for the BaseEscrow contract
-- These correspond to the Solidity contract's custom errors
data BaseEscrowError
    = InvalidCaller           -- ^ Called by unauthorized address
    | InvalidSecret          -- ^ Provided secret doesn't match hashlock
    | InvalidTime            -- ^ Operation attempted outside valid time window
    | InvalidImmutables      -- ^ Immutables validation failed
    | NativeTokenSendingFailure -- ^ ADA transfer failed
    | InsufficientFunds      -- ^ Not enough ADA in the escrow
    | ResolverNotAuthorized  -- ^ Caller is not an authorized resolver
    | InvalidAmount          -- ^ Invalid amount specified
    | EscrowNotActive        -- ^ Escrow is not in active state
    deriving (Show, Eq, Generic)

-- | Immutables structure for the escrow
-- Contains all the unchanging parameters of an escrow contract
-- This is simpler than the Solidity version - no complex hashing needed
data Immutables = Immutables
    { orderHash :: ByteString        -- ^ Unique identifier for this order (32 bytes)
    , hashlock :: ByteString         -- ^ Hash of the secret that unlocks the escrow (32 bytes)
    , maker :: ByteString            -- ^ Address of the order maker (seller)
    , taker :: ByteString            -- ^ Address of the order taker (buyer)
    , amount :: Integer              -- ^ Amount of ADA to be escrowed (in lovelace)
    , safetyDeposit :: Integer       -- ^ Additional safety deposit amount (in lovelace)
    , timelocks :: Timelocks         -- ^ Timelock configuration for this escrow
    } deriving (Show, Eq, Generic)

-- | Resolver registry to track authorized resolvers
-- Maps resolver addresses to their authorization status
-- This replaces the access token mechanism from Solidity
data ResolverRegistry = ResolverRegistry
    { registryOwner :: ByteString           -- ^ Owner who can add/remove resolvers
    , authorizedResolvers :: Map ByteString Bool -- ^ Map of resolver addresses to authorization
    } deriving (Show, Eq, Generic)

-- | Current state of an escrow contract
-- Contains all the mutable state and configuration
data EscrowState = EscrowState
    { immutables :: Immutables          -- ^ Unchanging escrow parameters
    , factory :: ByteString             -- ^ Address of the factory that created this escrow
    , rescueDelay :: Integer            -- ^ Delay in seconds before rescue operations are allowed
    , resolverRegistry :: ResolverRegistry -- ^ Registry of authorized resolvers
    , isActive :: Bool                  -- ^ Whether the escrow is currently active
    , currentBalance :: Integer         -- ^ Current ADA balance in the escrow (in lovelace)
    } deriving (Show, Eq, Generic)

-- | Actions that can be performed on an escrow
-- Defines the possible operations, similar to Solidity function calls
data EscrowAction
    = Withdraw 
        { secret :: ByteString          -- ^ Secret to unlock the funds
        , caller :: ByteString          -- ^ Address performing the withdrawal
        }
    | Cancel 
        { caller :: ByteString          -- ^ Address performing the cancellation
        }
    | PublicWithdraw 
        { secret :: ByteString          -- ^ Secret to unlock the funds
        , resolver :: ByteString        -- ^ Resolver performing the action
        }
    | PublicCancel 
        { resolver :: ByteString        -- ^ Resolver performing the action
        }
    | RescueFunds 
        { rescueAmount :: Integer       -- ^ Amount to rescue
        , caller :: ByteString          -- ^ Address performing the rescue
        }
    | AddResolver 
        { newResolver :: ByteString     -- ^ New resolver to add
        , caller :: ByteString          -- ^ Address adding the resolver (must be owner)
        }
    | RemoveResolver 
        { resolverToRemove :: ByteString -- ^ Resolver to remove
        , caller :: ByteString          -- ^ Address removing the resolver (must be owner)
        }
    deriving (Show, Eq, Generic)

-- | Create a new Immutables structure with validation
-- 
-- Parameters:
-- - orderHash: Unique order identifier (must be 32 bytes)
-- - secret: The secret that will unlock the escrow
-- - makerAddr: Maker's address
-- - takerAddr: Taker's address  
-- - escrowAmount: Amount to escrow (in lovelace)
-- - deposit: Safety deposit amount (in lovelace)
-- - timeLocks: Timelock configuration
--
-- Returns: Either an error or valid Immutables
mkImmutables :: ByteString -> ByteString -> ByteString -> ByteString 
             -> Integer -> Integer -> Timelocks 
             -> Either BaseEscrowError Immutables
mkImmutables orderHash secret makerAddr takerAddr escrowAmount deposit timeLocks
    | BS.length orderHash /= 32 = Left InvalidImmutables
    | BS.null makerAddr = Left InvalidImmutables  
    | BS.null takerAddr = Left InvalidImmutables
    | escrowAmount <= 0 = Left InvalidAmount
    | deposit < 0 = Left InvalidAmount
    | otherwise = Right $ Immutables
        { orderHash = orderHash
        , hashlock = hashSecret secret
        , maker = makerAddr
        , taker = takerAddr
        , amount = escrowAmount
        , safetyDeposit = deposit
        , timelocks = timeLocks
        }

-- | Main validation function for escrow actions
-- This is the core function that validates all operations on the escrow
-- Similar to the main validator in the Solidity contract
--
-- Parameters:
-- - state: Current state of the escrow
-- - action: Action being performed
-- - currentTime: Current timestamp
--
-- Returns: Either an error or the updated state after the action
validateEscrowAction :: EscrowState -> EscrowAction -> Integer 
                     -> Either BaseEscrowError EscrowState
validateEscrowAction state action currentTime = do
    -- First, validate that the escrow is active (for most operations)
    case action of
        AddResolver {} -> return ()  -- Registry operations don't require active escrow
        RemoveResolver {} -> return ()
        _ -> if not (isActive state) 
             then Left EscrowNotActive 
             else return ()
    
    -- Validate the action and return updated state
    case action of
        Withdraw secret caller -> do
            validateTaker caller (immutables state)
            validateSecret secret (immutables state)
            validateWithdrawTime currentTime (immutables state)
            validateImmutables (immutables state)
            let transferAmount = amount (immutables state)
            validateSufficientFunds state transferAmount
            return $ state 
                { currentBalance = currentBalance state - transferAmount
                , isActive = False  -- Escrow is completed
                }
                
        Cancel caller -> do
            validateTaker caller (immutables state)
            validateCancelTime currentTime (immutables state)
            validateImmutables (immutables state)
            let refundAmount = totalAmount (immutables state)
            validateSufficientFunds state refundAmount
            return $ state 
                { currentBalance = currentBalance state - refundAmount
                , isActive = False  -- Escrow is cancelled
                }
                
        PublicWithdraw secret resolver -> do
            validateResolver resolver (resolverRegistry state)
            validateSecret secret (immutables state)
            validatePublicWithdrawTime currentTime (immutables state)
            validateImmutables (immutables state)
            let transferAmount = amount (immutables state)
            validateSufficientFunds state transferAmount
            return $ state 
                { currentBalance = currentBalance state - transferAmount
                , isActive = False  -- Escrow is completed
                }
                
        PublicCancel resolver -> do
            validateResolver resolver (resolverRegistry state)
            validatePublicCancelTime currentTime (immutables state)
            validateImmutables (immutables state)
            let refundAmount = totalAmount (immutables state)
            validateSufficientFunds state refundAmount
            return $ state 
                { currentBalance = currentBalance state - refundAmount
                , isActive = False  -- Escrow is cancelled
                }
                
        RescueFunds rescueAmount caller -> do
            validateTaker caller (immutables state)
            validateRescueTime currentTime state
            validateImmutables (immutables state)
            if rescueAmount <= 0 
                then Left InvalidAmount
                else do
                    validateSufficientFunds state rescueAmount
                    return $ state { currentBalance = currentBalance state - rescueAmount }
                    
        AddResolver newResolver caller -> do
            validateRegistryOwner caller (resolverRegistry state)
            let updatedRegistry = (resolverRegistry state)
                    { authorizedResolvers = Map.insert newResolver True 
                                          (authorizedResolvers $ resolverRegistry state) }
            return $ state { resolverRegistry = updatedRegistry }
            
        RemoveResolver resolverToRemove caller -> do
            validateRegistryOwner caller (resolverRegistry state)
            let updatedRegistry = (resolverRegistry state)
                    { authorizedResolvers = Map.delete resolverToRemove 
                                          (authorizedResolvers $ resolverRegistry state) }
            return $ state { resolverRegistry = updatedRegistry }
  where
    -- Total amount includes both the main amount and safety deposit
    totalAmount immuts = amount immuts + safetyDeposit immuts

-- | Validate that the caller is the taker
-- Equivalent to the onlyTaker modifier in Solidity
validateTaker :: ByteString -> Immutables -> Either BaseEscrowError ()
validateTaker caller immuts
    | caller == taker immuts = Right ()
    | otherwise = Left InvalidCaller

-- | Validate that the caller is the maker  
validateMaker :: ByteString -> Immutables -> Either BaseEscrowError ()
validateMaker caller immuts
    | caller == maker immuts = Right ()
    | otherwise = Left InvalidCaller

-- | Validate that the provided secret matches the hashlock
-- Equivalent to the onlyValidSecret modifier in Solidity
validateSecret :: ByteString -> Immutables -> Either BaseEscrowError ()
validateSecret secret immuts
    | hashSecret secret == hashlock immuts = Right ()
    | otherwise = Left InvalidSecret

-- | Validate that the caller is an authorized resolver
-- Replaces the onlyAccessTokenHolder modifier from Solidity
validateResolver :: ByteString -> ResolverRegistry -> Either BaseEscrowError ()
validateResolver resolver registry = 
    case Map.lookup resolver (authorizedResolvers registry) of
        Just True -> Right ()
        _ -> Left ResolverNotAuthorized

-- | Validate that the caller is the registry owner
validateRegistryOwner :: ByteString -> ResolverRegistry -> Either BaseEscrowError ()
validateRegistryOwner caller registry
    | caller == registryOwner registry = Right ()
    | otherwise = Left InvalidCaller

-- | Validate the immutables structure
-- Basic validation of the immutables parameters
validateImmutables :: Immutables -> Either BaseEscrowError ()
validateImmutables immuts
    | amount immuts <= 0 = Left InvalidAmount
    | safetyDeposit immuts < 0 = Left InvalidAmount
    | BS.length (orderHash immuts) /= 32 = Left InvalidImmutables
    | BS.length (hashlock immuts) /= 32 = Left InvalidImmutables
    | BS.null (maker immuts) = Left InvalidImmutables
    | BS.null (taker immuts) = Left InvalidImmutables
    | otherwise = Right ()

-- | Validate sufficient funds for a transfer
validateSufficientFunds :: EscrowState -> Integer -> Either BaseEscrowError ()
validateSufficientFunds state transferAmount
    | currentBalance state >= transferAmount = Right ()
    | otherwise = Left InsufficientFunds

-- | Validate timing for regular withdrawal (taker only)
validateWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateWithdrawTime currentTime immuts = 
    let withdrawTime = getTimelock (timelocks immuts) SrcWithdrawal
        publicWithdrawTime = getTimelock (timelocks immuts) SrcPublicWithdrawal
    in if currentTime >= withdrawTime && currentTime < publicWithdrawTime
       then Right ()
       else Left InvalidTime

-- | Validate timing for public withdrawal (resolver)
validatePublicWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validatePublicWithdrawTime currentTime immuts = 
    let publicWithdrawTime = getTimelock (timelocks immuts) SrcPublicWithdrawal
    in if currentTime >= publicWithdrawTime
       then Right ()
       else Left InvalidTime

-- | Validate timing for regular cancellation (taker only)
validateCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateCancelTime currentTime immuts = 
    let cancelTime = getTimelock (timelocks immuts) SrcCancellation
        publicCancelTime = getTimelock (timelocks immuts) SrcPublicCancellation
    in if currentTime >= cancelTime && currentTime < publicCancelTime
       then Right ()
       else Left InvalidTime

-- | Validate timing for public cancellation (resolver)
validatePublicCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validatePublicCancelTime currentTime immuts = 
    let publicCancelTime = getTimelock (timelocks immuts) SrcPublicCancellation
    in if currentTime >= publicCancelTime
       then Right ()
       else Left InvalidTime

-- | Validate timing for rescue operations
validateRescueTime :: Integer -> EscrowState -> Either BaseEscrowError ()
validateRescueTime currentTime state = 
    let rescueTime = rescueStart (timelocks $ immutables state) (rescueDelay state)
    in if currentTime >= rescueTime
       then Right ()
       else Left InvalidTime

-- | Validate time constraints for any stage
-- General helper function for time validation
validateTimeConstraints :: Integer -> Timelocks -> Stage -> Either BaseEscrowError ()
validateTimeConstraints currentTime timeLocks stage = 
    if isStageActive timeLocks stage currentTime
    then Right ()
    else Left InvalidTime

-- | Transfer ADA to a recipient address
-- Equivalent to _uniTransfer/_ethTransfer in Solidity but ADA-only
-- In a real implementation, this would interface with Cardano's transaction building
--
-- Parameters:
-- - amount: Amount to transfer (in lovelace)
-- - recipient: Address to send ADA to
--
-- Returns: Either an error or success
transferAda :: Integer -> ByteString -> Either BaseEscrowError ()
transferAda amount recipient
    | amount <= 0 = Left InvalidAmount
    | BS.null recipient = Left InvalidCaller
    | otherwise = Right ()  -- In real implementation, would build transaction output

-- | Emergency rescue function for stuck funds
-- Equivalent to rescueFunds in Solidity
--
-- Parameters:
-- - state: Current escrow state
-- - amount: Amount to rescue
-- - currentTime: Current timestamp
--
-- Returns: Either an error or success
rescueFunds :: EscrowState -> Integer -> Integer -> Either BaseEscrowError ()
rescueFunds state amount currentTime = do
    validateRescueTime currentTime state
    validateSufficientFunds state amount
    transferAda amount (taker $ immutables state)

-- | Compute the hash of a secret
-- Uses Blake2b_256 instead of keccak256 since that's what Cardano uses
-- Equivalent to _keccakBytes32 in Solidity
--
-- Parameters:
-- - secret: The secret to hash
--
-- Returns: Blake2b_256 hash of the secret
hashSecret :: ByteString -> ByteString
hashSecret secret = 
    let digest = hashWith Blake2b_256 secret :: Digest Blake2b_256
    in BC.pack $ show digest  -- Convert hash to ByteString representation
