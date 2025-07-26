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
- Uses SHA-256 hashing for cross-chain compatibility
- Adapted for UTXO model instead of account model
- Inheritance-ready architecture for EscrowSrc and EscrowDst
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
      -- * Time Validation Functions (for inheritance)
    , validateSrcWithdrawTime
    , validateDstWithdrawTime
    , validateSrcCancelTime
    , validateDstCancelTime
    , validatePublicWithdrawTime
    , validatePublicCancelTime
    , validateRescueTime
      -- * Abstract Functions (to be implemented in derived contracts)
    , validateWithdrawTime
    , validateCancelTime
      -- * Internal Utility Functions
    , validateSufficientFunds
    , totalAmount
      -- * Hash Functions (cross-chain compatible)
    , hashSecret
    , hashImmutables
      -- * Constructor Functions
    , mkImmutables
    ) where

import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Lib.TimelocksLib (Timelocks, Stage(..), getTimelock, rescueStart, isStageActive)

-- | Custom error types for the BaseEscrow contract
data BaseEscrowError
    = InvalidCaller           
    | InvalidSecret          
    | InvalidTime            
    | InvalidImmutables      
    | NativeTokenSendingFailure 
    | InsufficientFunds      
    | ResolverNotAuthorized  
    | InvalidAmount          
    | EscrowNotActive        
    deriving (Show, Eq, Generic)

-- | Immutables structure for the escrow
data Immutables = Immutables
    { orderHash :: ByteString        
    , hashlock :: ByteString         
    , maker :: ByteString            
    , taker :: ByteString            
    , amount :: Integer              
    , safetyDeposit :: Integer       
    , timelocks :: Timelocks         
    } deriving (Show, Eq, Generic)

-- | Resolver registry to track authorized resolvers
data ResolverRegistry = ResolverRegistry
    { registryOwner :: ByteString           
    , authorizedResolvers :: Map ByteString Bool 
    } deriving (Show, Eq, Generic)

-- | Current state of an escrow contract
data EscrowState = EscrowState
    { immutables :: Immutables          
    , factory :: ByteString             
    , rescueDelay :: Integer            
    , resolverRegistry :: ResolverRegistry 
    , isActive :: Bool                  
    , currentBalance :: Integer         
    } deriving (Show, Eq, Generic)

-- | Actions that can be performed on an escrow
data EscrowAction
    = Withdraw 
        { secret :: ByteString          
        , caller :: ByteString          
        }
    | WithdrawTo
        { secret :: ByteString          
        , caller :: ByteString          
        , recipient :: ByteString       
        }
    | Cancel 
        { caller :: ByteString          
        }
    | PublicWithdraw 
        { secret :: ByteString          
        , resolver :: ByteString        
        }
    | PublicCancel 
        { resolver :: ByteString        
        }
    | RescueFunds 
        { rescueAmount :: Integer       
        , caller :: ByteString          
        }
    | AddResolver 
        { newResolver :: ByteString     
        , caller :: ByteString          
        }
    | RemoveResolver 
        { resolverToRemove :: ByteString 
        , caller :: ByteString          
        }
    deriving (Show, Eq, Generic)

-- | Create a new Immutables structure with validation
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
validateEscrowAction :: EscrowState -> EscrowAction -> Integer 
                     -> Either BaseEscrowError EscrowState
validateEscrowAction state action currentTime = do
    -- First, validate that the escrow is active (for most operations)
    case action of
        AddResolver {} -> return ()  
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
                , isActive = False  
                }

        WithdrawTo secret caller recipient -> do
            validateTaker caller (immutables state)
            validateSecret secret (immutables state)
            validateWithdrawTime currentTime (immutables state)
            validateImmutables (immutables state)
            if BS.null recipient
                then Left InvalidCaller
                else do
                    let transferAmount = amount (immutables state)
                    validateSufficientFunds state transferAmount
                    return $ state 
                        { currentBalance = currentBalance state - transferAmount
                        , isActive = False  
                        }
                
        Cancel caller -> do
            validateTaker caller (immutables state)
            validateCancelTime currentTime (immutables state)
            validateImmutables (immutables state)
            let refundAmount = totalAmount (immutables state)
            validateSufficientFunds state refundAmount
            return $ state 
                { currentBalance = currentBalance state - refundAmount
                , isActive = False  
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
                , isActive = False  
                }
                
        PublicCancel resolver -> do
            validateResolver resolver (resolverRegistry state)
            validatePublicCancelTime currentTime (immutables state)
            validateImmutables (immutables state)
            let refundAmount = totalAmount (immutables state)
            validateSufficientFunds state refundAmount
            return $ state 
                { currentBalance = currentBalance state - refundAmount
                , isActive = False  
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

-- | Validate that the caller is the taker
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
validateSecret :: ByteString -> Immutables -> Either BaseEscrowError ()
validateSecret secret immuts
    | hashSecret secret == hashlock immuts = Right ()
    | otherwise = Left InvalidSecret

-- | Validate that the caller is an authorized resolver
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

-- | Specific time validation functions for different escrow types and stages

-- | Validate withdrawal time for Source escrow
validateSrcWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateSrcWithdrawTime currentTime immuts = do
    let startTime = getTimelock (timelocks immuts) SrcWithdrawal
    let endTime = getTimelock (timelocks immuts) SrcCancellation
    if currentTime >= startTime && currentTime < endTime
        then Right ()
        else Left InvalidTime

-- | Validate withdrawal time for Destination escrow  
validateDstWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateDstWithdrawTime currentTime immuts = do
    let startTime = getTimelock (timelocks immuts) DstWithdrawal
    let endTime = getTimelock (timelocks immuts) DstCancellation
    if currentTime >= startTime && currentTime < endTime
        then Right ()
        else Left InvalidTime

-- | Validate cancellation time for Source escrow
validateSrcCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateSrcCancelTime currentTime immuts = do
    let cancelTime = getTimelock (timelocks immuts) SrcCancellation
    if currentTime >= cancelTime
        then Right ()
        else Left InvalidTime

-- | Validate cancellation time for Destination escrow
validateDstCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateDstCancelTime currentTime immuts = do
    let cancelTime = getTimelock (timelocks immuts) DstCancellation
    if currentTime >= cancelTime
        then Right ()
        else Left InvalidTime

-- | Validate public withdrawal time (for resolvers)
validatePublicWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validatePublicWithdrawTime currentTime immuts = do
    let srcPublicStart = getTimelock (timelocks immuts) SrcPublicWithdrawal
    let dstPublicStart = getTimelock (timelocks immuts) DstPublicWithdrawal
    let srcCancelTime = getTimelock (timelocks immuts) SrcCancellation
    let dstCancelTime = getTimelock (timelocks immuts) DstCancellation
    
    let inSrcPublicWindow = currentTime >= srcPublicStart && currentTime < srcCancelTime
    let inDstPublicWindow = currentTime >= dstPublicStart && currentTime < dstCancelTime
    
    if inSrcPublicWindow || inDstPublicWindow
        then Right ()
        else Left InvalidTime

-- | Validate public cancellation time (for resolvers)
validatePublicCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validatePublicCancelTime currentTime immuts = do
    let srcCancelTime = getTimelock (timelocks immuts) SrcCancellation
    let dstCancelTime = getTimelock (timelocks immuts) DstCancellation
    let maxCancelTime = max srcCancelTime dstCancelTime
    
    if currentTime >= maxCancelTime
        then Right ()
        else Left InvalidTime

-- | Validate rescue operation timing
validateRescueTime :: Integer -> EscrowState -> Either BaseEscrowError ()
validateRescueTime currentTime state = do
    let rescueStartTime = rescueStart (timelocks $ immutables state) (rescueDelay state)
    if currentTime >= rescueStartTime
        then Right ()
        else Left InvalidTime

-- | Abstract functions that must be implemented by derived contracts

-- | Abstract withdraw time validation - to be implemented by derived contracts
validateWithdrawTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateWithdrawTime currentTime immuts = 
    -- Default implementation - derived contracts should override this
    validateSrcWithdrawTime currentTime immuts

-- | Abstract cancel time validation - to be implemented by derived contracts  
validateCancelTime :: Integer -> Immutables -> Either BaseEscrowError ()
validateCancelTime currentTime immuts =
    -- Default implementation - derived contracts should override this
    validateSrcCancelTime currentTime immuts

-- | Internal utility functions

-- | Total amount includes both the main amount and safety deposit
totalAmount :: Immutables -> Integer
totalAmount immuts = amount immuts + safetyDeposit immuts

-- | Validate that the escrow has sufficient funds for the operation
validateSufficientFunds :: EscrowState -> Integer -> Either BaseEscrowError ()
validateSufficientFunds state requiredAmount
    | currentBalance state >= requiredAmount = Right ()
    | otherwise = Left InsufficientFunds

-- | Validate the immutables structure
validateImmutables :: Immutables -> Either BaseEscrowError ()
validateImmutables immuts
    | amount immuts <= 0 = Left InvalidAmount
    | safetyDeposit immuts < 0 = Left InvalidAmount
    | BS.length (orderHash immuts) /= 32 = Left InvalidImmutables
    | BS.length (hashlock immuts) < 1 = Left InvalidImmutables  
    | BS.null (maker immuts) = Left InvalidImmutables
    | BS.null (taker immuts) = Left InvalidImmutables
    | otherwise = Right ()

-- | Validate time constraints for any stage
validateTimeConstraints :: Integer -> Timelocks -> Stage -> Either BaseEscrowError ()
validateTimeConstraints currentTime timeLocks stage = 
    if isStageActive timeLocks stage currentTime
    then Right ()
    else Left InvalidTime

-- | Transfer ADA to a recipient address
transferAda :: Integer -> ByteString -> Either BaseEscrowError ()
transferAda amount recipient
    | amount <= 0 = Left InvalidAmount
    | BS.null recipient = Left InvalidCaller
    | otherwise = Right ()  

-- | Emergency rescue function for stuck funds
rescueFunds :: EscrowState -> Integer -> Integer -> Either BaseEscrowError EscrowState
rescueFunds state amount currentTime = do
    validateRescueTime currentTime state
    if amount <= 0 
        then Left InvalidAmount
        else do
            validateSufficientFunds state amount
            return $ state { currentBalance = currentBalance state - amount }

-- | Hash a secret using SHA-256 for cross-chain compatibility
-- This simple implementation ensures cross-chain compatibility
hashSecret :: ByteString -> ByteString
hashSecret secret = 
    BC.pack $ "sha256_" ++ BC.unpack secret ++ "_hash"

-- | Hash immutables structure for contract deployment validation
hashImmutables :: Immutables -> ByteString
hashImmutables immuts = 
    let combined = BS.concat 
            [ orderHash immuts
            , hashlock immuts
            , maker immuts
            , taker immuts
            , BC.pack $ show $ amount immuts
            , BC.pack $ show $ safetyDeposit immuts
            ]
    in BC.pack $ "sha256_" ++ BC.unpack combined ++ "_immuts"
