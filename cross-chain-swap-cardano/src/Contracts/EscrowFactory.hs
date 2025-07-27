{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Contracts.EscrowFactory
Description : Factory contract for creating and managing escrow contracts
Copyright   : (c) 2025
License     : MIT

This module provides the factory functionality for creating and managing both
source and destination escrow contracts for cross-chain atomic swaps.

Key functionality:
- Deploy new EscrowSrc and EscrowDst contracts for each swap
- Compute deterministic addresses for pre-funding validation
- Manage factory state and statistics
- Validate safety deposits and timing constraints
- Integrate with resolver registry for dispute resolution
- Owner permissions and access control
-}

module Contracts.EscrowFactory
    ( -- * Types
      EscrowFactoryState(..)
    , EscrowFactoryAction(..)
    , EscrowRecord(..)
    , FactoryStatistics(..)
    , FactoryError(..)
      -- * Core Functions
    , createEscrowFactory
    , validateFactoryAction
      -- * Escrow Creation Functions
    , createSrcEscrow
    , createDstEscrow
      -- * Address Computation Functions
    , addressOfEscrowSrc
    , addressOfEscrowDst
      -- * Management Functions
    , getEscrowRecord
    , getFactoryStatistics
    , transferFactoryOwnership
      -- * Constructor
    , mkEscrowFactory
      -- * Display Functions
    , displayEscrowFactory
    ) where

import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (POSIXTime)

-- Import our existing contracts
import Contracts.BaseEscrow
import qualified Contracts.EscrowSrc as Src
import qualified Contracts.EscrowDst as Dst
import Lib.TimelocksLib

-- | Errors specific to factory operations
data FactoryError
    = InvalidFactoryOwner
    | InsufficientEscrowBalance
    | InvalidCreationTime
    | EscrowAlreadyExists
    | EscrowNotFound
    | InvalidSrcCancellationTime
    | FactoryNotInitialized
    | UnauthorizedFactoryAccess
    deriving (Show, Eq, Generic)

-- | Record of a created escrow contract
data EscrowRecord = EscrowRecord
    { escrowAddress :: ByteString           -- Computed deterministic address
    , escrowType :: EscrowType             -- Source or Destination
    , immutables :: Immutables             -- Escrow immutables
    , createdAt :: Integer                 -- Creation timestamp
    , creator :: ByteString                -- Address that created the escrow
    , isActive :: Bool                     -- Whether escrow is still active
    , totalBalance :: Integer              -- Total balance (amount + safety deposit)
    } deriving (Show, Eq, Generic)

-- | Type of escrow contract
data EscrowType = SourceEscrow | DestinationEscrow
    deriving (Show, Eq, Generic)

-- | Factory statistics
data FactoryStatistics = FactoryStatistics
    { totalEscrowsCreated :: Integer       -- Total number of escrows created
    , activeEscrowsCount :: Integer        -- Number of active escrows
    , completedEscrowsCount :: Integer     -- Number of completed escrows
    , totalVolumeProcessed :: Integer      -- Total ADA volume processed
    , srcEscrowsCreated :: Integer         -- Source escrows created
    , dstEscrowsCreated :: Integer         -- Destination escrows created
    } deriving (Show, Eq, Generic)

-- | Factory state
data EscrowFactoryState = EscrowFactoryState
    { factoryOwner :: ByteString           -- Factory owner address
    , rescueDelaySrc :: Integer            -- Rescue delay for source escrows
    , rescueDelayDst :: Integer            -- Rescue delay for destination escrows
    , resolverRegistry :: ResolverRegistry -- Resolver registry for dispute resolution
    , escrowRegistry :: Map.Map ByteString EscrowRecord  -- Registry of created escrows
    , statistics :: FactoryStatistics      -- Factory statistics
    , isInitialized :: Bool                -- Whether factory is properly initialized
    , currentNonce :: Integer              -- Nonce for deterministic address generation
    } deriving (Show, Eq, Generic)

-- | Actions that can be performed on the factory
data EscrowFactoryAction
    = CreateSrcEscrowAction
        { orderHash :: ByteString          -- Order hash for the swap
        , secret :: ByteString             -- Secret for the hashlock
        , maker :: ByteString              -- Maker address
        , taker :: ByteString              -- Taker address
        , amount :: Integer                -- Main swap amount
        , safetyDeposit :: Integer         -- Safety deposit amount
        , timelocks :: Timelocks           -- Timing constraints
        , caller :: ByteString             -- Transaction caller
        }
    | CreateDstEscrowAction
        { dstImmutables :: Immutables      -- Destination escrow immutables
        , srcCancellationTimestamp :: Integer  -- Source cancellation timestamp
        , providedBalance :: Integer       -- Balance provided by caller
        , caller :: ByteString             -- Transaction caller
        }
    | TransferOwnershipAction
        { newOwner :: ByteString           -- New factory owner
        , caller :: ByteString             -- Transaction caller
        }
    | UpdateResolverRegistryAction
        { newRegistry :: ResolverRegistry  -- Updated resolver registry
        , caller :: ByteString             -- Transaction caller
        }
    deriving (Show, Eq, Generic)

-- | Create a new factory state
createEscrowFactory :: ByteString -> Integer -> Integer -> ResolverRegistry 
                   -> Either FactoryError EscrowFactoryState
createEscrowFactory owner srcDelay dstDelay registry = do
    -- Validate inputs
    if BS.null owner
        then Left InvalidFactoryOwner
        else if srcDelay < 0 || dstDelay < 0
            then Left InvalidCreationTime
            else do
                let initialStats = FactoryStatistics
                        { totalEscrowsCreated = 0
                        , activeEscrowsCount = 0
                        , completedEscrowsCount = 0
                        , totalVolumeProcessed = 0
                        , srcEscrowsCreated = 0
                        , dstEscrowsCreated = 0
                        }
                
                return $ EscrowFactoryState
                    { factoryOwner = owner
                    , rescueDelaySrc = srcDelay
                    , rescueDelayDst = dstDelay
                    , resolverRegistry = registry
                    , escrowRegistry = Map.empty
                    , statistics = initialStats
                    , isInitialized = True
                    , currentNonce = 0
                    }

-- | Main validation function for factory actions
validateFactoryAction :: EscrowFactoryState -> EscrowFactoryAction -> Integer
                      -> Either FactoryError (EscrowFactoryState, ByteString)
validateFactoryAction factory action currentTime = do
    if not (isInitialized factory)
        then Left FactoryNotInitialized
        else case action of
            CreateSrcEscrowAction{..} ->
                createSrcEscrow factory orderHash secret maker taker amount 
                               safetyDeposit timelocks caller currentTime
            
            CreateDstEscrowAction{..} ->
                createDstEscrow factory dstImmutables srcCancellationTimestamp 
                               providedBalance caller currentTime
            
            TransferOwnershipAction{..} ->
                transferFactoryOwnership factory newOwner caller
            
            UpdateResolverRegistryAction{..} ->
                updateResolverRegistry factory newRegistry caller

-- | Create a source escrow contract
createSrcEscrow :: EscrowFactoryState -> ByteString -> ByteString -> ByteString 
                -> ByteString -> Integer -> Integer -> Timelocks -> ByteString 
                -> Integer -> Either FactoryError (EscrowFactoryState, ByteString)
createSrcEscrow factory orderHash secret maker taker amount safetyDeposit 
               timelocks caller currentTime = do
    -- Validate order hash length (must be 32 bytes)
    if BS.length orderHash /= 32
        then Left (InvalidCreationTime)
        else do
            -- Create immutables
            let hashlock = hashSecret secret
            let deployedTimelocks = setDeployedAt timelocks currentTime
            let immuts = Immutables
                    { orderHash = orderHash
                    , hashlock = hashlock
                    , maker = maker
                    , taker = taker
                    , amount = amount
                    , safetyDeposit = safetyDeposit
                    , timelocks = deployedTimelocks
                    }
            
            -- Validate immutables
            case validateImmutables immuts of
                Left _ -> Left InvalidCreationTime
                Right _ -> do
                    -- Compute deterministic address
                    let escrowAddress = computeEscrowSrcAddress factory immuts
                    
                    -- Check if escrow already exists
                    if Map.member escrowAddress (escrowRegistry factory)
                        then Left EscrowAlreadyExists
                        else do
                            -- Create escrow state
                            srcResult <- case Src.mkEscrowSrc orderHash secret maker taker 
                                             amount safetyDeposit (rescueDelaySrc factory) 
                                             (resolverRegistry factory) of
                                Left _ -> Left InvalidCreationTime
                                Right _ -> Right ()
                            
                            -- Create escrow record
                            let totalBalance = amount + safetyDeposit
                            let escrowRecord = EscrowRecord
                                    { escrowAddress = escrowAddress
                                    , escrowType = SourceEscrow
                                    , immutables = immuts
                                    , createdAt = currentTime
                                    , creator = caller
                                    , isActive = True
                                    , totalBalance = totalBalance
                                    }
                            
                            -- Update factory state
                            let updatedRegistry = Map.insert escrowAddress escrowRecord (escrowRegistry factory)
                            let updatedStats = (statistics factory)
                                    { totalEscrowsCreated = totalEscrowsCreated (statistics factory) + 1
                                    , activeEscrowsCount = activeEscrowsCount (statistics factory) + 1
                                    , totalVolumeProcessed = totalVolumeProcessed (statistics factory) + totalBalance
                                    , srcEscrowsCreated = srcEscrowsCreated (statistics factory) + 1
                                    }
                            
                            let updatedFactory = factory
                                    { escrowRegistry = updatedRegistry
                                    , statistics = updatedStats
                                    , currentNonce = currentNonce factory + 1
                                    }
                            
                            return (updatedFactory, escrowAddress)

-- | Create a destination escrow contract
createDstEscrow :: EscrowFactoryState -> Immutables -> Integer -> Integer 
                -> ByteString -> Integer 
                -> Either FactoryError (EscrowFactoryState, ByteString)
createDstEscrow factory dstImmutables srcCancellationTimestamp providedBalance 
               caller currentTime = do
    -- Set deployment time for timelocks
    let deployedTimelocks = setDeployedAt (timelocks dstImmutables) currentTime
    let immuts = dstImmutables { timelocks = deployedTimelocks }
    
    -- Validate that destination cancellation doesn't happen after source cancellation
    let dstCancelTime = getTimelock (timelocks immuts) DstCancellation
    if dstCancelTime > srcCancellationTimestamp
        then Left InvalidSrcCancellationTime
        else do
            -- Validate immutables
            case validateImmutables immuts of
                Left _ -> Left InvalidCreationTime
                Right _ -> do
                    -- Compute deterministic address
                    let escrowAddress = computeEscrowDstAddress factory immuts
                    
                    -- Check if escrow already exists
                    if Map.member escrowAddress (escrowRegistry factory)
                        then Left EscrowAlreadyExists
                        else do
                            -- Validate provided balance
                            let requiredBalance = amount immuts + safetyDeposit immuts
                            if providedBalance < requiredBalance
                                then Left InsufficientEscrowBalance
                                else do
                                    -- Create escrow state
                                    dstResult <- case Dst.mkEscrowDst (orderHash immuts) (BC.pack "temp_secret") 
                                                     (maker immuts) (taker immuts) (amount immuts) 
                                                     (safetyDeposit immuts) (rescueDelayDst factory) 
                                                     (resolverRegistry factory) of
                                        Left _ -> Left InvalidCreationTime
                                        Right _ -> Right ()
                                    
                                    -- Create escrow record
                                    let escrowRecord = EscrowRecord
                                            { escrowAddress = escrowAddress
                                            , escrowType = DestinationEscrow
                                            , immutables = immuts
                                            , createdAt = currentTime
                                            , creator = caller
                                            , isActive = True
                                            , totalBalance = requiredBalance
                                            }
                                    
                                    -- Update factory state
                                    let updatedRegistry = Map.insert escrowAddress escrowRecord (escrowRegistry factory)
                                    let updatedStats = (statistics factory)
                                            { totalEscrowsCreated = totalEscrowsCreated (statistics factory) + 1
                                            , activeEscrowsCount = activeEscrowsCount (statistics factory) + 1
                                            , totalVolumeProcessed = totalVolumeProcessed (statistics factory) + requiredBalance
                                            , dstEscrowsCreated = dstEscrowsCreated (statistics factory) + 1
                                            }
                                    
                                    let updatedFactory = factory
                                            { escrowRegistry = updatedRegistry
                                            , statistics = updatedStats
                                            , currentNonce = currentNonce factory + 1
                                            }
                                    
                                    return (updatedFactory, escrowAddress)

-- | Compute deterministic address for source escrow
computeEscrowSrcAddress :: EscrowFactoryState -> Immutables -> ByteString
computeEscrowSrcAddress factory immuts =
    let factorySalt = BC.pack $ "factory_" ++ show (currentNonce factory)
        immutablesHash = hashImmutables immuts
        combinedHash = BC.pack $ BC.unpack factorySalt ++ BC.unpack immutablesHash ++ "_src"
    in BC.pack $ "src_escrow_" ++ take 32 (BC.unpack combinedHash ++ repeat '0')

-- | Compute deterministic address for destination escrow  
computeEscrowDstAddress :: EscrowFactoryState -> Immutables -> ByteString
computeEscrowDstAddress factory immuts =
    let factorySalt = BC.pack $ "factory_" ++ show (currentNonce factory)
        immutablesHash = hashImmutables immuts
        combinedHash = BC.pack $ BC.unpack factorySalt ++ BC.unpack immutablesHash ++ "_dst"
    in BC.pack $ "dst_escrow_" ++ take 32 (BC.unpack combinedHash ++ repeat '0')

-- | Get address of source escrow (external view function)
addressOfEscrowSrc :: EscrowFactoryState -> Immutables -> ByteString
addressOfEscrowSrc factory immuts = computeEscrowSrcAddress factory immuts

-- | Get address of destination escrow (external view function)
addressOfEscrowDst :: EscrowFactoryState -> Immutables -> ByteString  
addressOfEscrowDst factory immuts = computeEscrowDstAddress factory immuts

-- | Get escrow record by address
getEscrowRecord :: EscrowFactoryState -> ByteString -> Maybe EscrowRecord
getEscrowRecord factory escrowAddress = Map.lookup escrowAddress (escrowRegistry factory)

-- | Get factory statistics
getFactoryStatistics :: EscrowFactoryState -> FactoryStatistics
getFactoryStatistics factory = statistics factory

-- | Transfer factory ownership
transferFactoryOwnership :: EscrowFactoryState -> ByteString -> ByteString 
                         -> Either FactoryError (EscrowFactoryState, ByteString)
transferFactoryOwnership factory newOwner caller = do
    if caller /= factoryOwner factory
        then Left UnauthorizedFactoryAccess
        else if BS.null newOwner
            then Left InvalidFactoryOwner
            else do
                let updatedFactory = factory { factoryOwner = newOwner }
                return (updatedFactory, BC.pack "ownership_transferred")

-- | Update resolver registry
updateResolverRegistry :: EscrowFactoryState -> ResolverRegistry -> ByteString
                       -> Either FactoryError (EscrowFactoryState, ByteString)
updateResolverRegistry factory newRegistry caller = do
    if caller /= factoryOwner factory
        then Left UnauthorizedFactoryAccess
        else do
            let updatedFactory = factory { resolverRegistry = newRegistry }
            return (updatedFactory, BC.pack "registry_updated")

-- | Constructor helper function
mkEscrowFactory :: ByteString -> Integer -> Integer -> ResolverRegistry
                -> Either FactoryError EscrowFactoryState
mkEscrowFactory owner srcDelay dstDelay registry = 
    createEscrowFactory owner srcDelay dstDelay registry

-- | Display factory state for debugging
displayEscrowFactory :: EscrowFactoryState -> String
displayEscrowFactory factory = unlines
    [ "🏭 EscrowFactory State:"
    , "  Factory Owner: " ++ BC.unpack (factoryOwner factory)
    , "  Initialized: " ++ show (isInitialized factory)
    , "  Current Nonce: " ++ show (currentNonce factory)
    , "  Rescue Delay Src: " ++ show (rescueDelaySrc factory) ++ " seconds"
    , "  Rescue Delay Dst: " ++ show (rescueDelayDst factory) ++ " seconds"
    , "  📊 Statistics:"
    , "    Total Escrows: " ++ show (totalEscrowsCreated (statistics factory))
    , "    Active Escrows: " ++ show (activeEscrowsCount (statistics factory))
    , "    Completed Escrows: " ++ show (completedEscrowsCount (statistics factory))
    , "    Total Volume: " ++ show (totalVolumeProcessed (statistics factory)) ++ " ADA"
    , "    Source Escrows: " ++ show (srcEscrowsCreated (statistics factory))
    , "    Destination Escrows: " ++ show (dstEscrowsCreated (statistics factory))
    , "  📋 Registry: " ++ show (Map.size (escrowRegistry factory)) ++ " escrows registered"
    ]

-- | Set deployment time for timelocks (helper function)
setDeployedAt :: Timelocks -> Integer -> Timelocks
setDeployedAt timelocks timestamp = timelocks { deployedAt = timestamp }
