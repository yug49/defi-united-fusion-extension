{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Types where

import Prelude
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import qualified Data.Map as Map

-- | Address type for Cardano (simplified)
type Address = ByteString

-- | Asset identifier (using ADA for now, but extensible)
data AssetId = ADA | CustomAsset ByteString
    deriving (Show, Eq, Ord, Generic)

-- | Order struct equivalent to Solidity version
data Order = Order
    { orderSalt :: Integer
    , orderMaker :: Address
    , orderReceiver :: Address
    , orderMakerAsset :: AssetId
    , orderTakerAsset :: AssetId
    , orderMakingAmount :: Integer
    , orderTakingAmount :: Integer
    , orderMakerTraits :: Integer  -- Encoded traits as single Integer
    } deriving (Show, Eq, Generic)

-- | Order hash type
type OrderHash = ByteString

-- | Signature type for Ed25519
data Signature = Signature
    { sigR :: ByteString  -- 32 bytes
    , sigS :: ByteString  -- 32 bytes
    } deriving (Show, Eq, Generic)

-- | Taker traits (simplified version)
newtype TakerTraits = TakerTraits Integer
    deriving (Show, Eq, Generic)

-- | Order state for tracking fills and cancellations
data OrderState = OrderState
    { osRemainingAmount :: Integer
    , osIsCancelled :: Bool
    , osTimestamp :: Integer
    } deriving (Show, Eq, Generic)

-- | Protocol errors
data ProtocolError 
    = InvalidatedOrder
    | TakingAmountExceeded
    | PrivateOrder
    | BadSignature
    | OrderExpired
    | WrongSeriesNonce
    | SwapWithZeroAmount
    | PartialFillNotAllowed
    | OrderIsNotSuitableForMassInvalidation
    | EpochManagerAndBitInvalidatorsAreIncompatible
    | ReentrancyDetected
    | PredicateIsNotTrue
    | TakingAmountTooHigh
    | MakingAmountTooLow
    | TransferFromMakerToTakerFailed
    | TransferFromTakerToMakerFailed
    | MismatchArraysLengths
    | InvalidPermit2Transfer
    deriving (Show, Eq, Generic)

-- | Order registry state
type OrderRegistry = Map.Map OrderHash OrderState

-- | Fill result
data FillResult = FillResult
    { frMakingAmount :: Integer
    , frTakingAmount :: Integer
    , frOrderHash :: OrderHash
    } deriving (Show, Eq, Generic)

-- | Order events
data OrderEvent
    = OrderFilled OrderHash Integer  -- orderHash, remainingAmount
    | OrderCancelled OrderHash
    | BitInvalidatorUpdated Address Integer Integer  -- maker, slotIndex, slotValue
    deriving (Show, Eq, Generic)

-- | Extension data type (simplified)
type Extension = ByteString

-- | Predicate function type
type Predicate = Order -> Extension -> Bool

-- | Order with extension
data OrderWithExtension = OrderWithExtension
    { oweOrder :: Order
    , oweExtension :: Extension
    } deriving (Show, Eq, Generic)
