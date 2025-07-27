{-# LANGUAGE NoImplicitPrelude #-}

module Lib.OrderRegistry
    ( -- * Registry operations
      initRegistry
    , addOrder
    , getOrderState
    , updateOrderState
    , cancelOrder
    , isOrderCancelled
      -- * Remaining amount operations
    , getRemainingAmount
    , updateRemainingAmount
    , isFullyFilled
      -- * Bit invalidator operations (simplified)
    , invalidateNonce
    , isNonceInvalidated
      -- * Registry queries
    , getAllActiveOrders
    , getOrdersByMaker
    ) where

import Prelude
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)

import Lib.Types

-- | Initialize empty order registry
initRegistry :: OrderRegistry
initRegistry = Map.empty

-- | Add a new order to the registry
addOrder :: OrderHash -> Order -> OrderRegistry -> OrderRegistry
addOrder orderHash order registry =
    let initialState = OrderState 
            { osRemainingAmount = orderMakingAmount order
            , osIsCancelled = False
            , osTimestamp = 0  -- Would use actual timestamp in real implementation
            }
    in Map.insert orderHash initialState registry

-- | Get order state from registry
getOrderState :: OrderHash -> OrderRegistry -> Maybe OrderState
getOrderState = Map.lookup

-- | Update order state in registry
updateOrderState :: OrderHash -> OrderState -> OrderRegistry -> OrderRegistry
updateOrderState = Map.insert

-- | Cancel an order
cancelOrder :: OrderHash -> OrderRegistry -> OrderRegistry
cancelOrder orderHash registry =
    case Map.lookup orderHash registry of
        Nothing -> registry  -- Order doesn't exist
        Just state -> Map.insert orderHash (state { osIsCancelled = True }) registry

-- | Check if an order is cancelled
isOrderCancelled :: OrderHash -> OrderRegistry -> Bool
isOrderCancelled orderHash registry =
    case Map.lookup orderHash registry of
        Nothing -> False  -- Order doesn't exist, not cancelled
        Just state -> osIsCancelled state

-- | Get remaining amount for an order
getRemainingAmount :: OrderHash -> OrderRegistry -> Integer
getRemainingAmount orderHash registry =
    case Map.lookup orderHash registry of
        Nothing -> 0  -- Order doesn't exist
        Just state -> if osIsCancelled state then 0 else osRemainingAmount state

-- | Update remaining amount for an order
updateRemainingAmount :: OrderHash -> Integer -> OrderRegistry -> OrderRegistry
updateRemainingAmount orderHash newRemaining registry =
    case Map.lookup orderHash registry of
        Nothing -> registry  -- Order doesn't exist
        Just state -> 
            let updatedState = state { osRemainingAmount = newRemaining }
            in Map.insert orderHash updatedState registry

-- | Check if an order is fully filled
isFullyFilled :: OrderHash -> OrderRegistry -> Bool
isFullyFilled orderHash registry = getRemainingAmount orderHash registry <= 0

-- | Invalidate a nonce (simplified bit invalidator)
-- In the real implementation, this would use proper bit manipulation
invalidateNonce :: Address -> Integer -> Map Address [Integer] -> Map Address [Integer]
invalidateNonce maker nonce invalidatedNonces =
    let currentNonces = fromMaybe [] (Map.lookup maker invalidatedNonces)
        updatedNonces = if nonce `elem` currentNonces then currentNonces else nonce : currentNonces
    in Map.insert maker updatedNonces invalidatedNonces

-- | Check if a nonce is invalidated
isNonceInvalidated :: Address -> Integer -> Map Address [Integer] -> Bool
isNonceInvalidated maker nonce invalidatedNonces =
    case Map.lookup maker invalidatedNonces of
        Nothing -> False
        Just nonces -> nonce `elem` nonces

-- | Get all active (non-cancelled, non-fully-filled) orders
getAllActiveOrders :: OrderRegistry -> [OrderHash]
getAllActiveOrders registry =
    [ orderHash 
    | (orderHash, state) <- Map.toList registry
    , not (osIsCancelled state)
    , osRemainingAmount state > 0
    ]

-- | Get all orders by a specific maker
getOrdersByMaker :: Address -> OrderRegistry -> Map OrderHash Order -> [OrderHash]
getOrdersByMaker makerAddr registry orderStore =
    [ orderHash
    | (orderHash, state) <- Map.toList registry
    , case Map.lookup orderHash orderStore of
        Just order -> orderMaker order == makerAddr
        Nothing -> False
    ]

-- | Helper function to calculate filled amount
getFilledAmount :: OrderHash -> Order -> OrderRegistry -> Integer
getFilledAmount orderHash order registry =
    let remaining = getRemainingAmount orderHash registry
        original = orderMakingAmount order
    in original - remaining

-- | Helper function to check if order allows partial fills
allowsPartialFills :: Order -> Bool
allowsPartialFills order = 
    -- This would use the MakerTraits library
    -- For now, assume all orders allow partial fills
    True

-- | Helper function to get order fill percentage
getFillPercentage :: OrderHash -> Order -> OrderRegistry -> Double
getFillPercentage orderHash order registry =
    let filled = fromIntegral $ getFilledAmount orderHash order registry
        total = fromIntegral $ orderMakingAmount order
    in if total == 0 then 0.0 else filled / total * 100.0
