{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Contracts.LimitOrderProtocol
    ( -- * Protocol state
      ProtocolState(..)
    , initProtocolState
      -- * Order operations
    , fillOrder
    , fillOrderWithArgs
    , cancelOrder
    , cancelOrders
      -- * Query operations
    , hashOrder
    , getOrderState
    , checkPredicate
      -- * Admin operations
    , pause
    , unpause
      -- * Helper functions
    , validateAndFillOrder
      -- * Re-exports for testing
    , module Lib.Types
    ) where

import Prelude
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString as BS
import Control.Monad (when, unless)

import Lib.Types
import Lib.AmountCalculator
import qualified Lib.MakerTraits as MT
import qualified Lib.TakerTraits as TT
import qualified Lib.OrderLib as OL
import qualified Lib.OrderRegistry as OR

-- | Protocol state containing all necessary data
data ProtocolState = ProtocolState
    { psOrderRegistry :: OrderRegistry
    , psOrderStore :: Map OrderHash Order  -- Store actual orders
    , psInvalidatedNonces :: Map Address [Integer]  -- Simplified bit invalidator
    , psIsPaused :: Bool
    , psOwner :: Address
    , psDomainSeparator :: BS.ByteString
    } deriving (Show, Eq)

-- | Initialize protocol state
initProtocolState :: Address -> ProtocolState
initProtocolState owner = ProtocolState
    { psOrderRegistry = OR.initRegistry
    , psOrderStore = Map.empty
    , psInvalidatedNonces = Map.empty
    , psIsPaused = False
    , psOwner = owner
    , psDomainSeparator = OL.domainSeparator
    }

-- | Fill an order (main entry point)
fillOrder :: Order -> Signature -> Integer -> TakerTraits -> Address -> Integer 
          -> ProtocolState -> Either ProtocolError (FillResult, ProtocolState, [OrderEvent])
fillOrder order signature amount takerTraits taker currentTime state = do
    -- Check if protocol is paused
    when (psIsPaused state) $ Left InvalidatedOrder
    
    -- Validate and fill order
    validateAndFillOrder order signature amount takerTraits taker BS.empty currentTime state

-- | Fill order with additional arguments (extension, interaction data)
fillOrderWithArgs :: Order -> Signature -> Integer -> TakerTraits -> Address -> BS.ByteString 
                  -> Integer -> ProtocolState 
                  -> Either ProtocolError (FillResult, ProtocolState, [OrderEvent])
fillOrderWithArgs order signature amount takerTraits taker args currentTime state = do
    -- Check if protocol is paused
    when (psIsPaused state) $ Left InvalidatedOrder
    
    -- Parse args to extract extension (simplified)
    let extension = args  -- In real implementation, would parse args properly
    
    -- Validate and fill order
    validateAndFillOrder order signature amount takerTraits taker extension currentTime state

-- | Core order validation and filling logic
validateAndFillOrder :: Order -> Signature -> Integer -> TakerTraits -> Address -> Extension 
                     -> Integer -> ProtocolState 
                     -> Either ProtocolError (FillResult, ProtocolState, [OrderEvent])
validateAndFillOrder order signature amount takerTraits taker extension currentTime state = do
    -- Calculate order hash
    let orderHash = OL.hashOrder order
    
    -- Check order validity
    OL.validateOrder order extension currentTime
    
    -- Get remaining making amount
    let remainingMakingAmount = OR.getRemainingAmount orderHash (psOrderRegistry state)
    
    -- If first fill, validate signature and add to registry
    when (remainingMakingAmount == 0) $ do
        -- Validate signature
        unless (OL.validateSignature order signature (orderMaker order)) $ 
            Left BadSignature
        
        -- Add order to store
        return ()
    
    -- Check if order is cancelled
    when (OR.isOrderCancelled orderHash (psOrderRegistry state)) $ Left InvalidatedOrder
    
    -- Get actual remaining amount (either full amount for new orders or current remaining)
    let actualRemaining = if remainingMakingAmount == 0 
                         then orderMakingAmount order 
                         else remainingMakingAmount
    
    -- Calculate making and taking amounts
    (makingAmount, takingAmount) <- calculateFillAmounts order extension amount takerTraits actualRemaining orderHash
    
    -- Validate partial fill allowance
    unless (MT.allowPartialFills (orderMakerTraits order) || makingAmount == orderMakingAmount order) $
        Left PartialFillNotAllowed
    
    -- Check for zero amounts
    when (makingAmount == 0 || takingAmount == 0) $ Left SwapWithZeroAmount
    
    -- Update order state
    let newRemainingAmount = actualRemaining - makingAmount
        updatedRegistry = OR.updateRemainingAmount orderHash newRemainingAmount (psOrderRegistry state)
        updatedStore = if remainingMakingAmount == 0 
                      then Map.insert orderHash order (psOrderStore state)
                      else psOrderStore state
        newState = state { psOrderRegistry = updatedRegistry, psOrderStore = updatedStore }
    
    -- Create fill result
    let fillResult = FillResult makingAmount takingAmount orderHash
        events = [OrderFilled orderHash newRemainingAmount]
    
    return (fillResult, newState, events)

-- | Calculate making and taking amounts based on taker preferences
calculateFillAmounts :: Order -> Extension -> Integer -> TakerTraits -> Integer -> OrderHash 
                     -> Either ProtocolError (Integer, Integer)
calculateFillAmounts order extension amount takerTraits remainingMakingAmount orderHash = do
    if TT.isMakingAmount takerTraits
    then do
        -- Amount is making amount
        let makingAmount = min amount remainingMakingAmount
            takingAmount = OL.calculateTakingAmount order extension makingAmount remainingMakingAmount orderHash
        
        -- Check threshold
        let threshold = TT.threshold takerTraits
        when (threshold > 0 && takingAmount > threshold) $ Left TakingAmountTooHigh
        
        return (makingAmount, takingAmount)
    else do
        -- Amount is taking amount
        let takingAmount = amount
            makingAmount = OL.calculateMakingAmount order extension takingAmount remainingMakingAmount orderHash
        
        -- Adjust if making amount exceeds remaining
        (finalMakingAmount, finalTakingAmount) <- 
            if makingAmount > remainingMakingAmount
            then do
                let adjustedMakingAmount = remainingMakingAmount
                    adjustedTakingAmount = OL.calculateTakingAmount order extension adjustedMakingAmount remainingMakingAmount orderHash
                when (adjustedTakingAmount > amount) $ Left TakingAmountExceeded
                return (adjustedMakingAmount, adjustedTakingAmount)
            else return (makingAmount, takingAmount)
        
        -- Check threshold
        let threshold = TT.threshold takerTraits
        when (threshold > 0 && finalMakingAmount < threshold) $ Left MakingAmountTooLow
        
        return (finalMakingAmount, finalTakingAmount)

-- | Cancel a single order
cancelOrder :: Order -> OrderHash -> Address -> ProtocolState -> Either ProtocolError (ProtocolState, [OrderEvent])
cancelOrder order orderHash sender state = do
    -- Check if sender is the maker
    unless (sender == orderMaker order) $ Left PrivateOrder
    
    -- Cancel the order
    let updatedRegistry = OR.cancelOrder orderHash (psOrderRegistry state)
        newState = state { psOrderRegistry = updatedRegistry }
        events = [OrderCancelled orderHash]
    
    return (newState, events)

-- | Cancel multiple orders
cancelOrders :: [Order] -> [OrderHash] -> Address -> ProtocolState -> Either ProtocolError (ProtocolState, [OrderEvent])
cancelOrders orders orderHashes sender state = do
    -- Check array lengths match
    unless (length orders == length orderHashes) $ Left MismatchArraysLengths
    
    -- Cancel each order
    let cancelResults = zipWith (\order orderHash -> cancelOrder order orderHash sender state) orders orderHashes
    
    -- Process results (simplified - in real implementation would handle partial failures)
    case sequence cancelResults of
        Left err -> Left err
        Right results -> 
            let finalState = foldl (\s (newS, _) -> newS) state results
                allEvents = concatMap snd results
            in Right (finalState, allEvents)

-- | Get order hash
hashOrder :: Order -> OrderHash
hashOrder = OL.hashOrder

-- | Get order state
getOrderState :: OrderHash -> ProtocolState -> Maybe OrderState
getOrderState orderHash state = OR.getOrderState orderHash (psOrderRegistry state)

-- | Check predicate (simplified implementation)
checkPredicate :: BS.ByteString -> Order -> Extension -> Bool
checkPredicate predicate order extension = 
    -- Simplified predicate checking - always returns true
    -- In real implementation, this would evaluate the predicate logic
    True

-- | Pause the protocol (admin only)
pause :: Address -> ProtocolState -> Either ProtocolError ProtocolState
pause sender state = do
    unless (sender == psOwner state) $ Left PrivateOrder
    return $ state { psIsPaused = True }

-- | Unpause the protocol (admin only)
unpause :: Address -> ProtocolState -> Either ProtocolError ProtocolState
unpause sender state = do
    unless (sender == psOwner state) $ Left PrivateOrder
    return $ state { psIsPaused = False }
