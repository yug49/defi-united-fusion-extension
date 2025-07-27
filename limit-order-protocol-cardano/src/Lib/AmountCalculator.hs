{-# LANGUAGE NoImplicitPrelude #-}

module Lib.AmountCalculator
    ( getMakingAmount
    , getTakingAmount
    ) where

import Prelude

-- | The helper library to calculate linearly taker amount from maker amount and vice versa.

-- | Calculates maker amount
-- Returns floored maker amount
getMakingAmount :: Integer -> Integer -> Integer -> Integer
getMakingAmount orderMakerAmount orderTakerAmount swapTakerAmount =
    (swapTakerAmount * orderMakerAmount) `div` orderTakerAmount

-- | Calculates taker amount  
-- Returns ceiled taker amount
getTakingAmount :: Integer -> Integer -> Integer -> Integer
getTakingAmount orderMakerAmount orderTakerAmount swapMakerAmount =
    (swapMakerAmount * orderTakerAmount + orderMakerAmount - 1) `div` orderMakerAmount
