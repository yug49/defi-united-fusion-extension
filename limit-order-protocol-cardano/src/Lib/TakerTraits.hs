{-# LANGUAGE NoImplicitPrelude #-}

module Lib.TakerTraits
    ( -- * Trait flag checking
      argsHasTarget
    , isMakingAmount
    , unwrapWeth
    , skipOrderPermit
    , usePermit2
      -- * Trait data extraction
    , argsExtensionLength
    , argsInteractionLength
    , threshold
      -- * Utility functions
    , TakerTraits(..)
    ) where

import Prelude
import Data.Bits ((.|.), (.&.), shiftR, shiftL, testBit)
import Lib.Types (TakerTraits(..))

-- | Bit positions for taker traits flags (high bits)
_MAKER_AMOUNT_FLAG :: Int
_MAKER_AMOUNT_FLAG = 255

_UNWRAP_WETH_FLAG :: Int
_UNWRAP_WETH_FLAG = 254

_SKIP_ORDER_PERMIT_FLAG :: Int
_SKIP_ORDER_PERMIT_FLAG = 253

_USE_PERMIT2_FLAG :: Int
_USE_PERMIT2_FLAG = 252

_ARGS_HAS_TARGET :: Int
_ARGS_HAS_TARGET = 251

-- | Offsets and masks for data fields
_ARGS_EXTENSION_LENGTH_OFFSET :: Int
_ARGS_EXTENSION_LENGTH_OFFSET = 224

_ARGS_EXTENSION_LENGTH_MASK :: Integer
_ARGS_EXTENSION_LENGTH_MASK = 0xffffff

_ARGS_INTERACTION_LENGTH_OFFSET :: Int
_ARGS_INTERACTION_LENGTH_OFFSET = 200

_ARGS_INTERACTION_LENGTH_MASK :: Integer
_ARGS_INTERACTION_LENGTH_MASK = 0xffffff

_AMOUNT_MASK :: Integer
_AMOUNT_MASK = 0x000000000000000000ffffffffffffffffffffffffffffffffffffffffffffff

-- | Check if the args should contain target address
argsHasTarget :: TakerTraits -> Bool
argsHasTarget (TakerTraits traits) = testBit traits _ARGS_HAS_TARGET

-- | Retrieve the length of the extension calldata from the takerTraits
argsExtensionLength :: TakerTraits -> Integer
argsExtensionLength (TakerTraits traits) = 
    (traits `shiftR` _ARGS_EXTENSION_LENGTH_OFFSET) .&. _ARGS_EXTENSION_LENGTH_MASK

-- | Retrieve the length of the interaction calldata from the takerTraits
argsInteractionLength :: TakerTraits -> Integer
argsInteractionLength (TakerTraits traits) = 
    (traits `shiftR` _ARGS_INTERACTION_LENGTH_OFFSET) .&. _ARGS_INTERACTION_LENGTH_MASK

-- | Check if the taking amount should be calculated based on making amount
isMakingAmount :: TakerTraits -> Bool
isMakingAmount (TakerTraits traits) = testBit traits _MAKER_AMOUNT_FLAG

-- | Check if the order should unwrap WETH and send ETH to taker (adapted for ADA)
unwrapWeth :: TakerTraits -> Bool
unwrapWeth (TakerTraits traits) = testBit traits _UNWRAP_WETH_FLAG

-- | Check if the order should skip maker's permit execution
skipOrderPermit :: TakerTraits -> Bool
skipOrderPermit (TakerTraits traits) = testBit traits _SKIP_ORDER_PERMIT_FLAG

-- | Check if the order uses permit2 function (not applicable for Cardano)
usePermit2 :: TakerTraits -> Bool
usePermit2 (TakerTraits traits) = testBit traits _USE_PERMIT2_FLAG

-- | Extract threshold amount from traits
threshold :: TakerTraits -> Integer
threshold (TakerTraits traits) = traits .&. _AMOUNT_MASK
