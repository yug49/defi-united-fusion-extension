{-# LANGUAGE NoImplicitPrelude #-}

module Lib.MakerTraits
    ( -- * Trait flag checking
      hasExtension
    , isAllowedSender
    , isExpired
    , allowPartialFills
    , allowMultipleFills
    , needPreInteractionCall
    , needPostInteractionCall
    , needCheckEpochManager
    , useBitInvalidator
    , usePermit2
    , unwrapWeth
      -- * Trait data extraction
    , getAllowedSender
    , getExpirationTime
    , getNonceOrEpoch
    , getSeries
      -- * Utility functions
    , getCurrentTime
    ) where

import Prelude
import Data.Bits ((.|.), (.&.), shiftR, shiftL, testBit)
import Lib.Types

-- | Bit positions for maker traits flags (high bits)
_NO_PARTIAL_FILLS_FLAG :: Int
_NO_PARTIAL_FILLS_FLAG = 255

_ALLOW_MULTIPLE_FILLS_FLAG :: Int  
_ALLOW_MULTIPLE_FILLS_FLAG = 254

_PRE_INTERACTION_CALL_FLAG :: Int
_PRE_INTERACTION_CALL_FLAG = 252

_POST_INTERACTION_CALL_FLAG :: Int
_POST_INTERACTION_CALL_FLAG = 251

_NEED_CHECK_EPOCH_MANAGER_FLAG :: Int
_NEED_CHECK_EPOCH_MANAGER_FLAG = 250

_HAS_EXTENSION_FLAG :: Int
_HAS_EXTENSION_FLAG = 249

_USE_PERMIT2_FLAG :: Int
_USE_PERMIT2_FLAG = 248

_UNWRAP_WETH_FLAG :: Int
_UNWRAP_WETH_FLAG = 247

_USE_BIT_INVALIDATOR_FLAG :: Int
_USE_BIT_INVALIDATOR_FLAG = 246

-- | Masks and offsets for data fields (low 200 bits)
_ALLOWED_SENDER_MASK :: Integer
_ALLOWED_SENDER_MASK = 2^80 - 1  -- type(uint80).max equivalent

_EXPIRATION_OFFSET :: Int
_EXPIRATION_OFFSET = 80

_EXPIRATION_MASK :: Integer
_EXPIRATION_MASK = 2^40 - 1  -- type(uint40).max equivalent

_NONCE_OR_EPOCH_OFFSET :: Int
_NONCE_OR_EPOCH_OFFSET = 120

_NONCE_OR_EPOCH_MASK :: Integer
_NONCE_OR_EPOCH_MASK = 2^40 - 1

_SERIES_OFFSET :: Int
_SERIES_OFFSET = 160

_SERIES_MASK :: Integer
_SERIES_MASK = 2^40 - 1

-- | Check if the order has extension flag set
hasExtension :: Integer -> Bool
hasExtension makerTraits = testBit makerTraits _HAS_EXTENSION_FLAG

-- | Check if the maker allows a specific taker to fill the order
isAllowedSender :: Integer -> Address -> Bool
isAllowedSender makerTraits sender = 
    let allowedSender = makerTraits .&. _ALLOWED_SENDER_MASK
        senderLow = addressToInteger sender .&. _ALLOWED_SENDER_MASK
    in allowedSender == 0 || allowedSender == senderLow
  where
    -- Simplified address to integer conversion (you'd implement proper conversion)
    addressToInteger :: Address -> Integer
    addressToInteger _ = 0  -- Placeholder - implement based on your address format

-- | Check if the order has expired  
isExpired :: Integer -> Integer -> Bool
isExpired makerTraits currentTime =
    let expiration = getExpirationTime makerTraits
    in expiration /= 0 && expiration < currentTime

-- | Check if partial fills are allowed (inverted logic from NO_PARTIAL_FILLS_FLAG)
allowPartialFills :: Integer -> Bool
allowPartialFills makerTraits = not (testBit makerTraits _NO_PARTIAL_FILLS_FLAG)

-- | Check if multiple fills are allowed
allowMultipleFills :: Integer -> Bool  
allowMultipleFills makerTraits = testBit makerTraits _ALLOW_MULTIPLE_FILLS_FLAG

-- | Check if pre-interaction call is needed
needPreInteractionCall :: Integer -> Bool
needPreInteractionCall makerTraits = testBit makerTraits _PRE_INTERACTION_CALL_FLAG

-- | Check if post-interaction call is needed
needPostInteractionCall :: Integer -> Bool
needPostInteractionCall makerTraits = testBit makerTraits _POST_INTERACTION_CALL_FLAG

-- | Check if epoch manager check is needed
needCheckEpochManager :: Integer -> Bool
needCheckEpochManager makerTraits = testBit makerTraits _NEED_CHECK_EPOCH_MANAGER_FLAG

-- | Check if bit invalidator should be used
useBitInvalidator :: Integer -> Bool
useBitInvalidator makerTraits = testBit makerTraits _USE_BIT_INVALIDATOR_FLAG

-- | Check if permit2 should be used (not applicable for Cardano, but kept for compatibility)
usePermit2 :: Integer -> Bool
usePermit2 makerTraits = testBit makerTraits _USE_PERMIT2_FLAG

-- | Check if WETH should be unwrapped (adapted for ADA wrapping if needed)
unwrapWeth :: Integer -> Bool
unwrapWeth makerTraits = testBit makerTraits _UNWRAP_WETH_FLAG

-- | Extract allowed sender from traits
getAllowedSender :: Integer -> Integer
getAllowedSender makerTraits = makerTraits .&. _ALLOWED_SENDER_MASK

-- | Extract expiration time from traits
getExpirationTime :: Integer -> Integer
getExpirationTime makerTraits = 
    (makerTraits `shiftR` _EXPIRATION_OFFSET) .&. _EXPIRATION_MASK

-- | Extract nonce or epoch from traits
getNonceOrEpoch :: Integer -> Integer
getNonceOrEpoch makerTraits = 
    (makerTraits `shiftR` _NONCE_OR_EPOCH_OFFSET) .&. _NONCE_OR_EPOCH_MASK

-- | Extract series from traits
getSeries :: Integer -> Integer
getSeries makerTraits = 
    (makerTraits `shiftR` _SERIES_OFFSET) .&. _SERIES_MASK

-- | Get current time (placeholder - implement with actual time source)
getCurrentTime :: IO Integer
getCurrentTime = return 1640995200  -- Placeholder timestamp
