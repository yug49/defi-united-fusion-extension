{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.OrderLib
    ( -- * Order hashing
      hashOrder
    , hashOrderWithDomain
      -- * Amount calculations
    , calculateMakingAmount
    , calculateTakingAmount
      -- * Order validation
    , getReceiver
    , isValidExtension
    , validateOrder
    , validateSignature
      -- * Helper functions
    , domainSeparator
    ) where

import Prelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Crypto.Hash (Blake2b_256, hash)
import qualified Crypto.Hash as Hash
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteArray as BA

import Lib.Types
import Lib.AmountCalculator
import qualified Lib.MakerTraits as MT

-- | Domain separator for EIP-712 style hashing (adapted for Cardano)
domainSeparator :: ByteString
domainSeparator = "1inch Limit Order Protocol Cardano v1"

-- | Calculate order hash using Blake2b (Cardano's preferred hash function)
hashOrder :: Order -> OrderHash
hashOrder order = hashOrderWithDomain order domainSeparator

-- | Calculate order hash with custom domain separator
hashOrderWithDomain :: Order -> ByteString -> OrderHash
hashOrderWithDomain order domain = 
    let orderBytes = encodeOrder order
        domainHash = hash domain :: Hash.Digest Blake2b_256
        orderHash = hash orderBytes :: Hash.Digest Blake2b_256
        combined = BS.concat [BA.convert domainHash, BA.convert orderHash]
    in BA.convert (hash combined :: Hash.Digest Blake2b_256)

-- | Encode order to bytes for hashing
encodeOrder :: Order -> ByteString
encodeOrder order = BS.concat
    [ encodeInteger (orderSalt order)
    , orderMaker order
    , orderReceiver order  
    , encodeAssetId (orderMakerAsset order)
    , encodeAssetId (orderTakerAsset order)
    , encodeInteger (orderMakingAmount order)
    , encodeInteger (orderTakingAmount order)
    , encodeInteger (orderMakerTraits order)
    ]

-- | Helper to encode integer to bytes
encodeInteger :: Integer -> ByteString
encodeInteger = C8.pack . show

-- | Helper to encode asset ID to bytes
encodeAssetId :: AssetId -> ByteString
encodeAssetId ADA = "ADA"
encodeAssetId (CustomAsset bs) = bs

-- | Get the receiver address for an order
getReceiver :: Order -> Address
getReceiver order = 
    if BS.null (orderReceiver order)
    then orderMaker order
    else orderReceiver order

-- | Calculate making amount based on requested taking amount
calculateMakingAmount :: Order -> Extension -> Integer -> Integer -> OrderHash -> Integer
calculateMakingAmount order extension requestedTakingAmount remainingMakingAmount orderHash =
    if BS.null extension
    then 
        -- Linear proportion using our AmountCalculator
        getMakingAmount (orderMakingAmount order) (orderTakingAmount order) requestedTakingAmount
    else
        -- For now, fallback to linear calculation
        -- In full implementation, this would call extension-specific calculators
        getMakingAmount (orderMakingAmount order) (orderTakingAmount order) requestedTakingAmount

-- | Calculate taking amount based on requested making amount  
calculateTakingAmount :: Order -> Extension -> Integer -> Integer -> OrderHash -> Integer
calculateTakingAmount order extension requestedMakingAmount remainingMakingAmount orderHash =
    if BS.null extension
    then
        -- Linear proportion using our AmountCalculator
        getTakingAmount (orderMakingAmount order) (orderTakingAmount order) requestedMakingAmount
    else
        -- For now, fallback to linear calculation
        -- In full implementation, this would call extension-specific calculators
        getTakingAmount (orderMakingAmount order) (orderTakingAmount order) requestedMakingAmount

-- | Validate extension associated with an order
isValidExtension :: Order -> Extension -> (Bool, Maybe ProtocolError)
isValidExtension order extension =
    if MT.hasExtension (orderMakerTraits order)
    then
        if BS.null extension
        then (False, Just InvalidatedOrder)  -- Missing extension
        else 
            -- Simplified validation - in full implementation would check extension hash
            -- against order salt like in Solidity version
            (True, Nothing)
    else
        if not (BS.null extension)
        then (False, Just InvalidatedOrder)  -- Unexpected extension
        else (True, Nothing)

-- | Comprehensive order validation
validateOrder :: Order -> Extension -> Integer -> Either ProtocolError ()
validateOrder order extension currentTime = do
    -- Check extension validity
    case isValidExtension order extension of
        (False, Just err) -> Left err
        (False, Nothing) -> Left InvalidatedOrder
        (True, _) -> Right ()
    
    -- Check expiration
    if MT.isExpired (orderMakerTraits order) currentTime
    then Left OrderExpired
    else Right ()
    
    -- Check for zero amounts
    if orderMakingAmount order == 0 || orderTakingAmount order == 0
    then Left SwapWithZeroAmount
    else Right ()

-- | Validate Ed25519 signature (simplified implementation)
validateSignature :: Order -> Signature -> Address -> Bool
validateSignature order sig makerAddr =
    -- This is a simplified placeholder
    -- In a real implementation, you would:
    -- 1. Use proper Ed25519 signature verification
    -- 2. Verify the signature against the order hash and maker's public key
    -- 3. Handle public key derivation from address
    True  -- Placeholder - always returns true for now
