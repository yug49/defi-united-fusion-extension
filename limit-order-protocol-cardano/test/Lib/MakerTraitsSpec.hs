{-# LANGUAGE NoImplicitPrelude #-}

module Lib.MakerTraitsSpec (spec) where

import Test.Hspec
import Prelude
import Data.Bits (setBit, clearBit)
import Lib.MakerTraits

spec :: Spec
spec = describe "MakerTraits" $ do
    flagCheckingSpec
    dataExtractionSpec
    expirationSpec

flagCheckingSpec :: Spec
flagCheckingSpec = describe "Flag checking" $ do
    it "detects extension flag correctly" $ do
        let traitsWithExtension = setBit 0 249  -- Set HAS_EXTENSION_FLAG
            traitsWithoutExtension = 0
        hasExtension traitsWithExtension `shouldBe` True
        hasExtension traitsWithoutExtension `shouldBe` False

    it "detects partial fills flag correctly" $ do
        let traitsNoPartialFills = setBit 0 255  -- Set NO_PARTIAL_FILLS_FLAG
            traitsAllowPartialFills = 0
        allowPartialFills traitsNoPartialFills `shouldBe` False
        allowPartialFills traitsAllowPartialFills `shouldBe` True

    it "detects multiple fills flag correctly" $ do
        let traitsAllowMultipleFills = setBit 0 254  -- Set ALLOW_MULTIPLE_FILLS_FLAG
            traitsNoMultipleFills = 0
        allowMultipleFills traitsAllowMultipleFills `shouldBe` True
        allowMultipleFills traitsNoMultipleFills `shouldBe` False

    it "detects interaction flags correctly" $ do
        let traitsWithPreInteraction = setBit 0 252
            traitsWithPostInteraction = setBit 0 251
            traitsWithoutInteractions = 0
        needPreInteractionCall traitsWithPreInteraction `shouldBe` True
        needPreInteractionCall traitsWithoutInteractions `shouldBe` False
        needPostInteractionCall traitsWithPostInteraction `shouldBe` True
        needPostInteractionCall traitsWithoutInteractions `shouldBe` False

dataExtractionSpec :: Spec
dataExtractionSpec = describe "Data extraction" $ do
    it "extracts expiration time correctly" $ do
        let expiration = 1640995200  -- Example timestamp
            traits = expiration * (2^80)  -- Shift to expiration offset
        getExpirationTime traits `shouldBe` expiration

    it "extracts nonce correctly" $ do
        let nonce = 12345
            traits = nonce * (2^120)  -- Shift to nonce offset
        getNonceOrEpoch traits `shouldBe` nonce

    it "extracts series correctly" $ do
        let series = 999
            traits = series * (2^160)  -- Shift to series offset
        getSeries traits `shouldBe` series

expirationSpec :: Spec
expirationSpec = describe "Expiration checking" $ do
    it "correctly identifies expired orders" $ do
        let currentTime = 1640995200
            expiredTime = 1640995100  -- 100 seconds ago
            futureTime = 1640995300   -- 100 seconds in future
            expiredTraits = expiredTime * (2^80)
            futureTraits = futureTime * (2^80)
            noExpirationTraits = 0
        
        isExpired expiredTraits currentTime `shouldBe` True
        isExpired futureTraits currentTime `shouldBe` False
        isExpired noExpirationTraits currentTime `shouldBe` False

    it "handles allowed sender checking" $ do
        let anyAddress = "any_address"
            specificAddress = "specific_address"
            -- Simplified test - in real implementation would properly encode addresses
            traitsForAnyAddress = 0
            traitsForSpecificAddress = 12345  -- Some non-zero value
        
        isAllowedSender traitsForAnyAddress anyAddress `shouldBe` True
        isAllowedSender traitsForAnyAddress specificAddress `shouldBe` True
        -- Note: Real implementation would need proper address encoding
