{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.OrderLibSpec (spec) where

import Test.Hspec
import Prelude
import qualified Data.ByteString as BS
import Data.Bits (setBit, (.|.), shiftL)
import Lib.Types
import Lib.OrderLib

spec :: Spec
spec = describe "OrderLib" $ do
    orderHashingSpec
    amountCalculationSpec
    orderValidationSpec
    receiverSpec

orderHashingSpec :: Spec
orderHashingSpec = describe "Order hashing" $ do
    it "generates consistent hashes for same order" $ do
        let order = createTestOrder
            hash1 = hashOrder order
            hash2 = hashOrder order
        hash1 `shouldBe` hash2
        BS.length hash1 `shouldBe` 32  -- Blake2b_256 produces 32-byte hashes

    it "generates different hashes for different orders" $ do
        let order1 = createTestOrder
            order2 = order1 { orderSalt = orderSalt order1 + 1 }
            hash1 = hashOrder order1
            hash2 = hashOrder order2
        hash1 `shouldNotBe` hash2

amountCalculationSpec :: Spec
amountCalculationSpec = describe "Amount calculations" $ do
    it "calculates making amount correctly with no extension" $ do
        let order = createTestOrder
            extension = BS.empty
            requestedTaking = 500000  -- Half of order taking amount
            remaining = orderMakingAmount order
            orderHash = hashOrder order
            result = calculateMakingAmount order extension requestedTaking remaining orderHash
        result `shouldBe` 500000  -- Should be half of making amount

    it "calculates taking amount correctly with no extension" $ do
        let order = createTestOrder
            extension = BS.empty
            requestedMaking = 500000  -- Half of order making amount
            remaining = orderMakingAmount order
            orderHash = hashOrder order
            result = calculateTakingAmount order extension requestedMaking remaining orderHash
        result `shouldBe` 500000  -- Should be half of taking amount

    it "handles proportional calculations correctly" $ do
        let order = createTestOrder { orderMakingAmount = 300, orderTakingAmount = 900 }
            extension = BS.empty
            requestedTaking = 300  -- 1/3 of taking amount
            remaining = orderMakingAmount order
            orderHash = hashOrder order
            result = calculateMakingAmount order extension requestedTaking remaining orderHash
        result `shouldBe` 100  -- Should be 1/3 of making amount

orderValidationSpec :: Spec
orderValidationSpec = describe "Order validation" $ do
    it "validates order without extension correctly" $ do
        let order = createTestOrder { orderMakerTraits = 0 }  -- No extension flag
            extension = BS.empty
            currentTime = 1640995200
        case validateOrder order extension currentTime of
            Right () -> return ()
            Left err -> expectationFailure $ "Order validation failed: " ++ show err

    it "rejects order with missing extension" $ do
        let order = createTestOrder { orderMakerTraits = setBit 0 249 }  -- Has extension flag
            extension = BS.empty
        case isValidExtension order extension of
            (False, Just InvalidatedOrder) -> return ()
            (False, Just _) -> expectationFailure "Wrong error type"
            (True, _) -> expectationFailure "Should have failed validation"
            (False, Nothing) -> expectationFailure "Should have specific error"

    it "rejects expired orders" $ do
        let expiredTime = 1640995100
            currentTime = 1640995200
            traits = expiredTime * (2^80)  -- Set expiration
            order = createTestOrder { orderMakerTraits = traits }
            extension = BS.empty
        case validateOrder order extension currentTime of
            Left OrderExpired -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right () -> expectationFailure "Should have failed with expiration error"

    it "rejects orders with zero amounts" $ do
        let order = createTestOrder { orderMakingAmount = 0 }
            extension = BS.empty
            currentTime = 1640995200
        case validateOrder order extension currentTime of
            Left SwapWithZeroAmount -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right () -> expectationFailure "Should have failed with zero amount error"

receiverSpec :: Spec
receiverSpec = describe "Receiver logic" $ do
    it "returns receiver when specified" $ do
        let order = createTestOrder { orderReceiver = "specific_receiver" }
            receiver = getReceiver order
        receiver `shouldBe` "specific_receiver"

    it "returns maker when receiver is empty" $ do
        let order = createTestOrder { orderReceiver = BS.empty }
            receiver = getReceiver order
        receiver `shouldBe` orderMaker order

-- Helper functions
createTestOrder :: Order
createTestOrder = Order
    { orderSalt = 12345
    , orderMaker = "test_maker_address"
    , orderReceiver = "test_receiver_address"
    , orderMakerAsset = ADA
    , orderTakerAsset = CustomAsset "USDC"
    , orderMakingAmount = 1000000
    , orderTakingAmount = 1000000  -- 1:1 ratio for simple testing
    , orderMakerTraits = 0
    }
