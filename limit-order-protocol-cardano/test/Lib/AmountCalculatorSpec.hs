{-# LANGUAGE NoImplicitPrelude #-}

module Lib.AmountCalculatorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude
import Lib.AmountCalculator

spec :: Spec
spec = describe "AmountCalculator" $ do
    getMakingAmountSpec
    getTakingAmountSpec
    edgeCaseSpec

getMakingAmountSpec :: Spec
getMakingAmountSpec = describe "getMakingAmount" $ do
    it "calculates maker amount correctly for basic case" $ do
        -- Order: 100 maker tokens for 200 taker tokens
        -- Swap: 50 taker tokens -> should get 25 maker tokens
        getMakingAmount 100 200 50 `shouldBe` 25

    it "calculates maker amount correctly for 1:1 ratio" $ do
        -- Order: 100 maker tokens for 100 taker tokens
        -- Swap: 30 taker tokens -> should get 30 maker tokens
        getMakingAmount 100 100 30 `shouldBe` 30

    it "calculates maker amount correctly for fractional result (floors)" $ do
        -- Order: 100 maker tokens for 300 taker tokens
        -- Swap: 100 taker tokens -> should get 33 maker tokens (floored from 33.33...)
        getMakingAmount 100 300 100 `shouldBe` 33

    it "handles large numbers correctly" $ do
        -- Order: 1000000 maker tokens for 2000000 taker tokens
        -- Swap: 500000 taker tokens -> should get 250000 maker tokens
        getMakingAmount 1000000 2000000 500000 `shouldBe` 250000

    it "returns 0 when swap taker amount is 0" $ do
        getMakingAmount 100 200 0 `shouldBe` 0

    it "maintains proportionality" $
        property $ \orderMaker orderTaker swapTaker ->
            orderMaker > 0 && orderTaker > 0 && swapTaker >= 0 ==>
                let result = getMakingAmount orderMaker orderTaker swapTaker
                    expectedRatio = fromIntegral swapTaker / fromIntegral orderTaker
                    actualRatio = fromIntegral result / fromIntegral orderMaker
                in abs (expectedRatio - actualRatio) <= (1 / fromIntegral orderMaker)

getTakingAmountSpec :: Spec
getTakingAmountSpec = describe "getTakingAmount" $ do
    it "calculates taker amount correctly for basic case" $ do
        -- Order: 100 maker tokens for 200 taker tokens
        -- Swap: 50 maker tokens -> should get 100 taker tokens
        getTakingAmount 100 200 50 `shouldBe` 100

    it "calculates taker amount correctly for 1:1 ratio" $ do
        -- Order: 100 maker tokens for 100 taker tokens
        -- Swap: 30 maker tokens -> should get 30 taker tokens
        getTakingAmount 100 100 30 `shouldBe` 30

    it "calculates taker amount correctly for fractional result (ceils)" $ do
        -- Order: 300 maker tokens for 100 taker tokens
        -- Swap: 100 maker tokens -> should get 34 taker tokens (ceiled from 33.33...)
        getTakingAmount 300 100 100 `shouldBe` 34

    it "handles large numbers correctly" $ do
        -- Order: 1000000 maker tokens for 2000000 taker tokens
        -- Swap: 250000 maker tokens -> should get 500000 taker tokens
        getTakingAmount 1000000 2000000 250000 `shouldBe` 500000

    it "returns 1 when swap maker amount is 1 and order taker amount > order maker amount" $ do
        -- Order: 100 maker tokens for 200 taker tokens
        -- Swap: 1 maker token -> should get 2 taker tokens (ceiled from 2)
        getTakingAmount 100 200 1 `shouldBe` 2

    it "returns 0 when swap maker amount is 0" $ do
        getTakingAmount 100 200 0 `shouldBe` 0

    it "maintains proportionality with ceiling" $
        property $ \orderMaker orderTaker swapMaker ->
            orderMaker > 0 && orderTaker > 0 && swapMaker >= 0 ==>
                let result = getTakingAmount orderMaker orderTaker swapMaker
                    expectedExact = (swapMaker * orderTaker + orderMaker - 1) `div` orderMaker
                in result == expectedExact

    it "always ceils the result (never undercharges)" $
        property $ \orderMaker orderTaker swapMaker ->
            orderMaker > 0 && orderTaker > 0 && swapMaker > 0 ==>
                let result = getTakingAmount orderMaker orderTaker swapMaker
                    exactResult = fromIntegral (swapMaker * orderTaker) / fromIntegral orderMaker
                in fromIntegral result >= exactResult

-- Edge case tests
edgeCaseSpec :: Spec
edgeCaseSpec = describe "Edge cases" $ do
    it "getMakingAmount with minimal fractional loss" $ do
        -- Testing precision with numbers that would cause rounding
        getMakingAmount 3 7 10 `shouldBe` 4  -- (10 * 3) / 7 = 30/7 = 4.28... -> 4

    it "getTakingAmount with minimal fractional gain" $ do
        -- Testing ceiling with numbers that would cause rounding
        getTakingAmount 7 3 10 `shouldBe` 5  -- (10 * 3 + 7 - 1) / 7 = 36/7 = 5.14... -> 5
