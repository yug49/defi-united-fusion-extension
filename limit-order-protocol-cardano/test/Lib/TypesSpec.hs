{-# LANGUAGE NoImplicitPrelude #-}

module Lib.TypesSpec (spec) where

import Test.Hspec
import Prelude
import Lib.Types

spec :: Spec
spec = describe "Types" $ do
    orderSpec
    assetIdSpec
    signatureSpec

orderSpec :: Spec
orderSpec = describe "Order" $ do
    it "creates order with all fields" $ do
        let order = Order
                { orderSalt = 12345
                , orderMaker = "maker_address"
                , orderReceiver = "receiver_address"
                , orderMakerAsset = ADA
                , orderTakerAsset = CustomAsset "USDC"
                , orderMakingAmount = 1000000
                , orderTakingAmount = 2000000
                , orderMakerTraits = 0
                }
        orderSalt order `shouldBe` 12345
        orderMaker order `shouldBe` "maker_address"
        orderMakerAsset order `shouldBe` ADA
        orderTakerAsset order `shouldBe` CustomAsset "USDC"

assetIdSpec :: Spec
assetIdSpec = describe "AssetId" $ do
    it "handles ADA asset" $ do
        ADA `shouldBe` ADA
        
    it "handles custom assets" $ do
        let usdc = CustomAsset "USDC"
        usdc `shouldBe` CustomAsset "USDC"
        usdc `shouldNotBe` ADA

signatureSpec :: Spec
signatureSpec = describe "Signature" $ do
    it "creates signature with r and s components" $ do
        let sig = Signature "r_component_32_bytes" "s_component_32_bytes"
        sigR sig `shouldBe` "r_component_32_bytes"
        sigS sig `shouldBe` "s_component_32_bytes"
