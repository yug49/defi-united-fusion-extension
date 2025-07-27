{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.TakerTraitsSpec (spec) where

import Test.Hspec
import Prelude
import Data.Bits (setBit, clearBit, (.|.))
import Lib.TakerTraits
import Lib.Types (TakerTraits(..))

spec :: Spec
spec = describe "TakerTraits" $ do
    flagCheckingSpec
    dataExtractionSpec

flagCheckingSpec :: Spec
flagCheckingSpec = describe "Flag checking" $ do
    it "detects making amount flag correctly" $ do
        let traitsWithMakingAmount = TakerTraits $ setBit 0 255  -- Set MAKER_AMOUNT_FLAG
            traitsWithTakingAmount = TakerTraits 0
        isMakingAmount traitsWithMakingAmount `shouldBe` True
        isMakingAmount traitsWithTakingAmount `shouldBe` False

    it "detects unwrap WETH flag correctly" $ do
        let traitsWithUnwrap = TakerTraits $ setBit 0 254  -- Set UNWRAP_WETH_FLAG
            traitsWithoutUnwrap = TakerTraits 0
        unwrapWeth traitsWithUnwrap `shouldBe` True
        unwrapWeth traitsWithoutUnwrap `shouldBe` False

    it "detects skip permit flag correctly" $ do
        let traitsSkipPermit = TakerTraits $ setBit 0 253  -- Set SKIP_ORDER_PERMIT_FLAG
            traitsWithPermit = TakerTraits 0
        skipOrderPermit traitsSkipPermit `shouldBe` True
        skipOrderPermit traitsWithPermit `shouldBe` False

    it "detects permit2 flag correctly" $ do
        let traitsWithPermit2 = TakerTraits $ setBit 0 252  -- Set USE_PERMIT2_FLAG
            traitsWithoutPermit2 = TakerTraits 0
        usePermit2 traitsWithPermit2 `shouldBe` True
        usePermit2 traitsWithoutPermit2 `shouldBe` False

    it "detects args has target flag correctly" $ do
        let traitsWithTarget = TakerTraits $ setBit 0 251  -- Set ARGS_HAS_TARGET
            traitsWithoutTarget = TakerTraits 0
        argsHasTarget traitsWithTarget `shouldBe` True
        argsHasTarget traitsWithoutTarget `shouldBe` False

dataExtractionSpec :: Spec
dataExtractionSpec = describe "Data extraction" $ do
    it "extracts extension length correctly" $ do
        let extensionLen = 1000
            traits = TakerTraits $ extensionLen * (2^224)  -- Shift to extension length offset
        argsExtensionLength traits `shouldBe` fromIntegral extensionLen

    it "extracts interaction length correctly" $ do
        let interactionLen = 500
            traits = TakerTraits $ interactionLen * (2^200)  -- Shift to interaction length offset
        argsInteractionLength traits `shouldBe` fromIntegral interactionLen

    it "extracts threshold correctly" $ do
        let thresholdAmount = 1500000
            -- Threshold is in the low bits, so no shifting needed
            traits = TakerTraits thresholdAmount
        threshold traits `shouldBe` thresholdAmount

    it "handles combined traits correctly" $ do
        let thresholdAmount = 1000000
            makingAmountFlag = setBit 0 255
            unwrapFlag = setBit 0 254
            combined = thresholdAmount .|. makingAmountFlag .|. unwrapFlag
            traits = TakerTraits combined
        
        threshold traits `shouldBe` thresholdAmount
        isMakingAmount traits `shouldBe` True
        unwrapWeth traits `shouldBe` True
        skipOrderPermit traits `shouldBe` False
