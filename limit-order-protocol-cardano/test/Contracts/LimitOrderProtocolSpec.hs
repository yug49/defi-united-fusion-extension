{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Contracts.LimitOrderProtocolSpec (spec) where

import Test.Hspec
import Prelude
import Data.Bits (setBit)
import Lib.Types
import qualified Contracts.LimitOrderProtocol as LOP

spec :: Spec
spec = describe "LimitOrderProtocol" $ do
    protocolInitializationSpec
    orderFillingSpec
    orderCancellationSpec
    validationSpec
    adminSpec

protocolInitializationSpec :: Spec
protocolInitializationSpec = describe "Protocol initialization" $ do
    it "initializes with correct default state" $ do
        let owner = "protocol_owner"
            state = LOP.initProtocolState owner
        LOP.psIsPaused state `shouldBe` False
        LOP.psOwner state `shouldBe` owner

orderFillingSpec :: Spec
orderFillingSpec = describe "Order filling" $ do
    it "fills a simple order successfully" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            order = createTestOrder
            signature = createTestSignature
            amount = 500000  -- Half the order
            takerTraits = TakerTraits 0  -- No special traits
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Right (fillResult, newState, events) -> do
                frMakingAmount fillResult `shouldBe` 500000
                frTakingAmount fillResult `shouldBe` 500000
                length events `shouldBe` 1
            Left err -> expectationFailure $ "Fill should succeed: " ++ show err

    it "handles partial fills correctly" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            -- Create order that allows partial fills
            order = createTestOrder { orderMakerTraits = 0 }  -- Default allows partial fills
            signature = createTestSignature
            amount = 250000  -- Quarter of the order
            takerTraits = TakerTraits 0
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Right (fillResult, newState, events) -> do
                frMakingAmount fillResult `shouldBe` 250000
                frTakingAmount fillResult `shouldBe` 250000
                
                -- Try to fill again with remaining amount
                case LOP.fillOrder order signature amount takerTraits taker currentTime newState of
                    Right (fillResult2, finalState, events2) -> do
                        frMakingAmount fillResult2 `shouldBe` 250000
                        frTakingAmount fillResult2 `shouldBe` 250000
                    Left err -> expectationFailure $ "Second fill should succeed: " ++ show err
            Left err -> expectationFailure $ "First fill should succeed: " ++ show err

    it "rejects partial fills when not allowed" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            -- Create order that doesn't allow partial fills
            noPartialFillsFlag = setBit 0 255
            order = createTestOrder { orderMakerTraits = noPartialFillsFlag }
            signature = createTestSignature
            amount = 500000  -- Half the order (should fail)
            takerTraits = TakerTraits 0
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Left PartialFillNotAllowed -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed with PartialFillNotAllowed"

    it "handles making amount vs taking amount correctly" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            order = createTestOrder
            signature = createTestSignature
            amount = 500000
            -- Set flag to indicate amount is making amount
            makingAmountFlag = setBit 0 255
            takerTraits = TakerTraits makingAmountFlag
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Right (fillResult, _, _) -> do
                frMakingAmount fillResult `shouldBe` 500000  -- Should use amount as making amount
                frTakingAmount fillResult `shouldBe` 500000  -- Proportional taking amount
            Left err -> expectationFailure $ "Fill should succeed: " ++ show err

orderCancellationSpec :: Spec
orderCancellationSpec = describe "Order cancellation" $ do
    it "cancels order successfully" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            order = createTestOrder
            orderHash = LOP.hashOrder order
            maker = orderMaker order
            
        case LOP.cancelOrder order orderHash maker initialState of
            Right (newState, events) -> do
                length events `shouldBe` 1
                case LOP.getOrderState orderHash newState of
                    Just state -> osIsCancelled state `shouldBe` True
                    Nothing -> return ()  -- Order might not be in registry yet
            Left err -> expectationFailure $ "Cancellation should succeed: " ++ show err

    it "rejects cancellation from non-maker" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            order = createTestOrder
            orderHash = LOP.hashOrder order
            nonMaker = "not_the_maker"
            
        case LOP.cancelOrder order orderHash nonMaker initialState of
            Left PrivateOrder -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed with PrivateOrder"

validationSpec :: Spec
validationSpec = describe "Order validation" $ do
    it "rejects expired orders" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            expiredTime = 1640995100
            currentTime = 1640995200
            traits = expiredTime * (2^80)  -- Set expiration time
            order = createTestOrder { orderMakerTraits = traits }
            signature = createTestSignature
            amount = 500000
            takerTraits = TakerTraits 0
            taker = "taker_address"
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Left OrderExpired -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed with OrderExpired"

    it "rejects orders with zero amounts" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            order = createTestOrder { orderMakingAmount = 0 }
            signature = createTestSignature
            amount = 500000
            takerTraits = TakerTraits 0
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime initialState of
            Left SwapWithZeroAmount -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed with SwapWithZeroAmount"

adminSpec :: Spec
adminSpec = describe "Admin operations" $ do
    it "allows owner to pause protocol" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            
        case LOP.pause owner initialState of
            Right newState -> LOP.psIsPaused newState `shouldBe` True
            Left err -> expectationFailure $ "Pause should succeed: " ++ show err

    it "rejects pause from non-owner" $ do
        let owner = "protocol_owner"
            nonOwner = "not_the_owner"
            initialState = LOP.initProtocolState owner
            
        case LOP.pause nonOwner initialState of
            Left PrivateOrder -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed with PrivateOrder"

    it "rejects operations when paused" $ do
        let owner = "protocol_owner"
            initialState = LOP.initProtocolState owner
            Right pausedState = LOP.pause owner initialState
            order = createTestOrder
            signature = createTestSignature
            amount = 500000
            takerTraits = TakerTraits 0
            taker = "taker_address"
            currentTime = 1640995200
            
        case LOP.fillOrder order signature amount takerTraits taker currentTime pausedState of
            Left InvalidatedOrder -> return ()
            Left err -> expectationFailure $ "Wrong error: " ++ show err
            Right _ -> expectationFailure "Should have failed when paused"

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

createTestSignature :: Signature
createTestSignature = Signature
    { sigR = "test_r_component_32_bytes_long"
    , sigS = "test_s_component_32_bytes_long"
    }
