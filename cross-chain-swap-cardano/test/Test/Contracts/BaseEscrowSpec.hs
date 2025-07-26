{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Contracts.BaseEscrowSpec
Description : Unit tests for BaseEscrow contract
Copyright   : (c) 2025
License     : MIT

Comprehensive test suite for the BaseEscrow contract functionality.
Tests all escrow operations, validation logic, and edge cases.
-}

module Test.Contracts.BaseEscrowSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Either (isLeft, isRight)

import Contracts.BaseEscrow
import Lib.TimelocksLib (mkTimelocks)

-- | Test data for a typical escrow scenario
testOrderHash :: ByteString
testOrderHash = BC.replicate 32 'a'  -- 32-byte order hash

testSecret :: ByteString  
testSecret = "test_secret_123"

testMakerAddress :: ByteString
testMakerAddress = "addr_maker_12345"

testTakerAddress :: ByteString
testTakerAddress = "addr_taker_67890"

testResolverAddress :: ByteString
testResolverAddress = "addr_resolver_abc"

testOwnerAddress :: ByteString
testOwnerAddress = "addr_owner_xyz"

testAmount :: Integer
testAmount = 1000000  -- 1 ADA in lovelace

testSafetyDeposit :: Integer
testSafetyDeposit = 100000  -- 0.1 ADA in lovelace

testRescueDelay :: Integer
testRescueDelay = 86400  -- 24 hours

testCurrentTime :: Integer
testCurrentTime = 1735689600  -- January 1, 2025

-- | Create test timelocks with reasonable timing
testTimelocks = mkTimelocks 
    testCurrentTime
    3600      -- 1 hour: taker withdrawal
    7200      -- 2 hours: public withdrawal  
    10800     -- 3 hours: taker cancellation
    14400     -- 4 hours: public cancellation
    1800      -- 30 min: dst withdrawal
    5400      -- 1.5 hours: dst public withdrawal
    9000      -- 2.5 hours: dst cancellation

-- | Create test resolver registry
testResolverRegistry :: ResolverRegistry
testResolverRegistry = ResolverRegistry
    { registryOwner = testOwnerAddress
    , authorizedResolvers = Map.fromList [(testResolverAddress, True)]
    }

-- | Create test immutables
testImmutables :: Either BaseEscrowError Immutables
testImmutables = mkImmutables 
    testOrderHash 
    testSecret 
    testMakerAddress 
    testTakerAddress 
    testAmount 
    testSafetyDeposit 
    testTimelocks

-- | Create test escrow state
testEscrowState :: EscrowState
testEscrowState = case testImmutables of
    Right immuts -> EscrowState
        { immutables = immuts
        , factory = "factory_address"
        , rescueDelay = testRescueDelay
        , resolverRegistry = testResolverRegistry
        , isActive = True
        , currentBalance = testAmount + testSafetyDeposit
        }
    Left _ -> error "Failed to create test immutables"

-- | All tests for the BaseEscrow contract
baseEscrowTests :: TestTree
baseEscrowTests = testGroup "BaseEscrow Tests"
    [ testGroup "Immutables Construction Tests" immutablesTests
    , testGroup "Validation Tests" validationTests
    , testGroup "Escrow Action Tests" escrowActionTests
    , testGroup "Resolver Registry Tests" resolverRegistryTests
    , testGroup "Timing Tests" timingTests
    , testGroup "Error Handling Tests" errorHandlingTests
    ]

-- | Tests for Immutables creation and validation
immutablesTests :: [TestTree]
immutablesTests =
    [ testCase "mkImmutables creates valid structure" $ do
        case testImmutables of
            Right immuts -> do
                orderHash immuts @?= testOrderHash
                maker immuts @?= testMakerAddress
                taker immuts @?= testTakerAddress
                amount immuts @?= testAmount
                safetyDeposit immuts @?= testSafetyDeposit
                -- Verify hashlock is hash of secret
                hashlock immuts @?= hashSecret testSecret
            Left err -> assertFailure $ "Failed to create immutables: " ++ show err
            
    , testCase "mkImmutables rejects invalid order hash" $ do
        let result = mkImmutables 
                "short_hash"  -- Too short
                testSecret testMakerAddress testTakerAddress 
                testAmount testSafetyDeposit testTimelocks
        result `shouldSatisfy` isLeft
        
    , testCase "mkImmutables rejects zero amount" $ do
        let result = mkImmutables 
                testOrderHash testSecret testMakerAddress testTakerAddress 
                0  -- Zero amount
                testSafetyDeposit testTimelocks
        result `shouldSatisfy` isLeft
        
    , testCase "mkImmutables rejects negative safety deposit" $ do
        let result = mkImmutables 
                testOrderHash testSecret testMakerAddress testTakerAddress 
                testAmount 
                (-100)  -- Negative deposit
                testTimelocks
        result `shouldSatisfy` isLeft
    ]

-- | Tests for validation functions
validationTests :: [TestTree]
validationTests =
    [ testCase "validateTaker accepts correct taker" $ do
        case testImmutables of
            Right immuts -> do
                let result = validateTaker testTakerAddress immuts
                result @?= Right ()
            Left _ -> assertFailure "Failed to create test immutables"
            
    , testCase "validateTaker rejects wrong caller" $ do
        case testImmutables of
            Right immuts -> do
                let result = validateTaker "wrong_address" immuts
                result `shouldSatisfy` isLeft
            Left _ -> assertFailure "Failed to create test immutables"
            
    , testCase "validateSecret accepts correct secret" $ do
        case testImmutables of
            Right immuts -> do
                let result = validateSecret testSecret immuts
                result @?= Right ()
            Left _ -> assertFailure "Failed to create test immutables"
            
    , testCase "validateSecret rejects wrong secret" $ do
        case testImmutables of
            Right immuts -> do
                let result = validateSecret "wrong_secret" immuts
                result `shouldSatisfy` isLeft
            Left _ -> assertFailure "Failed to create test immutables"
            
    , testCase "validateResolver accepts authorized resolver" $ do
        let result = validateResolver testResolverAddress testResolverRegistry
        result @?= Right ()
        
    , testCase "validateResolver rejects unauthorized address" $ do
        let result = validateResolver "unauthorized_address" testResolverRegistry
        result `shouldSatisfy` isLeft
    ]

-- | Tests for escrow actions
escrowActionTests :: [TestTree]
escrowActionTests =
    [ testCase "Withdraw action succeeds with correct parameters" $ do
        let action = Withdraw testSecret testTakerAddress
            -- Set time during withdrawal window
            withdrawTime = testCurrentTime + 3600
        case validateEscrowAction testEscrowState action withdrawTime of
            Right newState -> do
                isActive newState @?= False  -- Should be completed
                currentBalance newState @?= testSafetyDeposit  -- Only safety deposit left
            Left err -> assertFailure $ "Withdraw failed: " ++ show err
            
    , testCase "Cancel action succeeds with correct parameters" $ do
        let action = Cancel testTakerAddress
            -- Set time during cancellation window  
            cancelTime = testCurrentTime + 10800
        case validateEscrowAction testEscrowState action cancelTime of
            Right newState -> do
                isActive newState @?= False  -- Should be completed
                currentBalance newState @?= 0  -- All funds returned
            Left err -> assertFailure $ "Cancel failed: " ++ show err
            
    , testCase "PublicWithdraw succeeds with authorized resolver" $ do
        let action = PublicWithdraw testSecret testResolverAddress
            -- Set time during public withdrawal window
            publicWithdrawTime = testCurrentTime + 7200
        case validateEscrowAction testEscrowState action publicWithdrawTime of
            Right newState -> do
                isActive newState @?= False  -- Should be completed
                currentBalance newState @?= testSafetyDeposit  -- Only safety deposit left
            Left err -> assertFailure $ "Public withdraw failed: " ++ show err
            
    , testCase "RescueFunds succeeds after rescue delay" $ do
        let action = RescueFunds 50000 testTakerAddress  -- Rescue 50K lovelace
            -- Set time after rescue delay
            rescueTime = testCurrentTime + testRescueDelay + 1
        case validateEscrowAction testEscrowState action rescueTime of
            Right newState -> do
                isActive newState @?= True  -- Should still be active
                currentBalance newState @?= (testAmount + testSafetyDeposit - 50000)
            Left err -> assertFailure $ "Rescue failed: " ++ show err
    ]

-- | Tests for resolver registry operations
resolverRegistryTests :: [TestTree]
resolverRegistryTests =
    [ testCase "AddResolver succeeds with owner" $ do
        let newResolver = "new_resolver_123"
            action = AddResolver newResolver testOwnerAddress
        case validateEscrowAction testEscrowState action testCurrentTime of
            Right newState -> do
                let registry = resolverRegistry newState
                    resolvers = authorizedResolvers registry
                Map.lookup newResolver resolvers @?= Just True
            Left err -> assertFailure $ "Add resolver failed: " ++ show err
            
    , testCase "RemoveResolver succeeds with owner" $ do
        let action = RemoveResolver testResolverAddress testOwnerAddress
        case validateEscrowAction testEscrowState action testCurrentTime of
            Right newState -> do
                let registry = resolverRegistry newState
                    resolvers = authorizedResolvers registry
                Map.lookup testResolverAddress resolvers @?= Nothing
            Left err -> assertFailure $ "Remove resolver failed: " ++ show err
            
    , testCase "AddResolver fails with non-owner" $ do
        let action = AddResolver "new_resolver" "not_owner"
        let result = validateEscrowAction testEscrowState action testCurrentTime
        result `shouldSatisfy` isLeft
    ]

-- | Tests for timing constraints
timingTests :: [TestTree]
timingTests =
    [ testCase "Withdraw fails outside time window" $ do
        let action = Withdraw testSecret testTakerAddress
            -- Time before withdrawal window
            earlyTime = testCurrentTime + 1800  -- 30 minutes
        let result = validateEscrowAction testEscrowState action earlyTime
        result `shouldSatisfy` isLeft
        
    , testCase "Cancel fails outside time window" $ do
        let action = Cancel testTakerAddress
            -- Time before cancellation window
            earlyTime = testCurrentTime + 1800  -- 30 minutes
        let result = validateEscrowAction testEscrowState action earlyTime
        result `shouldSatisfy` isLeft
        
    , testCase "RescueFunds fails before rescue delay" $ do
        let action = RescueFunds 50000 testTakerAddress
            -- Time before rescue delay
            earlyTime = testCurrentTime + 1000
        let result = validateEscrowAction testEscrowState action earlyTime
        result `shouldSatisfy` isLeft
    ]

-- | Tests for error handling and edge cases
errorHandlingTests :: [TestTree]
errorHandlingTests =
    [ testCase "Actions fail on inactive escrow" $ do
        let inactiveState = testEscrowState { isActive = False }
            action = Withdraw testSecret testTakerAddress
            withdrawTime = testCurrentTime + 3600
        let result = validateEscrowAction inactiveState action withdrawTime
        result `shouldSatisfy` isLeft
        
    , testCase "Transfer fails with insufficient funds" $ do
        let lowBalanceState = testEscrowState { currentBalance = 100 }  -- Very low balance
            action = Withdraw testSecret testTakerAddress
            withdrawTime = testCurrentTime + 3600
        let result = validateEscrowAction lowBalanceState action withdrawTime
        result `shouldSatisfy` isLeft
        
    , testCase "transferAda rejects zero amount" $ do
        let result = transferAda 0 testMakerAddress
        result `shouldSatisfy` isLeft
        
    , testCase "transferAda rejects empty recipient" $ do
        let result = transferAda testAmount ""
        result `shouldSatisfy` isLeft
        
    , testCase "hashSecret produces consistent results" $ do
        let hash1 = hashSecret testSecret
            hash2 = hashSecret testSecret
        hash1 @?= hash2
        
    , testCase "hashSecret produces different hashes for different secrets" $ do
        let hash1 = hashSecret "secret1"
            hash2 = hashSecret "secret2"
        hash1 /= hash2 @? "Different secrets should produce different hashes"
    ]

-- | Helper function to check if Either is Left
shouldSatisfy :: (Show a, Show b) => Either a b -> (Either a b -> Bool) -> Assertion
shouldSatisfy actual predicate = 
    if predicate actual
    then return ()
    else assertFailure $ "Predicate failed for: " ++ show actual

-- | Test runner for this module
runBaseEscrowTests :: IO ()
runBaseEscrowTests = defaultMain baseEscrowTests
