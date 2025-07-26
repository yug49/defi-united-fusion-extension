{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Test.Lib.TimelocksLibSpec  
Description : Unit tests for TimelocksLib
Copyright   : (c) 2025
License     : MIT

Comprehensive test suite for the TimelocksLib module.
Tests all functionality to ensure it behaves exactly like the original Solidity version.
-}

module Test.Lib.TimelocksLibSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Lib.TimelocksLib

-- | Test data representing a typical escrow scenario
-- Deployment time: January 1, 2025 00:00:00 UTC (1735689600)
testDeploymentTime :: Integer
testDeploymentTime = 1735689600

-- | Example timelock periods (in seconds from deployment)
-- These represent realistic timelock periods for a cross-chain swap
testSrcWithdrawal, testSrcPublicWithdrawal, testSrcCancellation, testSrcPublicCancellation :: Integer
testDstWithdrawal, testDstPublicWithdrawal, testDstCancellation :: Integer
testSrcWithdrawal = 3600        -- 1 hour: Taker can withdraw on source 
testSrcPublicWithdrawal = 7200   -- 2 hours: Anyone can withdraw on source
testSrcCancellation = 10800      -- 3 hours: Taker can cancel
testSrcPublicCancellation = 14400 -- 4 hours: Anyone can cancel
testDstWithdrawal = 1800         -- 30 minutes: Maker can withdraw on destination
testDstPublicWithdrawal = 5400   -- 1.5 hours: Anyone can withdraw on destination  
testDstCancellation = 9000       -- 2.5 hours: Destination can be cancelled

-- | Create a test timelock configuration
testTimelocks :: Timelocks
testTimelocks = mkTimelocks 
    testDeploymentTime
    testSrcWithdrawal testSrcPublicWithdrawal testSrcCancellation testSrcPublicCancellation
    testDstWithdrawal testDstPublicWithdrawal testDstCancellation

-- | All tests for the TimelocksLib module
timelocksLibTests :: TestTree
timelocksLibTests = testGroup "TimelocksLib Tests"
    [ testGroup "Construction Tests" constructionTests
    , testGroup "Basic Functionality Tests" basicFunctionalityTests  
    , testGroup "Stage Timing Tests" stageTimingTests
    , testGroup "Edge Case Tests" edgeCaseTests
    , testGroup "Equivalence Tests" equivalenceTests
    ]

-- | Tests for creating and modifying Timelocks structures
constructionTests :: [TestTree]
constructionTests = 
    [ testCase "mkTimelocks creates correct structure" $ do
        let tl = testTimelocks
        deployedAt tl @?= testDeploymentTime
        srcWithdrawal (srcTimelocks tl) @?= testSrcWithdrawal
        dstCancellation (dstTimelocks tl) @?= testDstCancellation
        
    , testCase "setDeployedAt updates deployment time" $ do
        let newTime = testDeploymentTime + 1000
            updatedTl = setDeployedAt testTimelocks newTime
        deployedAt updatedTl @?= newTime
        -- Verify other fields remain unchanged
        srcWithdrawal (srcTimelocks updatedTl) @?= testSrcWithdrawal
        dstWithdrawal (dstTimelocks updatedTl) @?= testDstWithdrawal
    ]

-- | Tests for basic timelock operations
basicFunctionalityTests :: [TestTree]  
basicFunctionalityTests =
    [ testCase "getTimelock returns correct absolute timestamps" $ do
        -- Test all stages return correct absolute times (deployment + offset)
        getTimelock testTimelocks SrcWithdrawal @?= testDeploymentTime + testSrcWithdrawal
        getTimelock testTimelocks SrcPublicWithdrawal @?= testDeploymentTime + testSrcPublicWithdrawal
        getTimelock testTimelocks SrcCancellation @?= testDeploymentTime + testSrcCancellation
        getTimelock testTimelocks SrcPublicCancellation @?= testDeploymentTime + testSrcPublicCancellation
        getTimelock testTimelocks DstWithdrawal @?= testDeploymentTime + testDstWithdrawal
        getTimelock testTimelocks DstPublicWithdrawal @?= testDeploymentTime + testDstPublicWithdrawal
        getTimelock testTimelocks DstCancellation @?= testDeploymentTime + testDstCancellation
        
    , testCase "rescueStart calculates correctly" $ do
        let rescueDelay = 86400  -- 24 hours
            expectedRescueStart = testDeploymentTime + rescueDelay
        rescueStart testTimelocks rescueDelay @?= expectedRescueStart
        
    , testCase "addSeconds utility works correctly" $ do
        let baseTime = 1000
            seconds = 500
            expected = 1500
        addSeconds baseTime seconds @?= expected
    ]

-- | Tests for stage activation timing
stageTimingTests :: [TestTree]
stageTimingTests =
    [ testCase "isStageActive works correctly" $ do
        -- Before any stage is active
        let beforeAll = testDeploymentTime - 1
        isStageActive testTimelocks SrcWithdrawal beforeAll @?= False
        isStageActive testTimelocks DstWithdrawal beforeAll @?= False
        
        -- When SrcWithdrawal is active but others aren't
        let duringFirst = testDeploymentTime + testSrcWithdrawal
        isStageActive testTimelocks SrcWithdrawal duringFirst @?= True
        isStageActive testTimelocks SrcPublicWithdrawal duringFirst @?= False
        
        -- When multiple stages are active
        let duringLater = testDeploymentTime + testSrcPublicWithdrawal  
        isStageActive testTimelocks SrcWithdrawal duringLater @?= True
        isStageActive testTimelocks SrcPublicWithdrawal duringLater @?= True
        isStageActive testTimelocks SrcCancellation duringLater @?= False
        
    , testCase "getCurrentStage returns correct active stage" $ do
        -- No stage active yet
        let beforeAll = testDeploymentTime - 1
        getCurrentStage testTimelocks beforeAll @?= Nothing
        
        -- Only first stage active
        let duringFirst = testDeploymentTime + testDstWithdrawal  -- Earliest stage
        getCurrentStage testTimelocks duringFirst @?= Just DstWithdrawal
        
        -- Multiple stages active - should return most advanced
        let duringMultiple = testDeploymentTime + testSrcPublicWithdrawal
        getCurrentStage testTimelocks duringMultiple @?= Just SrcPublicWithdrawal
        
        -- All stages active - should return latest  
        let afterAll = testDeploymentTime + testSrcPublicCancellation
        getCurrentStage testTimelocks afterAll @?= Just SrcPublicCancellation
    ]

-- | Tests for edge cases and boundary conditions
edgeCaseTests :: [TestTree]
edgeCaseTests =
    [ testCase "zero deployment time works" $ do
        let zeroTimeTl = mkTimelocks 0 100 200 300 400 50 150 250
        deployedAt zeroTimeTl @?= 0
        getTimelock zeroTimeTl SrcWithdrawal @?= 100
        
    , testCase "zero timelock periods work" $ do  
        let zeroOffsetTl = mkTimelocks testDeploymentTime 0 0 0 0 0 0 0
        getTimelock zeroOffsetTl SrcWithdrawal @?= testDeploymentTime
        getTimelock zeroOffsetTl DstCancellation @?= testDeploymentTime
        
    , testCase "large numbers work correctly" $ do
        let largeTime = 2147483647  -- Max 32-bit signed int
            largeTl = mkTimelocks largeTime largeTime 0 0 0 0 0 0
        deployedAt largeTl @?= largeTime
        getTimelock largeTl SrcWithdrawal @?= largeTime + largeTime
        
    , testCase "same timelock values work" $ do
        let sameTl = mkTimelocks testDeploymentTime 1000 1000 1000 1000 1000 1000 1000
        getTimelock sameTl SrcWithdrawal @?= getTimelock sameTl DstCancellation
        
    , testCase "rescue with zero delay" $ do
        rescueStart testTimelocks 0 @?= testDeploymentTime
    ]

-- | Tests to ensure our Haskell version behaves exactly like the Solidity version
equivalenceTests :: [TestTree]
equivalenceTests =
    [ testCase "Stage ordering matches Solidity enum" $ do
        -- In Solidity, enum values are assigned 0, 1, 2, etc.
        -- Our Haskell Enum instance should match this exactly
        fromEnum SrcWithdrawal @?= 0
        fromEnum SrcPublicWithdrawal @?= 1  
        fromEnum SrcCancellation @?= 2
        fromEnum SrcPublicCancellation @?= 3
        fromEnum DstWithdrawal @?= 4
        fromEnum DstPublicWithdrawal @?= 5
        fromEnum DstCancellation @?= 6
        
    , testCase "Timelock calculations match Solidity behavior" $ do
        -- In Solidity: deployedAt + uint32(data >> bitShift)
        -- Our version: deployedAt + stageOffset
        -- These should produce identical results for same inputs
        let solidityLikeResult offset = testDeploymentTime + offset
        getTimelock testTimelocks SrcWithdrawal @?= solidityLikeResult testSrcWithdrawal
        getTimelock testTimelocks DstCancellation @?= solidityLikeResult testDstCancellation
        
    , testCase "setDeployedAt behaves like Solidity version" $ do
        -- Solidity version updates only the deployment timestamp, keeps other data
        let newTime = testDeploymentTime + 5000
            updated = setDeployedAt testTimelocks newTime
        deployedAt updated @?= newTime
        -- All relative offsets should remain the same
        (getTimelock updated SrcWithdrawal - newTime) @?= testSrcWithdrawal
        (getTimelock updated DstCancellation - newTime) @?= testDstCancellation
        
    , testCase "rescueStart matches Solidity unchecked addition" $ do
        -- Solidity: rescueDelay + (timelocks >> _DEPLOYED_AT_OFFSET)  
        -- Our version: rescueDelay + deployedAt
        let delay = 12345
            expected = delay + testDeploymentTime  -- Same as Solidity unchecked math
        rescueStart testTimelocks delay @?= expected
    ]

-- | Test runner for this module
-- This can be called from the main test suite  
runTimelocksLibTests :: IO ()
runTimelocksLibTests = defaultMain timelocksLibTests
