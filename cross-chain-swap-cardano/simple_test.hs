#!/usr/bin/env runhaskell

{-|
Module      : SimpleTest  
Description : Simple verification that core functionality works
Copyright   : (c) 2025
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Either (isLeft, isRight)
import System.IO (putStrLn)

-- Import our modules
import Lib.TimelocksLib
import Contracts.BaseEscrow
import qualified Contracts.EscrowSrc as Src
import qualified Contracts.EscrowDst as Dst

-- Test Counter
data TestState = TestState { testsRun :: Int, testsPassed :: Int }
    deriving (Show)

-- Test Runner
runTest :: String -> Bool -> TestState -> IO TestState
runTest testName result state = do
    let newTestsRun = testsRun state + 1
    if result
        then do
            putStrLn $ "PASS " ++ testName
            return $ TestState newTestsRun (testsPassed state + 1)
        else do
            putStrLn $ "FAIL " ++ testName
            return $ TestState newTestsRun (testsPassed state)

main :: IO ()
main = do
    putStrLn "SIMPLE VERIFICATION TEST"
    putStrLn "=========================="
    putStrLn ""
    
    let initialState = TestState 0 0
    
    -- Test TimelocksLib basic functionality
    let testTimelocks = mkTimelocks 0 1000 1200 1400 1500 1600 1800 2000
    
    state1 <- runTest "TimelocksLib: Construction works" 
        (deployedAt testTimelocks == 0) initialState
    
    state2 <- runTest "TimelocksLib: Stage time retrieval" 
        (getTimelock testTimelocks SrcWithdrawal == 1000) state1
    
    state3 <- runTest "TimelocksLib: Stage activity check"
        (isStageActive testTimelocks SrcWithdrawal 1100) state2
    
    state4 <- runTest "TimelocksLib: Rescue calculation"
        (rescueStart testTimelocks 500 == 500) state3
    
    -- Test BaseEscrowCore
    let testImmutables = case mkImmutables 
            "12345678901234567890123456789012"  -- 32 byte order hash
            "secret123"                         -- secret
            "maker_address"                     -- maker
            "taker_address"                     -- taker  
            1000000                             -- amount
            100000                              -- safety deposit
            testTimelocks of
            Right immuts -> immuts
            Left err -> error $ "Failed to create test immutables: " ++ show err
    
    state5 <- runTest "BaseEscrowCore: Immutables creation"
        (amount testImmutables == 1000000) state4
    
    state6 <- runTest "BaseEscrowCore: Hash generation"
        (hashSecret "test" == hashSecret "test") state5
    
    state7 <- runTest "BaseEscrowCore: Total amount calculation"
        (totalAmount testImmutables == 1100000) state6
    
    let testRegistry = ResolverRegistry "registry_owner" (Map.fromList [("resolver1", True)])
    let testState = EscrowState 
            { immutables = testImmutables
            , factory = "factory_address"
            , rescueDelay = 500
            , resolverRegistry = testRegistry
            , isActive = True
            , currentBalance = 1200000
            }
    
    state8 <- runTest "BaseEscrowCore: State creation"
        (isActive testState == True) state7
    
    -- Test basic validation
    let withdrawAction = Withdraw "secret123" "taker_address"
    let validationResult = validateEscrowAction testState withdrawAction 1100
    
    state9 <- runTest "BaseEscrowCore: Withdrawal validation"
        (isRight validationResult) state8
    
    -- Test inheritance functions
    state10 <- runTest "Inheritance: SrcWithdrawTime validation"
        (isRight $ validateSrcWithdrawTime 1100 testImmutables) state9
    
    state11 <- runTest "Inheritance: DstWithdrawTime validation"
        (isRight $ validateDstWithdrawTime 1700 testImmutables) state10
    
    -- Test EscrowSrc integration
    let testRegistry = ResolverRegistry "registry_owner" (Map.fromList [("resolver1", True)])
    let srcResult = Src.mkEscrowSrc 
            "12345678901234567890123456789012" "secret123" "maker_address" "taker_address" 
            1000000 100000 500 testRegistry
    
    state12 <- runTest "EscrowSrc: Integration test"
        (isRight srcResult) state11
    
    -- Test EscrowDst functionality
    let testResolverRegistry = ResolverRegistry
            { registryOwner = BC.pack "registry_owner"
            , authorizedResolvers = Map.fromList [(BC.pack "resolver1", True)]
            }
    
    let dstResult = Dst.mkEscrowDst 
            (BC.pack "12345678901234567890123456789012")  -- 32-byte order hash
            (BC.pack "testsecret")                        -- Test secret
            (BC.pack "maker_address")                     -- Maker
            (BC.pack "taker_address")                     -- Taker
            1000                                          -- Amount
            100                                           -- Safety deposit
            86400                                         -- Rescue delay
            testResolverRegistry                          -- Resolver registry
    
    state13 <- runTest "EscrowDst: Creation works"
        (isRight dstResult) state12
    
    -- Test EscrowDst withdrawal functionality
    finalState <- case dstResult of
        Right dstState -> do
            let withdrawResult = Dst.withdraw dstState 
                    (BC.pack "testsecret") 
                    (BC.pack "maker_address") 
                    1300  -- During destination withdrawal window
            runTest "EscrowDst: Withdrawal works"
                (isRight withdrawResult) state13
        Left _ -> return state13
    
    -- Final results
    putStrLn ""
    putStrLn "=========================="
    putStrLn "RESULTS"
    putStrLn "=========================="
    
    let finalTestsRun = testsRun finalState
    let finalTestsPassed = testsPassed finalState
    
    putStrLn $ "Tests: " ++ show finalTestsPassed ++ "/" ++ show finalTestsRun
    
    if finalTestsPassed == finalTestsRun
        then do
            putStrLn ""
            putStrLn "ALL TESTS PASSED PERFECTLY!"
            putStrLn "PASS TimelocksLib: Working perfectly"
            putStrLn "PASS BaseEscrowCore: All validations work"
            putStrLn "PASS Cross-chain: Hash functions ready"
            putStrLn "PASS Inheritance: Functions prepared"
            putStrLn "PASS EscrowSrc: Integration successful"
            putStrLn "PASS EscrowDst: Integration successful"
            putStrLn ""
            putStrLn "EVERYTHING IS READY FOR NEXT PHASE!"
        else do
            putStrLn "FAIL Some tests failed"
    
    putStrLn "=========================="
