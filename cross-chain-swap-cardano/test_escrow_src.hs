#!/usr/bin/env runhaskell

{-|
Module      : EscrowSrcTest  
Description : Test the EscrowSrc module functionality
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
import Contracts.EscrowSrc

-- Test Counter
data TestState = TestState { testsRun :: Int, testsPassed :: Int }
    deriving (Show)

-- Test Runner
runTest :: String -> Bool -> TestState -> IO TestState
runTest testName result state = do
    let newTestsRun = testsRun state + 1
    if result
        then do
            putStrLn $ "✅ " ++ testName
            return $ TestState newTestsRun (testsPassed state + 1)
        else do
            putStrLn $ "❌ " ++ testName
            return $ TestState newTestsRun (testsPassed state)

main :: IO ()
main = do
    putStrLn "🚀 ESCROWSRC FUNCTIONALITY TEST"
    putStrLn "=============================="
    putStrLn ""
    
    let initialState = TestState 0 0
    
    -- Create test registry
    let testRegistry = ResolverRegistry "registry_owner" (Map.fromList [("resolver1", True)])
    
    -- Test EscrowSrc creation
    let srcResult = mkEscrowSrc 
            "12345678901234567890123456789012"  -- 32 byte order hash
            "secret123"                         -- secret
            "maker_address"                     -- maker
            "taker_address"                     -- taker  
            1000000                             -- amount (1 ADA in lovelace)
            100000                              -- safety deposit
            500                                 -- rescue delay
            testRegistry
    
    state1 <- runTest "EscrowSrc: State creation" 
        (isRight srcResult) initialState
    
    let testSrcState = case srcResult of
            Right s -> s
            Left _ -> error "Failed to create test EscrowSrc state"
    
    state2 <- runTest "EscrowSrc: Is source chain flag"
        (isSourceChain testSrcState == True) state1
    
    state3 <- runTest "EscrowSrc: Base state initialization"
        (isActive (baseState testSrcState) == True) state2
    
    state4 <- runTest "EscrowSrc: Correct balance initialization"
        (currentBalance (baseState testSrcState) == 1100000) state3
    
    -- Test source withdrawal (valid case)
    let withdrawAction = SrcWithdraw "secret123" "taker_address"
    let withdrawResult = validateEscrowSrcAction testSrcState withdrawAction 1100
    
    state5 <- runTest "EscrowSrc: Valid withdrawal"
        (isRight withdrawResult) state4
    
    -- Test withdrawal with wrong secret
    let wrongSecretAction = SrcWithdraw "wrong_secret" "taker_address"
    state6 <- runTest "EscrowSrc: Invalid secret withdrawal"
        (isLeft $ validateEscrowSrcAction testSrcState wrongSecretAction 1100) state5
    
    -- Test withdrawal by non-taker
    let wrongCallerAction = SrcWithdraw "secret123" "wrong_caller"
    state7 <- runTest "EscrowSrc: Non-taker withdrawal"
        (isLeft $ validateEscrowSrcAction testSrcState wrongCallerAction 1100) state6
    
    -- Test withdrawal at wrong time (before window)
    state8 <- runTest "EscrowSrc: Early withdrawal"
        (isLeft $ validateEscrowSrcAction testSrcState withdrawAction 500) state7
    
    -- Test withdrawal at wrong time (after window)  
    state9 <- runTest "EscrowSrc: Late withdrawal"
        (isLeft $ validateEscrowSrcAction testSrcState withdrawAction 2500) state8
    
    -- Test withdrawTo functionality
    let withdrawToAction = SrcWithdrawTo "secret123" "taker_address" "recipient_address"
    state10 <- runTest "EscrowSrc: WithdrawTo valid"
        (isRight $ validateEscrowSrcAction testSrcState withdrawToAction 1100) state9
    
    -- Test withdrawTo with empty target
    let emptyTargetAction = SrcWithdrawTo "secret123" "taker_address" ""
    state11 <- runTest "EscrowSrc: WithdrawTo empty target"
        (isLeft $ validateEscrowSrcAction testSrcState emptyTargetAction 1100) state10
    
    -- Test cancellation (valid case)
    let cancelAction = SrcCancel "taker_address"
    state12 <- runTest "EscrowSrc: Valid cancellation"
        (isRight $ validateEscrowSrcAction testSrcState cancelAction 2100) state11
    
    -- Test early cancellation (should fail)
    state13 <- runTest "EscrowSrc: Early cancellation"
        (isLeft $ validateEscrowSrcAction testSrcState cancelAction 1500) state12
    
    -- Test public withdrawal by resolver
    let publicWithdrawAction = SrcPublicWithdraw "secret123" "resolver1" "recipient_address"
    state14 <- runTest "EscrowSrc: Public withdrawal by resolver"
        (isRight $ validateEscrowSrcAction testSrcState publicWithdrawAction 1600) state13
    
    -- Test public withdrawal by unauthorized resolver
    let unauthorizedAction = SrcPublicWithdraw "secret123" "unauthorized" "recipient_address"
    state15 <- runTest "EscrowSrc: Unauthorized public withdrawal"
        (isLeft $ validateEscrowSrcAction testSrcState unauthorizedAction 1600) state14
    
    -- Test public cancellation
    let publicCancelAction = SrcPublicCancel "resolver1" "refund_target"
    state16 <- runTest "EscrowSrc: Public cancellation"
        (isRight $ validateEscrowSrcAction testSrcState publicCancelAction 2300) state15
    
    -- Test state updates after successful withdrawal
    case withdrawResult of
        Right updatedState -> do
            state17 <- runTest "EscrowSrc: State deactivated after withdrawal"
                (not $ isActive $ baseState updatedState) state16
            state18 <- runTest "EscrowSrc: Balance updated after withdrawal"
                (currentBalance (baseState updatedState) == 0) state17
            
            -- Final results
            putStrLn ""
            putStrLn "=============================="
            putStrLn "📊 ESCROWSRC TEST RESULTS"
            putStrLn "=============================="
            
            let finalTestsRun = testsRun state18
            let finalTestsPassed = testsPassed state18
            
            putStrLn $ "Tests: " ++ show finalTestsPassed ++ "/" ++ show finalTestsRun
            
            if finalTestsPassed == finalTestsRun
                then do
                    putStrLn ""
                    putStrLn "🎉 ESCROWSRC WORKING PERFECTLY!"
                    putStrLn "✅ State creation and initialization"
                    putStrLn "✅ Source-specific timing validation"
                    putStrLn "✅ Secret and caller validation"
                    putStrLn "✅ Withdraw and WithdrawTo functionality"
                    putStrLn "✅ Cancellation mechanisms"
                    putStrLn "✅ Public operations via resolvers"
                    putStrLn "✅ State management and updates"
                    putStrLn ""
                    putStrLn "🚀 ESCROWSRC READY FOR PRODUCTION!"
                else do
                    putStrLn "❌ Some tests failed - check implementation"
            
            putStrLn "=============================="
            
        Left _ -> do
            putStrLn "❌ Failed to get updated state from withdrawal"
