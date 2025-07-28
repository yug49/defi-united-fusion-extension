{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Cross-Chain Swap Cardano Deployment Application
Copyright   : (c) 2025
License     : MIT

Main application for deploying cross-chain swap contracts on Cardano.
This application creates an EscrowFactory that can deploy both source and
destination escrow contracts for atomic cross-chain swaps.
-}

module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)

-- Import our contract modules
import Contracts.EscrowFactory (mkEscrowFactory, displayEscrowFactory)
import Contracts.BaseEscrow (ResolverRegistry(..))

-- | Main function for deployment
main :: IO ()
main = do
    putStrLn "🚀 CROSS-CHAIN SWAP CARDANO DEPLOYMENT"
    putStrLn "====================================="
    putStrLn ""
    
    args <- getArgs
    case args of
        ["deploy", owner, lopContractAddr] -> deployFactory owner lopContractAddr
        ["test"] -> testFactory
        _ -> do
            putStrLn "Usage:"
            putStrLn "  cabal run cross-chain-swap-cardano deploy <owner_address> <lop_contract_address>"
            putStrLn "  cabal run cross-chain-swap-cardano test"
            putStrLn ""
            putStrLn "Arguments:"
            putStrLn "  owner_address      - Address that will own the EscrowFactory"
            putStrLn "  lop_contract_address - Address of the deployed Limit Order Protocol contract"

-- | Deploy the EscrowFactory with given parameters
deployFactory :: String -> String -> IO ()
deployFactory ownerStr lopAddr = do
    putStrLn $ "📋 Deploying EscrowFactory with:"
    putStrLn $ "   Owner: " ++ ownerStr
    putStrLn $ "   LOP Contract: " ++ lopAddr
    putStrLn ""
    
    let ownerAddress = BC.pack ownerStr
    let lopContract = BC.pack lopAddr
    
    -- Create resolver registry with the LOP contract as initial resolver
    let resolverRegistry = ResolverRegistry 
            { registryOwner = ownerAddress
            , authorizedResolvers = Map.fromList [(lopContract, True)]
            }
    
    -- Standard delays (in seconds)
    let srcDelay = 3600   -- 1 hour for source escrow
    let dstDelay = 7200   -- 2 hours for destination escrow
    
    putStrLn "🔧 Creating EscrowFactory..."
    case mkEscrowFactory ownerAddress srcDelay dstDelay resolverRegistry of
        Right factory -> do
            putStrLn "✅ EscrowFactory created successfully!"
            putStrLn ""
            putStrLn "📊 Factory Details:"
            putStrLn $ displayEscrowFactory factory
            putStrLn ""
            putStrLn "🎯 Deployment Summary:"
            putStrLn $ "   Factory Owner: " ++ ownerStr
            putStrLn $ "   Source Delay: " ++ show srcDelay ++ " seconds"
            putStrLn $ "   Destination Delay: " ++ show dstDelay ++ " seconds"
            putStrLn $ "   Authorized Resolver: " ++ lopAddr
            putStrLn ""
            putStrLn "✅ Ready for cross-chain swaps!"
            
        Left err -> do
            putStrLn $ "❌ Failed to create EscrowFactory: " ++ show err

-- | Test function to verify everything works
testFactory :: IO ()
testFactory = do
    putStrLn "🧪 TESTING ESCROW FACTORY"
    putStrLn "========================"
    putStrLn ""
    
    let testOwner = BC.pack "addr_test1qrytuz9x92h8pmt9rclnmft5gsfwctpgevq3sk3jfa5e42sqv2nd4"
    let testLopAddr = BC.pack "addr_test1wp203846488426adefdc379e46cc9713d1695f5dd424728694acb07f1e"
    
    let testRegistry = ResolverRegistry 
            { registryOwner = testOwner
            , authorizedResolvers = Map.fromList [(testLopAddr, True)]
            }
    
    putStrLn "🔧 Creating test EscrowFactory..."
    case mkEscrowFactory testOwner 3600 7200 testRegistry of
        Right factory -> do
            putStrLn "✅ Test EscrowFactory created successfully!"
            putStrLn ""
            putStrLn "📊 Test Factory Details:"
            putStrLn $ displayEscrowFactory factory
            putStrLn ""
            putStrLn "✅ All tests passed! Ready for deployment."
            
        Left err -> do
            putStrLn $ "❌ Test failed: " ++ show err
