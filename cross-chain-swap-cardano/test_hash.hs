#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.SHA256 as SHA256
import Data.ByteString.Base16 as Base16

-- Simple SHA-256 hash function for testing
hashSecret :: BS.ByteString -> BS.ByteString
hashSecret secret = 
    let hash = SHA256.hash secret
    in Base16.encode hash

main :: IO ()
main = do
    let testSecret = "test123"
    let hash1 = hashSecret testSecret
    let hash2 = hashSecret testSecret
    
    putStrLn $ "Secret: " ++ BS.unpack testSecret
    putStrLn $ "Hash 1: " ++ BS.unpack hash1
    putStrLn $ "Hash 2: " ++ BS.unpack hash2
    putStrLn $ "Consistent: " ++ show (hash1 == hash2)
    putStrLn $ "Hash length: " ++ show (BS.length hash1)
    
    -- Test with different secrets
    let differentSecret = "different123"
    let hash3 = hashSecret differentSecret
    putStrLn $ "Different secret hash: " ++ BS.unpack hash3
    putStrLn $ "Different hashes: " ++ show (hash1 /= hash3)
