module Main (main) where

import Test.Hspec
import qualified Lib.AmountCalculatorSpec

main :: IO ()
main = hspec $ do
    Lib.AmountCalculatorSpec.spec
