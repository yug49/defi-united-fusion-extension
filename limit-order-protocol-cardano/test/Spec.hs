module Main (main) where

import Test.Hspec
import qualified Lib.AmountCalculatorSpec
import qualified Lib.TypesSpec
import qualified Lib.MakerTraitsSpec
import qualified Lib.TakerTraitsSpec
import qualified Lib.OrderLibSpec
import qualified Contracts.LimitOrderProtocolSpec

main :: IO ()
main = hspec $ do
    Lib.AmountCalculatorSpec.spec
    Lib.TypesSpec.spec
    Lib.MakerTraitsSpec.spec
    Lib.TakerTraitsSpec.spec
    Lib.OrderLibSpec.spec
    Contracts.LimitOrderProtocolSpec.spec
