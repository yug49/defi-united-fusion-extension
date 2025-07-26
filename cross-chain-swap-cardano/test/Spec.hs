import Test.Tasty
import qualified Test.Lib.TimelocksLibSpec as TimelocksLib
import qualified Test.Contracts.BaseEscrowSpec as BaseEscrow

main :: IO ()
main = defaultMain $ testGroup "All Tests"
    [ TimelocksLib.timelocksLibTests
    , BaseEscrow.baseEscrowTests
    ]
