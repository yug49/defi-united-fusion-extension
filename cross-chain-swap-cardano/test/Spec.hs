import Test.Tasty
import qualified Test.Lib.TimelocksLibSpec as TimelocksLib

main :: IO ()
main = defaultMain $ testGroup "All Tests"
    [ TimelocksLib.timelocksLibTests
    ]
