import Test.HUnit
import qualified Day9 as D9

testDay9 :: Test
testDay9 =
  TestLabel "Day 9" $
    TestList
      [ TestCase $ assertEqual "Simple Parse" ("abc", 3, 7, "ghi") $ D9.parseStep "abc(3x7)ghi",
        TestCase $ assertEqual "Double Parse Gets one" ("abc", 3, 7, "ghi(9x1)oo") $ D9.parseStep "abc(3x7)ghi(9x1)oo",
        TestCase $ assertEqual "Failing Parse" ("abcghi", -1, 0, "") $ D9.parseStep "abcghi"
      ]

main :: IO ()
main = runTestTT testDay9 >>= print