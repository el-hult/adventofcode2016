import Day2 as D2
import Data.Map ((!?))
import Test.HUnit

testDay2 :: Test
testDay2 =
  TestList
    [ TestLabel "testX" $ TestCase (assertEqual "Nice show" "(-1,0)" (show $ D2.move D2.U (0, 0))),
      TestLabel "testX" $ TestCase (assertEqual "Nice show" "Just 5" (show $ D2.numPadA !? (1, 1))),
      TestLabel "testX" $ TestCase (assertEqual "Nice show" "(0,0)" (show $ D2.moveOnPad D2.numPadA (0, 0) D2.U)),
      TestLabel "testX" $ TestCase (assertEqual "Nice show" "(1,0)" (show $ D2.moveOnPad D2.numPadA (0, 0) D2.D)),
      TestLabel "testX" $ TestCase (assertEqual "Nice show" "([5,1],(0,0))" (show $ D2.addNextButton D2.numPadA ([D2.Five], (1, 1)) [D2.U, D2.U, D2.U, D2.L, D2.L])),
      TestLabel "testX" $ TestCase (assertEqual "Nice show" "[2,1]" (show $ D2.addAllButtons D2.numPadA (1, 1) [[D2.U, D2.U, D2.U], [D2.L, D2.L]]))
    ]

main :: IO ()
main =  (runTestTT $ TestList [testDay2]) >>= print