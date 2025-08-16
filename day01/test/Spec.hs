import qualified Day1 as D1
import Control.Monad.State (execState)
import Test.HUnit

testDay1 :: Test
testDay1 =
  TestList
    [ TestLabel "test1" $ TestCase (assertEqual "Nice show" "Ins CW 1" (show $ D1.Ins D1.CW 1)),
      TestLabel "test2" $ TestCase (assertEqual "Nice show" "[Ins CW 7,Ins CCW 3,Ins CCW 1,Ins CW 7]" (show $ D1.stringToInstructions "R7, L3, L1, R7")),
      TestLabel "test3" $ TestCase (assertEqual "Nice show" "[(0,1),(0,0)]" (show $ D1.go1 D1.U [(0, 0)])),
      TestLabel "test4" $ TestCase (assertEqual "Nice show" "[(0,-1),(0,0)]" (show $ D1.go1 D1.D [(0, 0)])),
      TestLabel "test5" $ TestCase (assertEqual "Nice show" "[(1,0),(0,0)]" (show $ D1.go1 D1.R [(0, 0)])),
      TestLabel "test6" $ TestCase (assertEqual "Nice show" "[(-1,0),(0,0)]" (show $ D1.go1 D1.L [(0, 0)])),
      TestLabel "test7" $ TestCase (assertEqual "Nice show" "([(3,0),(2,0),(1,0),(0,0)],R)" (show $ execState (D1.goA (D1.Ins D1.CW 3)) ([(0, 0)], D1.U))),
      TestLabel "test8" $ TestCase (assertEqual "Nice show" "([(1,1),(1,0),(0,0)],U)" (show $ execState (mapM_ D1.goA [D1.Ins D1.CW 1, D1.Ins D1.CCW 1]) ([(0, 0)], D1.U))),
      TestLabel "test9" $ TestCase (assertEqual "Solve A correct" 5 (D1.taskA' "R2, L3")),
      TestLabel "test10" $ TestCase (assertEqual "Solve A correct" 2 (D1.taskA' "R2, R2, R2")),
      TestLabel "test11" $ TestCase (assertEqual "Solve A correct" 12 (D1.taskA' "R5, L5, R5, R3")),
      TestLabel "test12" $ TestCase (assertEqual "Solve B correct" 4 (D1.taskB' "R8, R4, R4, R8"))
    ]

main :: IO ()
main = do
  asdf <-
    runTestTT $
      TestList
        [ testDay1
        ]
  print asdf