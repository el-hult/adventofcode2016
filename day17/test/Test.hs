import Day17
import Test.HUnit hiding (State)

initialStateTest0 :: State
initialStateTest0 = makeState2 "hijkl"

initialStateTest1 :: State
initialStateTest1 = makeState2 "ihgpwlah"

initialStateTest2 :: State
initialStateTest2 = makeState2 "kglvqrro"

initialStateTest3 :: State
initialStateTest3 = makeState2 "ulqzkmiv"


testDay17 :: Test
testDay17 =
  TestList $
    map
      TestCase
      [ [D] @=? doors initialStateTest0,
        [makeState (1, 2) "hijklD" 5 1] @=? getNeighbors initialStateTest0,
        [U, R] @=? doors (makeState (1, 2) "hijklD" 4 1),
        makeState (1, 2) "hijklD" 5 1 @=? step D initialStateTest0,
        [] @=? doors (makeState (2, 2) "hijklDR" 4 1),
        "DDRRRD" @=? solveA initialStateTest1,
        "DDUDRLRRUDRD" @=? solveA initialStateTest2,
        "DRURDRUDDLLDLUURRDULRLDUUDDDRR" @=? solveA initialStateTest3
        --,370 @=?  solveB initialStateTest1 -- VERY SLOW test. Takes several minutes. So skip them for the most part.
        --,492 @=?  solveB initialStateTest2 -- VERY SLOW test. Takes several minutes. So skip them for the most part.
        --,830 @=?  solveB initialStateTest3 -- VERY SLOW test. Takes several minutes. So skip them for the most part.
      ]

main :: IO ()
main = runTestTT testDay17 >>= print