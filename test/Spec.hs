import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day7 as D7
import qualified Day9 as D9
import qualified Day12 as D12
import qualified Day13 as D13
import Test.HUnit
import Control.Monad.State ( execState, evalState, runState )
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Either (Either(Right))
import qualified Data.Sequence as SS



testDay1 = TestList [
    TestLabel "test1" $ TestCase (assertEqual "Nice show" "Ins CW 1" (show $ D1.Ins D1.CW 1)),
    TestLabel "test2" $ TestCase (assertEqual "Nice show" "[Ins CW 7,Ins CCW 3,Ins CCW 1,Ins CW 7]" (show $ D1.stringToInstructions "R7, L3, L1, R7")),
    TestLabel "test3" $ TestCase (assertEqual "Nice show" "[(0,1),(0,0)]" (show $ D1.go1 D1.U [(0,0)])),
    TestLabel "test4" $ TestCase (assertEqual "Nice show" "[(0,-1),(0,0)]" (show $ D1.go1 D1.D [(0,0)])),
    TestLabel "test5" $ TestCase (assertEqual "Nice show" "[(1,0),(0,0)]" (show $ D1.go1 D1.R [(0,0)])),
    TestLabel "test6" $ TestCase (assertEqual "Nice show" "[(-1,0),(0,0)]" (show $ D1.go1 D1.L [(0,0)])),
    TestLabel "test7" $ TestCase (assertEqual "Nice show" "([(3,0),(2,0),(1,0),(0,0)],R)" (show $ execState (D1.goA (D1.Ins D1.CW 3)) ([(0,0)],D1.U))),
    TestLabel "test8" $ TestCase (assertEqual "Nice show" "([(1,1),(1,0),(0,0)],U)" (show $ execState (mapM_ D1.goA [D1.Ins D1.CW 1, D1.Ins D1.CCW 1]) ([(0,0)],D1.U))),
    TestLabel "test9"  $ TestCase (assertEqual "Solve A correct" 5 (D1.taskA' "R2, L3")),
    TestLabel "test10" $ TestCase (assertEqual "Solve A correct" 2 (D1.taskA' "R2, R2, R2")),
    TestLabel "test11" $ TestCase (assertEqual "Solve A correct" 12 (D1.taskA' "R5, L5, R5, R3")),
    TestLabel "test12" $ TestCase (assertEqual "Solve B correct" 4 (D1.taskB' "R8, R4, R4, R8"))
    ]

testDay2 = TestList [
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "(-1,0)" (show $ D2.move D2.U (0,0))),
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "Just 5" (show $ D2.numPadA !? (1,1))),
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "(0,0)" (show $ D2.moveOnPad D2.numPadA (0,0) D2.U)),
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "(1,0)" (show $ D2.moveOnPad D2.numPadA (0,0) D2.D)),
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "([5,1],(0,0))" (show $ D2.addNextButton D2.numPadA ([D2.Five],(1,1)) [D2.U,D2.U,D2.U,D2.L,D2.L])),
    TestLabel "testX" $ TestCase (assertEqual "Nice show" "[2,1]" (show $ D2.addAllButtons D2.numPadA (1,1) [[D2.U,D2.U,D2.U],[D2.L,D2.L]]))
    ]

testDay7 = TestList [
    TestLabel "Windowing" $ TestCase $ assertEqual "case1" ["ab", "bc", "cd", "de"] $ D7.windows 2 "abcde",
    TestLabel "Parsing" $ TestList [ TestCase $ assertEqual "Show" "abba[qwerty]bajs" $ show $ D7.IPv7 ["abba","qwerty","bajs"],
                                     TestCase $ assertEqual "Parsing" (D7.IPv7 ["abba","qwerty","bajs"]) (D7.parseIPv7  "abba[qwerty]bajs")
                           ],
    TestLabel "ABBA detection" $ TestList [ TestCase $ True @=? D7.strHasABBA "abba",
                                            TestCase $ False @=? D7.strHasABBA "qwery",
                                            TestCase $ False @=? D7.strHasABBA "xxxx",
                                            TestCase $ False @=? D7.strHasABBA "aaaab",
                                            TestCase $ True @=? D7.strHasABBA "aaaabba"
                                            ],
    TestLabel "TLS validation" $ TestList [TestCase $ assertBool "case1" $ D7.supportsTLS . D7.parseIPv7 $ "abba[mnop]qrst",
                                           TestCase $ assertBool "case2" $ not . D7.supportsTLS . D7.parseIPv7 $ "abcd[bddb]xyyx",
                                           TestCase $ assertBool "case3" $ not . D7.supportsTLS . D7.parseIPv7 $ "aaaa[qwer]tyui",
                                           TestCase $ assertBool "case4" $ D7.supportsTLS . D7.parseIPv7 $ "ioxxoj[asdfgh]zxcvbn",
                                           TestCase $ assertBool "case5" $ D7.supportsTLS . D7.parseIPv7 $ "ioxxoj[asdfgh]zxcvbn[qwertr]alll"
                ],
    TestLabel "ABA detection" $ TestList [TestCase $ assertEqual "case1" True $  D7.correspondingABABAB "aba" "bab",
                                          TestCase $ assertEqual "case2" True $ not $  D7.correspondingABABAB "aba" "qyq"
                                          ],
    TestLabel "SSL Validation" $ TestList [TestCase $ assertBool "case1" $ D7.supportsSSL . D7.parseIPv7 $ "aba[bab]xyz",
                                           TestCase $ assertBool "case2" $ not . D7.supportsSSL . D7.parseIPv7 $ "xyx[xyx]xyx",
                                           TestCase $ assertBool "case3" $ D7.supportsSSL . D7.parseIPv7 $ "aaa[kek]eke",
                                           TestCase $ assertBool "case4" $ D7.supportsSSL . D7.parseIPv7 $ "zazbz[bzb]cdb"]
    ]

testDay9 = TestLabel "Day 9" $ TestList [
    TestCase $ assertEqual "Simple Parse" ("abc",3,7,"ghi") $ D9.parseStep "abc(3x7)ghi" ,
    TestCase $ assertEqual "Double Parse Gets one" ("abc",3,7,"ghi(9x1)oo") $ D9.parseStep "abc(3x7)ghi(9x1)oo" ,
    TestCase $ assertEqual "Failing Parse" ("abcghi",-1,0,"") $ D9.parseStep "abcghi"
    ]

testDay12 = TestLabel "Day 12" $ TestList [
    TestLabel "parse" $ TestList [
        TestCase $ assertEqual "cpy" [D12.Cpy (D12.Value 8) D12.A] (D12.parseProg "cpy 8 a"),
        TestCase $ assertEqual "cpy" [D12.Cpy (D12.Ref D12.A) D12.C] (D12.parseProg "cpy a c"),
        TestCase $ assertEqual "jnz" [D12.Jnz (D12.Ref D12.A) 9] (D12.parseProg "jnz a 9"),
        TestCase $ assertEqual "many" [D12.Cpy (D12.Value 8) D12.A,D12.Jnz (D12.Ref D12.D) 1] (D12.parseProg "cpy 8 a\njnz d 1")
        ],
        TestLabel "run" $ TestList [
            TestCase $ assertEqual "cpy" (D12.Runtime (M.fromList [(D12.A, 8)]) [D12.Cpy (D12.Value 8) D12.A] 1) (execState D12.step (D12.Runtime M.empty [D12.Cpy (D12.Value 8) D12.A] 0) ),
            TestCase $ do{ x<- readFile "inputs/day12test.txt"; assertEqual "test" D12.Runtime{D12.mem = M.fromList [(D12.A,42)], D12.prog = [D12.Cpy (D12.Value 41) D12.A,D12.Inc D12.A,D12.Inc D12.A,D12.Dec D12.A,D12.Jnz (D12.Ref D12.A) 2,D12.Dec D12.A], D12.ptr = 6} (execState D12.runProg (D12.Runtime M.empty (D12.parseProg x) 0))}
        ]
    ]

testDay13 = TestLabel "Day 13" $ TestList [
    TestCase $ True @=?  D13.isOpen 10 (7,4),
    TestCase $ False @=?  D13.isOpen 10 (1,0),
    TestCase $ False @=?  D13.isOpen 10 (-1,0),
    TestCase $ True @=?  D13.isOpen 10 (2,2),
    TestCase $ [(2,2),(1,1)] @=?  D13.neighborStates 10 (1,2),
    TestCase $ 11 @=? ((+) (-1) . SS.length) (D13.shortestPathTo 10 (1,1) (7,4)),
    TestCase $ 1 @=? D13.solveB 10 0 (1,1),
    TestCase $ 3 @=? D13.solveB 10 1 (1,1),
    TestCase $ 5 @=? D13.solveB 10 2 (1,1)
    ]

main :: IO ()
main = do
    counts <- runTestTT $ TestList [testDay1, testDay2, testDay7, testDay9, testDay12, testDay13]
    print counts
