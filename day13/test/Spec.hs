

testDay13 =
  TestLabel "Day 13" $
    TestList
      [ TestCase $ True @=? D13.isOpen 10 (7, 4),
        TestCase $ False @=? D13.isOpen 10 (1, 0),
        TestCase $ False @=? D13.isOpen 10 (-1, 0),
        TestCase $ True @=? D13.isOpen 10 (2, 2),
        TestCase $ [(2, 2), (1, 1)] @=? D13.neighborStates 10 (1, 2),
        TestCase $ 11 @=? ((+) (-1) . SS.length) (D13.shortestPathTo 10 (1, 1) (7, 4)),
        TestCase $ 1 @=? D13.solveB 10 0 (1, 1),
        TestCase $ 3 @=? D13.solveB 10 1 (1, 1),
        TestCase $ 5 @=? D13.solveB 10 2 (1, 1)
      ]