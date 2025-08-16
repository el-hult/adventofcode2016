
testDay12 =
  TestLabel "Day 12" $
    TestList
      [ TestLabel "parse" $
          TestList
            [ TestCase $ assertEqual "cpy" [D12.Cpy (D12.Value 8) D12.A] (D12.parseProg "cpy 8 a"),
              TestCase $ assertEqual "cpy" [D12.Cpy (D12.Ref D12.A) D12.C] (D12.parseProg "cpy a c"),
              TestCase $ assertEqual "jnz" [D12.Jnz (D12.Ref D12.A) 9] (D12.parseProg "jnz a 9"),
              TestCase $ assertEqual "many" [D12.Cpy (D12.Value 8) D12.A, D12.Jnz (D12.Ref D12.D) 1] (D12.parseProg "cpy 8 a\njnz d 1")
            ],
        TestLabel "run" $
          TestList
            [ TestCase $ assertEqual "cpy" (D12.Runtime (M.fromList [(D12.A, 8)]) [D12.Cpy (D12.Value 8) D12.A] 1) (execState D12.step (D12.Runtime M.empty [D12.Cpy (D12.Value 8) D12.A] 0)),
              TestCase $ do x <- readFile "test/day12test.txt"; assertEqual "test" D12.Runtime {D12.mem = M.fromList [(D12.A, 42)], D12.prog = [D12.Cpy (D12.Value 41) D12.A, D12.Inc D12.A, D12.Inc D12.A, D12.Dec D12.A, D12.Jnz (D12.Ref D12.A) 2, D12.Dec D12.A], D12.ptr = 6} (execState D12.runProg (D12.Runtime M.empty (D12.parseProg x) 0))
            ]
      ]