module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Day22

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Solve test cases"
        [ bench "0 bfs" $ whnf (bfs maxXtest0 maxYtest0 testState0) isFinalState,
          bench "1 bfs" $ whnf (bfs maxXtest1 maxYtest1 testState1) isFinalState,
          bench "0 astar" $ whnf (starFind maxXtest0 maxYtest0) testState0,
          bench "1 astar" $ whnf (starFind maxXtest1 maxYtest1) testState1
        ]
    ]
