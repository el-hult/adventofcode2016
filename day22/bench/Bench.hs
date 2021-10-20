module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Day22

main =
  defaultMain
    [ bgroup
        "Solve test cases"
        [ bench "0" $ whnf (bfs maxXtest0 maxYtest0 testState0) isFinalState,
          bench "1" $ whnf (bfs maxXtest1 maxYtest1 testState1) isFinalState
        ]
    ]