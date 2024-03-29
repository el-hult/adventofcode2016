module Main where

import Data.Either (rights)
import Day22

solveA :: [DataNode] -> String
solveA xs = show . length . filter (== True) $ [isViablePair n1 n2 | n1 <- xs, n2 <- xs]

solveB :: [DataNode] -> String
solveB dns =
  let (s0, (maxX, maxY)) = genInit dns
      mSteps = bfs maxX maxY s0 isFinalState
   in show mSteps

main :: IO ()
main = do
  -- there is a unit test that makes sure that all parses are successful, so this line wont discard data
  input <- rights . parseInput <$> readFile "inputs/day22.txt"
  print $ solveA input -- 1003 is correct for part A
  print $ solveB input -- ?? is correct for part B
