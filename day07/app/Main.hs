module Main where

import Day7 (taskA, taskB)

main :: IO ()
main = do
  x <- readFile "inputs/day07.txt"
  taskA x -- 110
  taskB x -- 242
