module Main where

import Day21

solveA :: [Instruction] -> [String]
solveA = scanl applyInstruction "abcdefgh"

solveB :: [Instruction] -> [String]
solveB = scanl applyInstruction "fbgdceah" . reverse . map invertInstruction

main :: IO ()
main = do
  input <- parseInput <$> readFile "inputs/day21.txt"
  print $ fmap solveA input -- cbeghdaf is correct for part A
  print $ fmap solveB input -- bacdefgh is correct for part B
