module Main where
import Prelude hiding (last)
import Day21

last :: [a] -> a
last [] = error "empty list - no last element"
last (x:[]) = x
last (_:xs) = last xs

solveA :: [Instruction] -> String
solveA = last . scanl applyInstruction "abcdefgh"

solveB :: [Instruction] -> String
solveB = last . scanl applyInstruction "fbgdceah" . reverse . map invertInstruction

main :: IO ()
main = do
  input <- parseInput <$> readFile "inputs/day21.txt"
  print $ fmap solveA input -- cbeghdaf is correct for part A
  print $ fmap solveB input -- bacdefgh is correct for part B
