module Main where

import Data.List (transpose)

-- take a String with integers separated by (one or more) spaces and return a List over Int
splitIntLine :: String -> [Int]
splitIntLine x =
  map (read :: String -> Int) (words x)

canBeTriangle :: [Int] -> Bool
canBeTriangle x =
  if length x /= 3
    then error "ValueError - a triangle has three sides only!"
    else do
      let tot = sum x
      let m = maximum x
      m * 2 < tot

countTruesInBoolList :: [Bool] -> Int
countTruesInBoolList x = sum (map fromEnum x) -- It happens that Bool values is an enum, where True = 1, and False = 0

-- http://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = take n l : chunk n (drop n l)
  | otherwise = error "Negative n"

partA :: String -> Int
partA = countTruesInBoolList . map (canBeTriangle . splitIntLine) . lines

partB :: String -> Int
partB = countTruesInBoolList . map canBeTriangle . chunk 3 . concat . transpose . map splitIntLine . lines

main :: IO ()
main = do
  x <- readFile "inputs/day03.txt"
  print $ partA x
  print $ partB x
