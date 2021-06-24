{- stack
  script
  --resolver lts-10.3
-}
import System.IO
import Data.List

-- take a String with integers separated by (one or more) spaces and return a List over Int
splitIntLine :: String -> [Int]
splitIntLine x = map (read::String->Int) (words x)

canBeTriangle :: [Int] -> Bool
canBeTriangle x =
  if length x /= 3
    then error "ValueError - a triangle has three sides only!"
    else do
      let tot = sum x
      let m = maximum x
      m*2 < tot

countTruesInBoolList :: [Bool] -> Int
countTruesInBoolList x = sum (map fromEnum x ) -- It happens that Bool values is an enum, where True = 1, and False = 0

-- http://stackoverflow.com/questions/12876384/grouping-a-list-into-lists-of-n-elements-in-haskell
myGroup :: Int -> [a] -> [[a]]
myGroup _ [] = []
myGroup n l
  | n > 0 = take n l : myGroup n (drop n l)
  | otherwise = error "Negative n"

main = withFile "input.txt" ReadMode (\handle -> do
  contents <- hGetContents handle
  let l = lines contents
  let i = transpose $ map splitIntLine l
  let j = concat i
  let groupsOfThrees = myGroup 3 j
  let n = countTruesInBoolList $ map canBeTriangle groupsOfThrees
  print n )