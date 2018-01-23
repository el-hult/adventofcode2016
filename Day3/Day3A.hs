{- stack
  script
  --resolver lts-10.3
-}
import System.IO

-- take a String with integers separated by (one or more) spaces and return a List over Int
splitIntLine :: String -> [Int]
splitIntLine x =
  map (read::String->Int) (words x)

canBeTriangle :: [Int] -> Bool
canBeTriangle x =
  if (length x) /= 3
    then error "ValueError - a triangle has three sides only!"
    else do
      let tot = sum x
      let m = maximum x
      if m*2 < tot
        then True
        else False

countTruesInBoolList :: [Bool] -> Int
countTruesInBoolList x = 
  sum (map fromEnum x ) -- It happens that Bool values is an enum, where True = 1, and False = 0

main = do
  withFile "input.txt" ReadMode (\handle -> do  
    contents <- hGetContents handle     
    let l = lines contents
    let i = map splitIntLine l
    let b = map canBeTriangle i
    putStrLn . show $ countTruesInBoolList b )

-- http://stackoverflow.com/questions/15450467/applying-putstr-to-each-item-of-a-list