module Day6 where 

import Data.List ( sortBy, transpose ) 
import System.IO (Handle, withFile, IOMode(ReadMode), hGetContents)
import Data.Map (toList, fromListWith)

makeCountMap ::  [Char] -> [(Char, Int)]
makeCountMap inputString = 
 toList $ fromListWith (+) [(c, 1) | c <- inputString]

sortChars :: (Char,Int) -> (Char,Int) -> Ordering
sortChars (c1,i1) (c2,i2)
 | i1 < i2 = GT
 | i1 > i2 = LT
 | c1 > c2 = GT
 | c1 < c2 = LT

taskA  :: Handle -> IO () 
taskA handle = do
 contents <- hGetContents handle
 let k = map makeCountMap $ transpose $ lines contents
 let n = map fst $ concatMap ( take 1 . sortBy sortChars) k
 putStrLn n

taskB  :: Handle -> IO () 
taskB handle = do
 contents <- hGetContents handle
 let k = map makeCountMap $ transpose $ lines contents
 let n = map fst $ concatMap ( take 1 . sortBy (flip sortChars)) k
 putStrLn n

main = do
 putStrLn "Answer to A:"
 withFile "inputs/day06.txt" ReadMode taskA -- xhnqpqql
 putStrLn "Answer to B:"
 withFile "inputs/day06.txt" ReadMode taskB -- brhailro