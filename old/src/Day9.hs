module Day9 where

import System.IO (putStrLn, readFile)
import Text.Parsec
import Data.Bifunctor (first)

parseOneExpansion = do
  x1 <- manyTill anyChar (try (string "("))
  d1 <- many1 digit
  char 'x'
  d2 <- many1 digit
  char ')'
  x2 <- many anyToken
  return (x1, read d1 :: Int, read d2 :: Int, x2)

parseStep :: String -> (String, Int, Int, String)
parseStep x =
  let parseRes = parse parseOneExpansion "MyTag" x
   in case parseRes of
        Right a -> a
        Left _ -> (x, -1, 0, "")

-------
-- A --
-------

applyStep :: (String, Int, Int, String) -> ([Char], [Char])
applyStep (head, i1, i2, tail) = (head ++ concat (replicate i2 (take i1 tail)), drop i1 tail)

goTakeStep :: String -> ([Char], [Char])
goTakeStep = applyStep . parseStep

takeManySteps :: ([Char], [Char]) -> ([Char], [Char])
takeManySteps (s1, "") = (s1, "")
takeManySteps (s1, s2) = takeManySteps (first (s1 ++) (goTakeStep s2))

processString :: String -> String
processString s = fst (takeManySteps ("", s))

taskA :: String -> IO ()
taskA indata = do
  let l = head (lines indata) -- only first line
  let pl = processString l
  let n = length pl
  putStrLn $ "Task A:" ++ show n

-------
-- B --
-------
processStringB :: String -> Int
processStringB s = do
  let (head, inLen, multiple, midAndTail) = parseStep s :: (String, Int, Int, String)
  if inLen == -1
    then length head
    else do
      let (mid, tail) = splitAt inLen midAndTail
          headVal = length head
          inVal = processStringB mid
          midVal = inVal * multiple -- force evaluation!
          tailVal = processStringB tail
      headVal + midVal + tailVal

taskB :: String -> IO ()
taskB indata = do
  let n = processStringB $ head $ lines indata -- only first line
  putStrLn $ "Task B:" ++ show n

------------
-- runner --
------------
main :: IO ()
main = do
  readFile "inputs/day09.txt"
    >>= ( \file ->
            taskA file -- 110346
              >> taskB file -- 10774309173
        )
