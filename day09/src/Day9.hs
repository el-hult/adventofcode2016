module Day9 where

import Text.Parsec
import Data.Bifunctor (first)

parseOneExpansion :: Parsec String () (String, Int, Int, String)
parseOneExpansion = do
  x1 <- manyTill anyChar (try (string "("))
  d1 <- many1 digit
  _ <- char 'x'
  d2 <- many1 digit
  _ <- char ')'
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
applyStep (h, i1, i2, t) = (h ++ concat (replicate i2 (take i1 t)), drop i1 t)

goTakeStep :: String -> ([Char], [Char])
goTakeStep = applyStep . parseStep

takeManySteps :: ([Char], [Char]) -> ([Char], [Char])
takeManySteps (s1, "") = (s1, "")
takeManySteps (s1, s2) = takeManySteps (first (s1 ++) (goTakeStep s2))

processString :: String -> String
processString s = fst (takeManySteps ("", s))

taskA :: String -> IO ()
taskA indata = do
  let l = case lines indata of
            (x:_) -> x
            _     -> error "Invalid input"
  let pl = processString l
  let n = length pl
  putStrLn $ "Task A:" ++ show n


-------
-- B --
-------
processStringB :: String -> Int
processStringB s = do
  let (h, inLen, multiple, midAndTail) = parseStep s :: (String, Int, Int, String)
  if inLen == -1
    then length h
    else do
      let (mid, t) = splitAt inLen midAndTail
          headVal = length h
          inVal = processStringB mid
          midVal = inVal * multiple -- force evaluation!
          tailVal = processStringB t
      headVal + midVal + tailVal

taskB :: String -> IO ()
taskB indata = do
  let n = case lines indata of
            (x:_) -> processStringB x
            _     -> error "Invalid input"
  putStrLn $ "Task B:" ++ show n
