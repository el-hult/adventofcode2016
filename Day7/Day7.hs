{- stack
  script
  --resolver lts-10.3
-}
import System.IO
import Data.List
import Data.List.Split (splitOneOf)
import Data.Map (toList, fromListWith)

parseString :: String -> Bool

parseString (a:b:c:d:tail)
 | isAbba a b c d = True
 | otherwise = parseString (b:c:d:tail)
parseString _ = False

isAbba :: Char -> Char -> Char -> Char -> Bool
isAbba a b c d
 | a == b = False
 | c == d = False
 | (a == d) && (b == c) = True
 | otherwise = False

data Level = Good | Bad | Meh deriving(Eq,Show)

evalPair :: (Bool,String) -> Level
evalPair (b,s)
 | b && parseString s = Good
 | not(b) && parseString s = Bad
 | otherwise = Meh

evalLine :: Bool -> [(Bool,String)] -> Bool
evalLine s (x:y:xs)
 | evalPair x == Bad = False
 | evalPair x == Good = evalLine True (y:xs)
 | evalPair x == Meh = evalLine s (y:xs)

evalLine s (x:_)
 | evalPair x == Bad = False
 | evalPair x == Good = True
 | evalPair x == Meh = s

taskA  :: String -> IO () 
taskA s = do
 let splitLists = map (zip (cycle [True,False] ) . splitOneOf "[]" ) $ lines s
 let goodBad = map (evalLine False) splitLists
 print $ length $ filter id goodBad

taskB  :: String -> IO () 
taskB s = putStrLn "hej"

main =
 readFile "input.txt" >>= \inString ->
  putStrLn "Answer to A:" >>
  taskA inString >>
  putStrLn "Answer to B:" >>
  taskB inString