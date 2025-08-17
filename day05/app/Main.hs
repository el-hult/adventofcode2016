
module Main where

import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Util (hashString, replaceAtIndex)

getMaybeKey :: String -> Maybe Char
getMaybeKey ('0' : '0' : '0' : '0' : '0' : x : _) = Just x
getMaybeKey _ = Nothing

makeCandidate :: String -> Int -> String
makeCandidate i x = i ++ show x

getMaybeKeyAndPos :: String -> Maybe (Int, Char)
getMaybeKeyAndPos ('0' : '0' : '0' : '0' : '0' : x : y : _)
  | x `elem` ['0' .. '7'] = Just (digitToInt x, y) -- NB the key has 8 positions that are 0 indexed.
  | otherwise = Nothing
getMaybeKeyAndPos _ = Nothing

evalMoves :: String -> [(Int, Char)] -> String
evalMoves codeState [] = codeState
evalMoves codeState [(i, c)]
  | '-' `notElem` codeState = codeState -- we are done!
  | codeState !! i == '-' = replaceAtIndex i c codeState -- set the new value and finish
  | otherwise = codeState -- it stopped here....
evalMoves codeState ((i, c) : moreMoves)
  | '-' `notElem` codeState = codeState -- we are done!
  | codeState !! i == '-' = evalMoves (replaceAtIndex i c codeState) moreMoves -- set the new value and try next move
  | otherwise = evalMoves codeState moreMoves -- try next move

task5b :: String -> String
task5b input = do
  let maybeMoves = map (getMaybeKeyAndPos . hashString . makeCandidate input) [0 ..]
  let allowedMoves = catMaybes maybeMoves
  evalMoves "--------" allowedMoves

task5a :: String -> String
task5a input = do
  let maybeChars = map (getMaybeKey . hashString . makeCandidate input) [0 ..]
  let chars = catMaybes maybeChars
  show $ take 8 chars

inputString :: String
inputString = "wtnhxymk"

main :: IO ()
main = do
  putStrLn (task5a inputString) -- 2414bc77
  putStrLn (task5b inputString) -- 437e60fc
