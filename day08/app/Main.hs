module Main where

import Data.List (transpose)
import Util (splitOn)

newtype Screen = Screen [[Bool]]

instance Show Screen where
  show (Screen b) = unlines $ map (map (\x -> if x then '#' else '.')) b

countPixels :: Screen -> Int
countPixels (Screen b) = sum (map countPixelRow b)

countPixelRow :: [Bool] -> Int
countPixelRow = foldr (\b z -> z + fromEnum b) 0

instruct :: Screen -> String -> Screen
instruct sc instruction = case words instruction of
  ("rect" : argList) -> rect argList sc
  ("rotate" : "row" : argList) -> rotateR argList sc
  ("rotate" : "column" : argList) -> rotateC argList sc
  _ -> sc

rect :: [String] -> Screen -> Screen
rect args (Screen b) =
  let (rows, cols) = parseRectArgs args
   in Screen $ zipWith (\i bs -> if i < rows then drawRow cols bs else bs) [0 ..] b

drawRow :: Int -> [Bool] -> [Bool]
drawRow i b = replicate i True ++ drop i b

rotateR :: [String] -> Screen -> Screen
rotateR args (Screen b) =
  let (rowNo, n) = parseRotateArgs args
   in Screen $ zipWith (\i bs -> if i == rowNo then rotate n bs else bs) [0 ..] b

rotateC :: [String] -> Screen -> Screen
rotateC args sc = transScreen $ rotateR args $ transScreen sc

transScreen :: Screen -> Screen
transScreen (Screen b) = Screen (transpose b)

parseRotateArgs :: [String] -> (Int, Int)
parseRotateArgs (w1:_:w3:_) = do
  let n = read (drop 2 w1) :: Int
  let m = read w3 :: Int
  (n, m)
parseRotateArgs _ = error "Invalid rotate args"

parseRectArgs :: [String] -> (Int, Int)
parseRectArgs (w:_) =
  case splitOn 'x' w of 
    [] -> error "Invalid rect args"
    (_:[]) -> error "Invalid rect args"
    (z0:z1:_) -> (read z1 :: Int, read z0 :: Int)
parseRectArgs _ = error "Invalid rect args"


-- rotates to the right in a normal list.
rotate :: Int -> [a] -> [a]
rotate i l = let (a, b) = splitAt (length l - i) l in b ++ a

taskA :: String -> String
taskA s = do
  let blank = Screen $ replicate 6 $ replicate 50 False
  let done = foldl instruct blank $ lines s
  show done ++ show (countPixels done) ++ " pixels are on"

main :: IO ()
main =
  readFile "inputs/day08.txt" >>= \inString ->
    putStrLn "Answer to A:"
      >> putStrLn (taskA inString)
      >> putStrLn "Answer to B is shown together with A above! :)" -- 166
