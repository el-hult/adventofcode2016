module Day8 where

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
rotateC args sc = transScreen $ rotateR args $transScreen sc

transScreen :: Screen -> Screen
transScreen (Screen b) = Screen (transpose b)

parseRotateArgs :: [String] -> (Int, Int)
parseRotateArgs w = do
  let n = read (tail $ tail ( head w)) :: Int
  let m = read (w !! 2) :: Int
  (n, m)

parseRectArgs :: [String] -> (Int, Int)
parseRectArgs w =
  let z0 : z1 : _ = splitOn 'x' (head w)
   in (read z1 :: Int, read z0 :: Int)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item : b) where (a, _ : b) = splitAt n ls

-- rotates to the right in a normal list.
rotate :: Int -> [a] -> [a]
rotate i l = let (a, b) = splitAt (length l - i) l in b ++ a

taskA :: String -> String
taskA s = do
  let blank = Screen $ replicate 6 $ replicate 50 False
  let done = foldl instruct blank $ lines s
  show done ++ show (countPixels done) ++ " pixels are on"

main =
  readFile "inputs/day08.txt" >>= \inString ->
    putStrLn "Answer to A:"
      >> putStrLn (taskA inString)
      >> putStrLn "Answer to B is shown together with A above! :)" -- 166
