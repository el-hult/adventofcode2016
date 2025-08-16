module Main where

import Data.Char (chr)
import Data.List (isInfixOf, sortBy)
import System.IO (IOMode (ReadMode), hGetContents, withFile, Handle)
import Util (makeCountMap, removePunc, splitLast, splitOn)

data MyContainer = Foo [(Char, Int)] Int [Char] [Char] deriving (Show)

data MyContainer2 = DecodedRoom [Char] Int deriving (Show)

getComponents :: [Char] -> MyContainer
getComponents line = do
  let h1 : h2 : _ = splitOn '[' line
  let g1 : g2 : _ = splitLast '-' h1
  Foo (makeCountMap (removePunc g1)) (read g2 :: Int) (init h2) h1

verifyChecksum :: MyContainer -> Bool
verifyChecksum (Foo inMap _ inCheck _) = inCheck == takeFive inMap

takeFive :: [(Char, Int)] -> [Char]
takeFive x = take 5 [fst tu | tu <- sortBy mySort x]

mySort :: (Char, Int) -> (Char, Int) -> Ordering
mySort (c1, i1) (c2, i2)
  | i1 < i2 = GT
  | i1 > i2 = LT
  | c1 > c2 = GT
  | c1 < c2 = LT

getInt :: MyContainer -> Int
getInt (Foo _ i _ _) = i

getNameFromRoom :: MyContainer2 -> [Char]
getNameFromRoom (DecodedRoom c _) = c

roomSort :: MyContainer2 -> MyContainer2 -> Ordering
roomSort (DecodedRoom n1 _) (DecodedRoom n2 _)
  | n1 > n2 = GT
  | n2 > n1 = LT

dechiper :: MyContainer -> MyContainer2
dechiper (Foo _ sectorId _ crypto) = do
  let z = map (`shiftAlpha` sectorId) crypto
  DecodedRoom z sectorId

alphaMin :: Int
alphaMin = fromEnum 'a'
alphaMax :: Int
alphaMax = fromEnum 'z'
alphaRange :: Int
alphaRange = alphaMax - alphaMin + 1

shiftAlpha :: Char -> Int -> Char
shiftAlpha letterIn shift =
  if letterIn == '-'
    then ' ' -- swap - to space
    else
      let charAsInt = fromEnum letterIn -- what ascii index?
          relative = charAsInt - alphaMin -- how much ahead of "a" is it?
          dechiperedCharIntRelative = (relative + shift) `mod` alphaRange
          w = alphaMin + dechiperedCharIntRelative
       in chr w

task4a :: Handle -> IO ()
task4a handle = do
  contents <- hGetContents handle
  let l = lines contents
  let c = map getComponents l
  let c2 = filter verifyChecksum c
  let j = map getInt c2
  print $ sum j

task4b :: Handle -> IO ()
task4b handle = do
  contents <- hGetContents handle
  let l = lines contents
  let c = map getComponents l
  let trueRooms = filter verifyChecksum c
  let decrypted = map dechiper trueRooms
  let interesting = filter (\w -> "north" `isInfixOf` getNameFromRoom w) decrypted
  let sorted = sortBy roomSort interesting
  mapM_ print sorted

main :: IO ()
main = do
  putStrLn "Answer to A:"
  withFile "inputs/day04.txt" ReadMode task4a --137896
  putStrLn "Answer to B:"
  withFile "inputs/day04.txt" ReadMode task4b -- 501
