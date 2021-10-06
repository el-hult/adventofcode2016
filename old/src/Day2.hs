module Day2 where

import           Data.List  (foldl')
import           Data.Map   (Map, fromList, member, (!), (!?))
import           Data.Maybe (mapMaybe)

data Button = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Alpha | Bravo | Charlie | Delta

instance Show Button where
  show One     = "1"
  show Two     = "2"
  show Three   = "3"
  show Four    = "4"
  show Five    = "5"
  show Six     = "6"
  show Seven   = "7"
  show Eight   = "8"
  show Nine    = "9"
  show Alpha   = "A"
  show Bravo   = "B"
  show Charlie = "C"
  show Delta   = "D"

data Instruction = D | U | R | L deriving (Show)

type Coordinate = (Int, Int)

type NumPad = Map Coordinate Button

-- XY coordinates go from top left corner and down/right
move :: Instruction -> Coordinate -> Coordinate
move D (i, j) = (i + 1, j)
move U (i, j) = (i -1, j)
move R (i, j) = (i, j + 1)
move L (i, j) = (i, j -1)

parseInstruction :: Char -> Maybe Instruction
parseInstruction 'U' = Just U
parseInstruction 'R' = Just R
parseInstruction 'L' = Just L
parseInstruction 'D' = Just D
parseInstruction _   = Nothing

moveOnPad :: NumPad -> Coordinate -> Instruction -> Coordinate
moveOnPad pad coord instr =
  let attemptedCoordinate = move instr coord
      moveOk = member attemptedCoordinate pad
   in if moveOk then attemptedCoordinate else coord

simulateOnePath :: NumPad -> [Instruction] -> Coordinate -> Coordinate
simulateOnePath pad is c1 = foldl' (moveOnPad pad) c1 is

addNextButton :: NumPad -> ([Button], Coordinate) -> [Instruction] -> ([Button], Coordinate)
addNextButton pad (bs, c1) is =
  let c2 = simulateOnePath pad is c1
      b = pad ! c2
   in (bs ++ [b], c2)

addAllButtons :: NumPad -> Coordinate -> [[Instruction]] -> [Button]
addAllButtons _ _ [] = []
addAllButtons pad c1 iss =
  let (bs, _) = foldl' (addNextButton pad) ([], c1) iss
   in bs

-- TASK A ------------------------------

numPadA :: NumPad
numPadA =
  fromList
    [ ((0, 0), One),
      ((0, 1), Two),
      ((0, 2), Three),
      ((1, 0), Four),
      ((1, 1), Five),
      ((1, 2), Six),
      ((2, 0), Seven),
      ((2, 1), Eight),
      ((2, 2), Nine)
    ]

taskA = do
  x <- readFile "inputs/day02.txt"
  let y = lines x
  let iss = map (mapMaybe parseInstruction) y -- bad input fails silently!
  let bs = addAllButtons numPadA (1, 1) iss
  print bs

-- TASK B -------------------------------------

numPadB :: NumPad
numPadB =
  fromList
    [ ((0, 2), One),
      ((1, 1), Two),
      ((1, 2), Three),
      ((1, 3), Four),
      ((2, 0), Five),
      ((2, 1), Six),
      ((2, 2), Seven),
      ((2, 3), Eight),
      ((2, 4), Nine),
      ((3, 1), Alpha),
      ((3, 2), Bravo),
      ((3, 3), Charlie),
      ((4, 2), Delta)
    ]

taskB = do
  x <- readFile "inputs/day02.txt"
  let y = lines x
  let iss = map (mapMaybe parseInstruction) y -- bad input fails silently!
  let bs = addAllButtons numPadB (1, 1) iss
  print bs

-- RUNNER -------------------------------------
main = taskA >> taskB
