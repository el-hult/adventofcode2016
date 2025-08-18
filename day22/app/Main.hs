{-# LANGUAGE FlexibleContexts #-}

--import Day22
import Text.Regex.TDFA
import Control.Exception (assert)
import Data.List (nub)

-- | Ignore the two header rows, and then parse all the data lines
-- the input looks kind of like 
--
-- root@ebhq-gridcenter# df -h
-- Filesystem              Size  Used  Avail  Use%
-- /dev/grid/node-x0-y0     89T   67T    22T   75%
-- /dev/grid/node-x0-y1     91T   72T    19T   79%
--
parseInput :: String -> [(Int, Int, Int, Int)]
parseInput s = 
  let ls = drop 2 . lines $ s
  in map parseLine ls
  where
    parseLine str = 
      case getAllTextMatches (str =~ "[0-9]+") :: [String] of 
        (x:y:size:used:_:_) -> (read x, read y, read size, read used)
        _ -> error $ "Could not parse line: " ++ str

-- | Solve part A
-- Check for viable pairs of nodes
-- Since we want to look at all pairs in a list, the list comprehension syntax is nice
solveA :: [(Int, Int, Int, Int)] -> Int
solveA nodes = length . filter (\(nA,nB) -> viablePair nA nB) $ [(a, b) | a <- nodes, b <- nodes]
  where
    -- A pair is viable if the first node holds some data, and the second node can accept that data
    -- and the nodes are distinct
    viablePair (xA, yA, _, usedA) (xB, yB, sizeB, usedB) =
      usedA > 0 && (usedA + usedB) < sizeB && (xA, yA) /= (xB, yB)


-- | Compute the least number of moves to move the goal data from the top right corner to the top left
-- I made SO MANY attempts on this with a BFS search and then a A* search, but I never got it to work
-- I then tried in python, and failed there too.
-- I then looked more at my data, realized the explicit hints in the problem description is valid and helpful.
-- With that, I solved it by pen and paper.
-- This solution below is an algorithmic version of that pen-and-paper solution.
-- The idea is:
-- 1. Verify that the problem is solvable in terms of 
--   Goal data coordinate
--   Hole coordinate (the single node with useage == 0)
--   Some 'wall' nodes that you cannot move data in/out of
-- 2. Check no walls are in the top two rows
-- 3. Make a flood fill to find how to moves the hole to the position left of the goal data
-- 4. Add one move to swap the hole and goal data positions, and count 5 moves for each move of the goal/hole pair to the left
--
-- I also solved the goal/hole/wall version of the problem with A* search in python, but that seems like a pain to implement in Haskell
-- The only benefit with that solution is that we don't need to check whether any walls are in the top two rows. Otherwise it is just more complicated code.
-- The heuristic I used in that case was the manhattan distance from hole to target (ignoring walls) + 5 * distance from goal to hole.
-- If you find an input that has walls in the top two rows, try that A* solution instead!
solveB :: [(Int, Int, Int, Int)] -> Int
solveB nodes = 
  let wallCoords = [(x, y) | (x, y, _, used) <- nodes, used > 100]
      noWallsTop2Rows = all (\(_,y) -> y >= 2) wallCoords
      holeCoords = [(x, y) | (x, y, _, used) <- nodes, used == 0]
      holeCoord = case holeCoords of 
        [(x, y)] -> (x, y)
        _ -> error "Expected exactly one hole coordinate"
      maxX = maximum [x | (x, _, _, _) <- nodes]
      maxY = maximum [y | (_, y, _, _) <- nodes]
      minXisZero = minimum [x | (x, _, _, _) <- nodes] == 0
      minYisZero = minimum [y | (_, y, _, _) <- nodes] == 0
      intermediateCoord = (maxX - 1, 0) -- the position we want to move the hole to while flood filling
      minNormalSize = minimum [size | (x, y, size, _) <- nodes, (x, y) `notElem` wallCoords]
      minNormalUsed = minimum [used | (x, y, _, used) <- nodes, (x, y) `notElem` wallCoords]
      maxNormalUsed = maximum [used | (x, y, _, used) <- nodes, (x, y) `notElem` wallCoords]
      minDataSize = minimum [ size | (_, _, size, _) <- nodes, size > 0]
      normalNodesCanMove = maxNormalUsed <= minNormalSize -- all nodes that are non-walls have data that can be moved to any non-wall node
      normalNodesCantMerge = 2*minNormalUsed <= minNormalSize -- all nodes that are non-walls have too much data to merge with any other non-wall node
      wallDataDontFitInNormalNodes = minimum [ used > minNormalSize | (x,y,_,used) <- nodes, (x,y) `elem` wallCoords] -- all wall nodes have too much data to fit in any normal node
      wallNodesCantAcceptAnyData = minimum [ size - used > minDataSize | (x,y,size,used) <- nodes, (x,y) `elem` wallCoords]
  in
   -- Step 1. Verify problem structure
  assert minXisZero $
  assert minYisZero $
  assert normalNodesCanMove $
  assert normalNodesCantMerge $
  assert wallDataDontFitInNormalNodes $
  assert wallNodesCantAcceptAnyData $
  -- Step 2. Check the solution from the intermediate position is solvable with the 1+5*distance trick
  assert noWallsTop2Rows $
  -- Steps 3+4 run flood fill and add in the extra moves to complete it all
    (floodFill holeCoord intermediateCoord wallCoords maxX maxY) + 1 + 5 * (maxX - 1)

-- | floodFill holePos intermediatePos walls xMax yMax
-- Flood fill algorithm to find the manhattan distance from the hole position to the intermediate position
-- while staying on the grid and avoiding walls
floodFill :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int -> Int -> Int
floodFill holePos intermediatePos walls xMax yMax = go [holePos] [] 0
  where 
    go [] _ _ = error "No path found"
    go frontier visited dist
      | intermediatePos `elem` frontier = dist
      | otherwise = 
          let 
            neighbours (x,y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
            newFrontier = nub [(x, y) | pos <- frontier,(x, y) <- neighbours pos, (x, y) `notElem` walls, (x, y) `notElem` visited, x >= 0, y >= 0, x <= xMax, y <= yMax]
          in go newFrontier (frontier ++ visited) (dist + 1)

main :: IO ()
main = do
  -- there is a unit test that makes sure that all parses are successful, so this line wont discard data
  input <-  parseInput <$> readFile "inputs/day22.txt"
  print $ solveA input -- 1003 is correct for part A
  print $ solveB input -- 192 is correct for part B
