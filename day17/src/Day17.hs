{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Day17 where

import Util (hashString)

-- |
-- Module: Day17
--
-- BFS and DFS seraches. Quite sraight forward.
-- This code takes ca 100 seconds to run on 1 core, but compiling for many cores actually increases runtime.
-- So "stack exec aoc2016 -- 17 +RTS -N1" is advisable, so it only runs on a single core.
-- I guess it would be REALLY interesting to understand the performance, and why it parallelizes so badly.
type Pos =
  -- | Current coordinate. Top left is (1,1) Top right it (4,1). Bottom right is (4,4).
  (Int, Int)

data Dir = U | D | L | R deriving (Read, Show, Eq)

-- | A helper for updating the coordinate
stepCoord :: (Num a1, Num a2) => (a2, a1) -> Dir -> (a2, a1)
stepCoord (x, y) U = (x, y -1)
stepCoord (x, y) D = (x, y + 1)
stepCoord (x, y) R = (x + 1, y)
stepCoord (x, y) L = (x -1, y)

-- | The state is really just the steps taken, since all else can be computed from that
-- But to make the program performance better, I compute and record a lot of intermediate values and keep.
data State = State
  { -- | Current coordinate
    pos :: Pos,
    -- | The passcode+all steps taken.
    track :: String,
    -- | All OPEN doors we could take from this position
    doors :: [Dir],
    -- | The passcode length
    pcLen :: Int,
    -- | The current. Mostly for manual bug hunting. Not used in final solution.
    hash :: String,
    -- | A running tally of how many steps we have taken. To not have to count it manually later
    pathLen :: Int
  }
  deriving (Show, Eq)

-- | Compute the open doors and pack into a state
-- Calling 'hashString' on 'key' all the time is wasteful, since only a single character is added to the `key' at each
-- step. If the hasher was stateful we could pass it around and speed up things. That is a optimization for another day...
makeState :: Pos -> String -> Int -> Int -> State
makeState (x, y) key pcLen = State (x, y) key (filter onMap . map fst . filter (flip elem "bcdef" . snd) . zip [U, D, L, R] $ hash') pcLen hash'
  where
    hash' = hashString key
    onMap D = y < 4
    onMap U = y > 1
    onMap L = x > 1
    onMap R = x < 4

-- | A wrapper for `makeState`, in the case when we create initial states.
initializeState :: String -> State
initializeState key = makeState (1, 1) key (length key) 0

step :: Dir -> State -> State
step dir State {..} = makeState (stepCoord pos dir) (track ++ show dir) pcLen (pathLen + 1)

getNeighbors :: State -> [State]
getNeighbors s = [step d s | d <- doors s]

getPath :: State -> [Char]
getPath State {..} = drop pcLen track

isWinState :: State -> Bool
isWinState State {..} = pos == (4, 4)

-- * The main algorithm is BFS (for shortest path) and DFS (for longest path)

-- | breadth first search in a tree.
-- No need to keep track of visited nodes. :)
bfs :: State -> [State]
bfs s = go [s]
  where
    go [] = []
    go xs = xs ++ go (xs >>= getNeighbors) -- slow appends! but it might be good enough I guess.

-- | dfs s
-- depth first search in a tree structure
-- since it is a tree, we dont need to keep track of revealed nodes (otherwise used to avoid revisiting expanded nodes)
-- there is no termination condition, so ALL nodes will be visited.
-- the nice thing here is that due to haskell lazyness, we can consume the list
-- it returns, and it won't need to allocate space for all possible expansions.
dfs :: State -> [State]
dfs s = go [s] []
  where
    go [] r = r
    go (x : xs) toReturn = do
      if isWinState x
        then go xs (toReturn ++ [x])
        else go (getNeighbors x ++ xs) (toReturn ++ [x])

-- ** The solvers

solveA :: State -> [Char]
solveA = getPath . head . filter isWinState . bfs

solveB :: State -> Int
solveB = maximum . map pathLen . filter isWinState . dfs
