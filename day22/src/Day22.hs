{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Solutions to Day 22
--
-- # Part One
-- Here, we are asked to read a list of data nodes, and check pairwise if they are a "viable" pair. I.e. if
--     1) A != B
--     2) used A <= (size B - used B)
--     3) 0 < used A
--
-- it is an ordered pair, so (A,B) and (B,A) can be different viable pairs.
--
-- # Part Two
-- We are to solve a kind of 15 sliding puzzle. We are to move data between nodes. We can only move to adjacent nodes. If moving, it leaves the source node empty.
-- I hated this problem. A lot.
-- My first try was many many years ago. I tried solving it with a BFS search, but it was too slow.
-- Then I tried A*, but that was also too slow.
-- I gave up for a couple of years.
-- Tried to solve it in Python instead. (with BFS and A*).
-- Failed that too.
-- Finally I plotted my data.
-- I then realized that the simplifications made in the instructions apply to my data as well -- you don't need to care about the amount of data each node has!
-- You only need to track: 
-- 1) where the 'goal' data is
-- 2) where the 'hole' is (the empty node)
-- And you must know (but this is not variable) where are the 'wall' nodes, i.e. nodes that can never be moved to/from, so the hole can never go there.
-- with this simplification, I solved my paper with pen and paper.
-- I then implemented an A* solver in python. It was super fast. Essentially just copy pasting the pseudo code from wikipedia.
--
-- Trying to make A* work, while tracking all the utilization levels for each node, I could not get to work. The problem is that the searach space simply explodes too fast.
-- I need some super clever pruning/heuristic to make that work, and that is definitely outside the scope of Advent of Code.
-- The simplification I needed to make was explicitly hinted in the instructions, so that seems like a fair simplification to make. 
-- I have implemented the relevant checks in my code to verify that the simplification is valid for my input data, in case you run it with some other input.
module Day22 where

-- TODO: implement the python algorithm in Haskell code below

import Control.Lens
import Control.Monad (MonadPlus (mzero), guard)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified SkewHeap as SH
import Text.Read (readEither)
import Util

data DataNode = -- | A node in the data center at coordinate x y holding @used@ data, with a capacity of @size@
  DataNode {x :: Int, y :: Int, size :: Int, used :: Int}
  deriving (Show, Eq)

-- | Parse one line of input
-- RETURNS Right: the correct parse
--         Left: A string with some cryptic diagnostics for troubleshooting
lineToNode :: String -> Either String DataNode
lineToNode s = go mx my msize mused
  where
    mused = readEither . take 3 . drop 30 $ s
    msize = readEither . take 3 . drop 24 $ s
    mx = readEither . tail . (!! 1) . splitOn '-' $ s
    yS = take 3 . tail . (!! 2) . splitOn '-' $ s
    my = readEither yS
    go :: Either String Int -> Either String Int -> Either String Int -> Either String Int -> Either String DataNode
    go (Right x2) (Right y2) (Right size2) (Right used2) = Right DataNode {x = x2, y = y2, size = size2, used = used2}
    go x' y' size' used' = Left $ "FAILED!!!! " ++ show x' ++ show y' ++ yS ++ show size' ++ show used'

-- | Parse the whole input file
parseInput :: String -> [Either String DataNode]
parseInput = map lineToNode . drop 2 . lines

-- | isViablePair nodeA nodeB
-- To do this, you'd like to count the number of viable pairs of nodes. A viable pair is any two nodes (A,B), regardless of whether they are directly connected, such that:
--
--    Node A is not empty (its Used is not zero).
--    Nodes A and B are not the same node.
--    The data on node A (its Used) would fit on node B (its Avail).
isViablePair :: DataNode -> DataNode -> Bool
isViablePair DataNode {used = 0} _ = False
isViablePair n1@DataNode {x = x1, y = y1, used = u1} n2@DataNode {x = x2, y = y2, size = s2, used = u2}
  | x1 == x2 && y1 == y2 = False
  | u1 <= s2 - u2 = True
  | u2 > s2 - u1 = False
  | otherwise = error $ "Patterns and guards were not exhaustive, failed on n1=" ++ show n1 ++ " n2=" ++ show n2

-- |
--
-- For part B we have yet another optimization problem.
-- We are supposed to apply a series of data-moves to the data center until we have transformed the data center into the end-state.
-- The end-state is that specifically the data from the top right corner is moved into the top left corner.
--
-- We need to track the current position of the target data. Since all data moves moved _all_ the data from a source data center node,
-- a move will always keep the targetdata unfargented. A simple coordinate pointer is fine.
--
-- A move thus transfer data between grid points.
-- If the grid point contains the target data, we also move the target data pointer.
--
-- The source state is the data center as read from the input, and target data pointer in top right.
-- The target state is the target data pointer in the top left.
-- Any state is encoded by a map from (x,y) to (size,used)
type Coord = (Int, Int)

-- | The quantity of data at a node.
-- The pair is Size, Used
type NodeStatus = (Int, Int)

data Dir = R | L | U | D

-- | The map over the datacenter
type Grid = Map Coord NodeStatus

data SolverState = S
  { -- | a pointer that tracks where the target data is now
    _ptr :: Coord,
    -- | The map explaining the status of every node
    _board :: Grid
  }
  deriving (Show, Eq, Ord)

makeLenses ''SolverState

-- | Generate the initial parameters, from the input data
genInit :: [DataNode] -> (SolverState, Coord)
genInit lst =
  let board' = M.fromList . map (\DataNode {x, y, size, used} -> ((x, y), (size, used))) $ lst
      maxX = maximum $ map x lst
      maxY = maximum $ map y lst
   in (S {_ptr = (maxX, 0), _board = board'}, (maxX, maxY))

moveCoord :: Dir -> Coord -> Coord
moveCoord D (x, y) = (x, y + 1)
moveCoord U (x, y) = (x, y -1)
moveCoord L (x, y) = (x -1, y)
moveCoord R (x, y) = (x + 1, y)

-- | The test state in the example
maxYtest0 :: Int
maxXtest0 :: Int
testState0 :: SolverState
(testState0, (maxXtest0, maxYtest0)) =
  genInit
    [ DataNode 0 0 10 8,
      DataNode 1 0 9 7,
      DataNode 2 0 10 6,
      DataNode 0 1 11 0,
      DataNode 1 1 8 8,
      DataNode 2 1 9 6,
      DataNode 0 2 32 28,
      DataNode 1 3 11 7,
      DataNode 2 2 9 6
    ]

-- | A really small test state to play with
testState1 :: SolverState
maxXtest1 :: Int
maxYtest1 :: Int
(testState1, (maxXtest1, maxYtest1)) =
  genInit
    [ DataNode 0 0 10 5,
      DataNode 1 0 10 5,
      DataNode 0 1 10 5,
      DataNode 1 1 10 10
    ]

--- | Try to make a move. If the move is invalid, return Nothing
-- >>> doMove (1,0) R testState1
-- Nothing

-- >>> doMove (1,0) L testState1
-- Just (S {_ptr = (0,0), _board = fromList [((0,0),(10,10)),((0,1),(10,5)),((1,0),(10,0)),((1,1),(10,10))]})

doMove :: Coord -> Dir -> SolverState -> Maybe SolverState
doMove fromC dir s0 = do
  let toC = moveCoord dir fromC -- to what coordinate do we move data?
      qToMove = s0 ^. board . at fromC . to (fromMaybe (0, 0)) . _2 -- first see how much data we move about
      (sizeTarget, usedTarget) = s0 ^. board . at toC . to (fromMaybe (0, 0)) -- inspect target
  guard $ sizeTarget >= (qToMove + usedTarget)
  let s1 = if fromC == s0 ^. ptr then s0 & ptr %~ moveCoord dir else s0 -- move the target data pointer, if that is applicable
      s2 =
        s1 & board . ix fromC . _2 .~ 0 -- set the value to zero at the state we are clearing
          & board . ix toC . _2 %~ (+ qToMove) -- increment the value at the target
  return s2

-- | A general BFS traverser, that returns all nodes in the graph, together with the number of steps to take there
-- >>> genNodes n = if  abs n < 3 then [n+1,n-1] else []
-- >>> bfsTraverse genNodes 1
-- [(0,1),(1,2),(1,0),(2,3),(2,-1),(3,-2),(4,-3)]
bfsTraverse :: forall a. Ord a => (a -> [a]) -> a -> [(Int, a)]
bfsTraverse genNodes start = go [start] Set.empty 0
  where
    go :: [a] -> Set a -> Int -> [(Int, a)]
    go [] _ _ = []
    go front visited n =
      let newFront = nub . filter (`notElem` visited) . concatMap genNodes $ front
          newVisited = foldr Set.insert visited front
       in map (n,) front ++ go newFront newVisited (n + 1)

-- | bfs maxX maxY a isFinish
-- Do a breadth first search in the state space of SolverStates.
-- Start at the SolverState `a`, and make BFS until the predicate isFinish has been fulfilled
-- Report hove many edges we needed to traverse to get there

-- >>> bfs maxXtest0 maxYtest0 testState0 isFinalState
-- 7
bfs :: Int -> Int -> SolverState -> (SolverState -> Bool) -> Int
bfs maxX maxY a isFinish = subtract 1 . fst . head . filter (isFinish . snd) $ bfsTraverse (generateNeighbors maxX maxY) a

-- | Compute the taxicab dinstande between points
l1Distance :: Coord -> Coord -> Int
l1Distance (a, b) (c, d) = abs (a - c) + abs (b - d)

myHeuristic :: SolverState -> Double
myHeuristic ss = realToFrac $ l1Distance (ss ^. ptr) (0, 0)

-- >>> starFind maxXtest0 maxYtest0 testState0
starFind :: Int -> Int -> SolverState -> Int
starFind maxX maxY startState = subtract 1 . length $ aStar (generateNeighbors maxX maxY) myHeuristic startState isFinalState

-- >>> infinity
-- Infinity
infinity :: Double
infinity = read "Infinity"

getInf :: Ord a => a -> Map a Double -> Double
getInf = M.findWithDefault infinity

-- | Search for the shortest path between a start and a goal-node
-- Returns a list that represents the path from start to goal, in reverse.
aStar ::
  forall a.
  Ord a =>
  -- | the method that creates new nodes
  (a -> [a]) ->
  -- | The heuristic. Often called `h` in the litterature. use double, as they allow infinity
  (a -> Double) ->
  -- | the starting state
  a ->
  -- | a predicate for whether we have come to the goal or not
  (a -> Bool) ->
  -- | a path from start to goal,
  [a]
aStar genNewNodes scoreNode start isDone =
  go (SH.singleton (scoreNode start, start)) (Set.singleton start) M.empty (M.singleton start 0) (M.singleton start (scoreNode start))
  where
    go ::
      -- | The skewHeap has pairs where the first component is the fScore, which we order by
      SH.SkewHeap (Double, a) ->
      -- | The set is a list of all visited positions to prevent loops.
      Set a ->
      Map a a ->
      Map a Double ->
      Map a Double ->
      [a]
    go openSet discovered cameFrom gScore fScore
      | SH.null openSet = error "No path to goal!"
      | isDone current = trackToCurrent
      | otherwise = go newOpen newDiscovered newFrom newGScore newFScore
      where
        -- pop the best node to expand
        (current, _) = case SH.extractMin openSet of
          Nothing -> error "Bad code? This should NOT be empty"
          Just ((_, currentNode), val) -> (currentNode, val)
        gCurrent = getInf current gScore
        toUpdate = map (\(g, n) -> (g, g + scoreNode n, n)) . filter (\(g, n) -> g <= getInf n gScore) . map (gCurrent + 1,) $ genNewNodes current
        toAdd = filter (\(_, _, n) -> n `notElem` discovered) toUpdate
        -- These loops can be combined if im clever
        newOpen = foldl (flip SH.insert) openSet . map (\(_, f, n) -> (f, n)) $ toAdd
        newDiscovered = foldl (flip Set.insert) discovered . map (\(_, _, n) -> n) $ toAdd
        -- These too...
        newFrom = foldl (\m n -> M.insert n current m) cameFrom . map (\(_, _, n) -> n) $ toUpdate
        newGScore = foldl (\m (g, k) -> M.insert k g m) gScore . map (\(g, _, n) -> (g, n)) $ toUpdate
        newFScore = foldl (\m (f, k) -> M.insert k f m) fScore . map (\(_, f, n) -> (f, n)) $ toUpdate
        trackToCurrent = go2 [current]
          where
            go2 [] = error "you messed up f-head"
            go2 (x : xs) = cameFrom M.! x : x : xs

-- | generateNeighbors maxX maxY ss
-- Generate all neighbor states to the solver state `ss`, and check that all moves are valid
-- >>> generateNeighbors maxXtest1 maxYtest1 testState1
-- [S {_ptr = (1,0), _board = fromList [((0,0),(10,0)),((0,1),(10,10)),((1,0),(10,5)),((1,1),(10,10))]},S {_ptr = (1,0), _board = fromList [((0,0),(10,0)),((0,1),(10,5)),((1,0),(10,10)),((1,1),(10,10))]},S {_ptr = (1,0), _board = fromList [((0,0),(10,10)),((0,1),(10,0)),((1,0),(10,5)),((1,1),(10,10))]},S {_ptr = (0,0), _board = fromList [((0,0),(10,10)),((0,1),(10,5)),((1,0),(10,0)),((1,1),(10,10))]}]
generateNeighbors :: Int -> Int -> SolverState -> [SolverState]
generateNeighbors maxX maxY ss = do
  coord <- [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]
  dir <- [U, D, L, R]
  let (x', y') = moveCoord dir coord
  guard (0 <= x' && x' <= maxX && 0 <= y' && y' <= maxY)
  let possibleState = doMove coord dir ss
  maybe mzero return possibleState

isFinalState :: SolverState -> Bool
isFinalState s = s ^. ptr . to (== (0, 0))
