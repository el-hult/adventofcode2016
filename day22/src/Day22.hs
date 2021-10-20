{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day22 where

import Control.Exception (assert)
import Control.Lens
import qualified Control.Lens.Operators as B1
import Control.Monad (MonadPlus (mzero), guard)
import Data.List (nub)
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Text.Read (readEither)
import Util

--
--
-- Solutions to Day 22
--
-- # Part A
-- Here, we are asked to read a list of data nodes, and check pairwise if they are a "viable" pair. I.e. if
--     1) A != B
--     2) used A <= (size B - used B)
--     3) 0 < used A
--
-- it is unclear if they are considered an ordered pair or not. I.e. if this is fulfilled both for (A,B) and (B,A), does that count as two occurances?

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
  let board = fromList . map (\DataNode {x, y, size, used} -> ((x, y), (size, used))) $ lst
      maxX = maximum $ map x lst
      maxY = maximum $ map y lst
   in (S {_ptr = (maxX, 0), _board = board}, (maxX, maxY))

moveCoord D (x, y) = (x, y + 1)
moveCoord U (x, y) = (x, y -1)
moveCoord L (x, y) = (x -1, y)
moveCoord R (x, y) = (x + 1, y)

-- | The test state in the example
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
(testState1, (maxXtest1, maxYtest1)) =
  genInit
    [ DataNode 0 0 10 5,
      DataNode 1 0 10 5,
      DataNode 0 1 10 5,
      DataNode 1 1 10 10
    ]

--- | Try to make a move. If the move is invalid, return Nothing
-- >>> doMove (1,0) R testState1
-- WAS Nothing
-- NOW Nothing

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
-- >>> bfs2 genNodes 1
-- [(0,1),(1,2),(1,0),(2,3),(2,-1),(3,-2),(4,-3)]
bfs2 :: forall a. Ord a => (a -> [a]) -> a -> [(Int, a)]
bfs2 genNodes start = go [start] Set.empty 0
  where
    go :: [a] -> Set a -> Int -> [(Int, a)]
    go [] _ _ = []
    go front visited n =
      let newFront = nub . filter (`notElem` visited) . concatMap genNodes $ front
          newVisited = foldr Set.insert visited front
       in map (n,) front ++ go newFront newVisited (n + 1)

-- | The blackbird combinator. Also called B1.
-- take a binary function and a unary function and make a pipeline for them
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

-- | A takeWhile that also picks the element that is the one fulfilling the predicate.
takeUpTo p xs =
  let firstPassIdx = ((snd . head . filter (p . fst)) .: zip) xs [1 ..]
   in take firstPassIdx xs

-- | bfs maxX maxY a isFinish
-- Do a breadth first search in the state space of SolverStates.
-- Start at the SolverState `a`, and make BFS until the predicate isFinish has been fulfilled
-- Report hove many edges we needed to traverse to get there

-- >>> bfs maxXtest0 maxYtest0 testState0 isFinalState
-- 7
bfs :: Int -> Int -> SolverState -> (SolverState -> Bool) -> Int
bfs maxX maxY a isFinish = subtract 1 . fst . head . filter (isFinish . snd) $ bfs2 (generateNeighbors maxX maxY) a

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
