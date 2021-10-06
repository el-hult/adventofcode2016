{-# LANGUAGE RecordWildCards #-}
module Day17 where

import Data.Bifunctor (first, second)
import Data.List (foldl')
import Util (hashString)

{-|
Module: Day17

BFS and DFS seraches. Quite sraight forward. 

This code takes ca 100 seconds to run on 1 core, but compiling for many cores actually increases runtime.

So "stack exec aoc2016 -- 17 +RTS -N1" is advisable, so it only runs on a single core.

I guess it would be REALLY interesting to understand the performance, and why it parallelizes so badly.

-}

type Pos = (Int,Int) -- ^ Current coordinate. Top left is (1,1) Top right it (4,1). Bottom right is (4,4).
data Dir = U | D | L | R deriving (Read, Show, Eq)

-- | A helper for updating the coordinate
stepCoord (x,y) U = (x,y-1)
stepCoord (x,y) D = (x,y+1)
stepCoord (x,y) R = (x+1,y)
stepCoord (x,y) L = (x-1,y)

-- | The state is really just the steps taken, since all else can be computed from that
-- But to make the program performance better, I compute and record a lot of intermediate values and keep.
data State = State{
    pos::Pos, -- ^ Current coordinate
    track:: String, -- ^ The passcode+all steps taken.
    doors:: [Dir], -- ^ All OPEN doors we could take from this position
    pcLen:: Int, -- ^ The passcode length
    hash :: String, -- ^ The current. Mostly for manual bug hunting. Not used in final solution.
    pathLen :: Int -- ^ A running tally of how many steps we have taken. To not have to count it manually later
    } deriving (Show,Eq)

{-Compute the open doors and pack into a state -}
makeState :: Pos -> String-> Int->Int -> State
makeState (x,y) key pcLen = State (x,y) key ( filter onMap . map fst. filter ( flip elem "bcdef" .snd) . zip [U,D,L,R] $ hash') pcLen hash'
    where hash' = hashString key
          onMap D = y<4
          onMap U = y>1
          onMap L = x>1
          onMap R = x<4

-- | A wrapper for `makeState`, in the case when we create initial states.
makeState2 key = makeState (1,1) key (length key) 0

step dir s@State{..} = makeState (stepCoord pos dir) (track++show dir) pcLen (pathLen+1)
getNeighbors s = [step d s | d<- doors s ]
getPath State{..} = drop pcLen track
isWinState State{..} = pos == (4,4)

-- * The main algorithm is BFS (for shortest path) and DFS (for longest path)

-- | breadth first search in a tree. 
-- No need to keep track of visited nodes. :)
bfs :: State -> [State]
bfs s = go [s]
    where go [] = []
          go xs = xs++go (xs >>= getNeighbors) -- slow appends! but it might be good enough I guess.


-- | depth first search
-- This implementation is in IO so I can print-debug
-- I also have a recursion limit set to 50_000
-- This DFS makes the WinState absorbing
-- to make sure that the recursion limit i high enough, I have
-- a print statement that I exhausted the stack
dfs' :: State -> IO [State]
dfs' s = go [s] [] 50000
    where
        go _ r 0 = print "ran all lotted times" >> pure r
        go [] r _ = print "exhausted the stack" >> pure r
        go (x:xs) toReturn n = do
            if n `mod` 100 == 0 then print ("Curr stack size:"++show (length xs +1)) else pure ()
            if isWinState x
                then go xs (toReturn++[x]) (n-1)
                else go (getNeighbors x ++ xs) (toReturn++[x]) (n-1)

-- | A Non-IO version of `dfs` above, with no recursion limit.
-- I moved to this after seeing that the algo above worked.
dfs :: State -> [State]
dfs s = go [s] []
    where
        go [] r = r
        go (x:xs) toReturn = do
            if isWinState x
                then go xs (toReturn++[x])
                else go (getNeighbors x ++ xs) (toReturn++[x])

-- ** The solvers
solveA = getPath . head . filter isWinState . bfs
solveB = maximum . map pathLen . filter isWinState . dfs -- non-io version
solveB' s =  maximum . map pathLen . filter isWinState <$> dfs' s -- io version witha little print debug

initialState = makeState2 "qljzarfv"
main = do
    print $ solveA initialState -- DRLRDDURDR
    print =<< solveB' initialState --500
    print $ solveB initialState --500 as well.