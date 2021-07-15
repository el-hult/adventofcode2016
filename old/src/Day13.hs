module Day13 where

import Control.Monad.State (State, evalState, get, put)
import Data.Bits (bit, popCount)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..), (><), (|>))
import qualified Data.Sequence as SS
import qualified Data.Set as S
import Util (untilM')

type Track = Seq (Int, Int)

type BfeState' = (S.Set (Int, Int), Seq Track)

type BfeState = State BfeState'

isOpen :: Int -> (Int, Int) -> Bool
isOpen favNum (x, y) = (x >= 0) && (y >= 0) && (even . popCount) (x * x + 3 * x + 2 * x * y + y + y * y + favNum)

neighborStates favNum (x, y) = filter (isOpen favNum) [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

-- from the sequence of tracks to check, pop one, populate the queue and tick of seens
bfeStep ::
  Int -> -- favNumber
  BfeState Track -- MAke progress on the state
bfeStep favNum = do
  (seen, tracksToExpand) <- get
  case tracksToExpand of
    (currTrack :<| checkLater) -> do
      let (_ :|> currState) = currTrack
      let newStates = [newS | newS <- neighborStates favNum currState, not (newS `S.member` seen)]
      let moreToCheck = map (currTrack |>) newStates
      put (S.fromList newStates `S.union` seen, checkLater >< SS.fromList moreToCheck)
      pure currTrack
    _ -> pure SS.empty

bfeTo ::
  Int -> -- favNumber
  (Int, Int) -> -- Final Point
  BfeState Track -- Out put is the first found path to the target
bfeTo favNum xyWin = go
  where
    go = do
      seqq <- bfeStep favNum
      let (_ :|> state) = seqq
      if state == xyWin
        then return seqq
        else go

ffill :: -- flood fill the graph, and step when next track has 51 steps (before it is taken!!)
  Int -> -- favNum
  Int -> -- how long tracks?
  BfeState BfeState'
ffill favNum n = go
  where
    go = do
      state <- get -- save a copy
      seqq <- bfeStep favNum -- take a step
      if SS.length seqq > n -- N.B. the track is 1 longer than the number of steps taken
        then return state
        else go

trackGoesTo xy (_ :|> xyNow) = xy == xyNow
trackGoesTo _ _ = False

-- Given a favoriteNumber, a starting point and a end point, return the first Track found in BFS
shortestPathTo favNum xy0 xyWin = evalState (bfeTo favNum xyWin) (S.singleton xy0, SS.singleton $ SS.singleton xy0)

solveB favNum depth xy0 = S.size . fst $ evalState (ffill favNum depth) (S.singleton xy0, SS.singleton $ SS.singleton xy0)

day13input = 1352

main = do
  let xyWin = (31, 39)
      xy0 = (1, 1)
  let tmp = (+) (-1) . length $ shortestPathTo day13input xy0 xyWin
  print tmp -- 90 is correct for Part A
  print $ solveB day13input 50 xy0 -- 135 is correct for B