module Day13 where

import Data.Maybe (fromMaybe)
import Data.Bits (popCount,bit)
import Data.Sequence (Seq (..),(><),(|>))
import qualified Data.Sequence as SS
import qualified Data.Set as S




isOpen :: Int -> (Int,Int) -> Bool
isOpen favNum (x,y) =  (x>=0) && (y>=0) && (even . popCount) ( x*x + 3*x + 2*x*y + y + y*y + favNum)

neighborStates favNum (x,y) = filter (isOpen favNum) [(x-1,y), (x+1,y),(x,y-1),(x,y+1)]

-- breadth first enumeration using Sequences
bfe ::
    Int             -- favNumber
    -> (Int,Int)    -- Initial Point
     -> Seq (Seq (Int,Int)) -- Out put is a set of paths
bfe favNum xy0 = inner (SS.singleton $ SS.singleton xy0) S.empty SS.empty
    where
        inner SS.Empty _ outlist = outlist
        inner (currTrack :<| checkLater) seen outList = inner (checkLater >< SS.fromList moreToCheck) (S.fromList newStates `S.union` seen) (outList |> currTrack)
            where newStates = [ newS | newS <- neighborStates favNum currState, not (newS `S.member` seen) ] where (_ :|> currState) = currTrack
                  moreToCheck = map (currTrack |> ) newStates

trackGoesTo xy (_ :|> xyNow) = xy == xyNow
trackGoesTo _ _ = False

shortestPathTo favNum xy0 xyWin = SS.lookup 0 $ SS.filter (trackGoesTo xyWin) (bfe favNum xy0)


day13input = 1352
main = do
    let xyWin = (31,39)
        xy0 = (1,1)
        favNum = day13input
    let tmp  = (+) (-1) . length <$> shortestPathTo favNum xy0 xyWin
    maybe (print "No path to goal") print tmp -- 90 is correct for Part A