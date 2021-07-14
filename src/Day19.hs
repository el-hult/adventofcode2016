module Day19 where
import Data.List (unfoldr)
import qualified Data.Sequence as SS
{- | Compute the solution to part A 
After some contemplation, I realize the problem has an easy solution. :)
-}
solveA k = 2*(k-a)+1
          where d = fromInteger k :: Double
                a' = toInteger (floor $ logBase 2 d)
                a = 2 ^ a'

solveB k = flip SS.index 0 . fst . last $ unfoldr stealOne (SS.fromList [1..k], 0)

main = do
    print . solveA $ 3001330 --Part A: 1808357
    print . solveB $ 3001330 --Part B: 1407007

type State = (SS.Seq Int,Int)

{- | Compute the solution to part B
By returning the whole State as return value (not just as the accumulator), I can use this in troubleshooting and testing
-}
stealOne :: State -> Maybe (State,State)
stealOne (ring,idx)
    | length ring <= 1 = Nothing
    | otherwise  = Just ((ring',idx'),(ring',idx'))
                    where
                        nElves = SS.length ring
                        toStealFrom = (idx + (nElves `div` 2)) `mod` nElves
                        ring' = SS.deleteAt toStealFrom ring
                        idx' = case () of _
                                            | idx == nElves -1 -> 0
                                            | toStealFrom < idx -> idx
                                            | toStealFrom > idx -> idx+1
                                            | otherwise -> error "This is not possible to get"


