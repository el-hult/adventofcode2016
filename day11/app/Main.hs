

import Control.Exception (assert)
import Control.Monad (guard, join)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as SS
import qualified Data.Set as S
import Prelude hiding (head)
import PairingHeap


-------------------------------------------------------------------------
-- My utilities

extractEvery :: Int -> [a] -> [a]
extractEvery m = map snd . filter (\(x, _) -> mod x m == 0) . zip [1 ..]

evenElems :: [a] -> [a]
evenElems = extractEvery 2

oddElems :: [a] -> [a]
oddElems xs = head xs : extractEvery 2 (drop 1 xs)

isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement a (x : xs)
  | a == x = True
  | otherwise = isElement a xs

singles :: [a] -> [[a]]
singles list = do
  a <- list
  pure [a]

distictUnorderedPairs :: Ord a => [a] -> [[a]]
distictUnorderedPairs list = do
  a <- list
  b <- list
  guard $ a < b -- one must be smaller for them to be distict
  pure [a, b]

pickOneOrTwo :: Ord a => [a] -> [[a]]
pickOneOrTwo list = singles list ++ distictUnorderedPairs list

pairwise :: [a] -> [(a, a)]
pairwise (a : b : xs) = (a, b) : pairwise xs
pairwise _ = []

head :: [a] -> a
head [] = error "head of empty list"
head (x : _) = x

-------------------------------------------------------------------------
--- Domain specific data types and associated functions

data State = State {elevator :: Int, items :: [Int]}

instance Eq State where
  s1 == s2 = (elevator s1 == elevator s2) && (sort . pairwise . items $ s1) == (sort . pairwise . items $ s2)

-- The Ord must be a total ordering
-- It is also used as a heuristic - small values is close to the win-state
instance Ord State where
  s1 <= s2 = (itms1, e1) <= (itms2, e2)
    where
      itms1 = sort . pairwise . items $ s1
      itms2 = sort . pairwise . items $ s2
      e1 = elevator s1
      e2 = elevator s2

instance Show State where
  show State {elevator = e, items = is} =
    let itemsLine k = zipWith (\floor' label -> if floor' == k then label else ". ") is [show i ++ a | i <- [1 .. (length is `div` 2)], a <- ["G", "M"]]
        fullLine k = ["F" ++ show k, if e == k then "E " else ". "] ++ itemsLine k
        allLineData = [fullLine k | k <- reverse [1 .. 4]]
     in unlines $ map unwords allLineData

isValidState :: State -> Bool
isValidState s@State {elevator = e, items = is} = all (\i -> i `elem` [1, 2, 3, 4]) (e : is) && isSafeState s

isSafeState :: State -> Bool
isSafeState = isSafe . items

isWinState :: State -> Bool
isWinState s = all (== 4) (items s)

isSafe :: [Int] -> Bool
isSafe items' =
  and $ zipWith (||) shieldedChips lonelyChips
  where
    generators = oddElems items'
    chips = evenElems items'
    shieldedChips = zipWith (==) generators chips
    lonelyChips = do
      elementNr <- [1 .. (length generators)]
      let floor' = chips !! (elementNr -1)
      let collisions = [(gElem /= elementNr) && (gFloor == floor') | (gElem, gFloor) <- zip [1 ..] generators]
      pure . not $ or collisions

itemMoveList :: State -> [[Int]]
itemMoveList State {items = is, elevator = e} = pickOneOrTwo itemsOnThisFloor
  where
    itemsOnThisFloor = map snd . filter fst $ zipWith (\itemFloor b -> (itemFloor == e, b)) is [1 ..]

neighborStates :: State -> [State]
neighborStates s = 
  catMaybes $ join [[moveElevator (+ 1) itemsToMove s, moveElevator (+ (-1)) itemsToMove s] | itemsToMove <- itemMoveList s]

-- run the elevator up or down a floor, with some specific items in it
moveElevator :: (Int -> Int) -> [Int] -> State -> Maybe State
moveElevator dir itemsToMove State {elevator = e, items = is} =
  let newFloor = dir e
      movedItems = zipWith (\item floor' -> if item `isElement` itemsToMove then newFloor else floor') [1 ..] is :: [Int]
      newS = State newFloor movedItems
   in if isValidState newS then Just newS else Nothing

-------------------------------------------------------------------------
-- Solving the problem. I have kept my various search implementations for later comparison

-- Breadth First Enumeration, but sort each "level" in the BFE by using a Heap and sort "tracks" by length and then some heuristic.
-- the heuristic is very naive. this does not seem to win much time.
newtype StateTrack = StateTrack [State] deriving (Eq)

instance Ord StateTrack where
  compare (StateTrack ss1) (StateTrack ss2) = if length ss1 /= length ss2 then comparing length ss1 ss2 else comparing head ss1 ss2

bfeH :: State -> Seq [State]
bfeH s = inner (singleton (StateTrack [s])) S.empty SS.empty
  where
    inner EmptyHeap _ outlist = outlist
    inner (Heap (StateTrack currTrack) checkLater) seen outList = inner (mergePairs (fromList moreToCheck : checkLater)) (S.fromList newStates `S.union` seen) (outList |> currTrack)
      where
        newStates = [newS | newS <- neighborStates (head currTrack), not (newS `S.member` seen)]
        moreToCheck = map (StateTrack . (: currTrack)) newStates

-------------------------------------------------------------------------
-- Runners
simpleTests :: IO ()
simpleTests = do
  assert (not $ isSafeState State {elevator = 1, items = [1, 1, 2, 1]}) (pure ())
  assert (not $ isSafeState State {elevator = 3, items = [3, 4, 4, 3]}) (pure ())
  assert (not $ isSafe [3, 4, 4, 3]) (pure ())
  assert (not $ isSafe [1, 4, 4, 4]) (pure ())
  assert (not $ isValidState (State 4 [1, 4, 4, 4])) (pure ())
  assert (not $ isValidState (State 6 [1, 1, 4, 4])) (pure ())
  assert (isSafeState State {elevator = 1, items = [2, 1, 2, 1]}) (pure ())
  assert (isWinState $ State {elevator = 4, items = [4, 4, 4, 4]}) (pure ())
  assert (not . isWinState $ State {elevator = 4, items = [4, 4, 4, 3]}) (pure ())
  assert ("[[1],[2],[4],[1,2],[1,4],[2,4]]" == show (itemMoveList State {elevator = 1, items = [1, 1, 3, 1]})) (pure ())

humanReadableTests :: IO ()
humanReadableTests = do
  print $ State {elevator = 1, items = [2, 1]} --ok
  print $ State {elevator = 1, items = [2, 1, 3, 1]} --ok
  print $ State {elevator = 1, items = [1, 2, 3, 4, 3, 2]} --ok
  print $ State {elevator = 1, items = [1, 2, 3, 4, 3, 2, 1, 2]} --ok
  print $ neighborStates $ State {elevator = 1, items = [1, 1, 4, 4]} -- ok
  print $ neighborStates $ State {elevator = 1, items = [1, 2, 3, 4]} -- ok
  print $ neighborStates $ State {elevator = 2, items = [1, 2, 3, 4]} -- ok

solve :: State -> Int
solve state = flip (-) 1 . length . head . filter (isWinState . head) . toList $ bfeH state

testState :: State
testState = State {elevator = 1, items = [2, 1, 3, 1]}

inputStateA :: State
inputStateA = State 1 [1, 1, 2, 3, 2, 3, 2, 3, 2, 3] --element order is Promethium, Cobalt, Curium, Ruthenium, Plutinoim

inputStateB :: State
inputStateB = State 1 [1, 1, 2, 3, 2, 3, 2, 3, 2, 3, 1, 1, 1, 1] --element order is Promethium, Cobalt, Curium, Ruthenium, Plutinoim, Elerium, Dilithium

main :: IO ()
main = do
  print "=========== TESTS ============"
  simpleTests -- the program raises an assert if there is a bug
  humanReadableTests -- these, I have to check they look good by eye
  
  print "Test case - should be 11"
  print $ solve testState
  print "========== REAL RUN =========="
  print "Part A - should be 33"
  print $ solve inputStateA -- 33 is correct. Takes ca 0.5 seconds.
  print "ANS B:"
  print $ solve inputStateB -- 57 is correct. Takes ca 4 seconds.

{-
In a profiling run of my code, it seemed the Set eats a lot of my memory in the runs.
So maybe I store too many states? How to reduce that?
To improve the runtime I was inspired by https://blog.jverkamp.com/2016/12/11/aoc-2016-day-11-radiation-avoider/
The problem don't really care about which element is which. So I whould change the Eq and Ord instance of State so that two states are identical if the
elelemnts acan be permutated to look the same. E.g. sort the elements on generator order and then chip order and then go on.
The search space decreases in part A by 5!=120 and part B with 7!=5040 since there are 5/7 elements.
 -}
