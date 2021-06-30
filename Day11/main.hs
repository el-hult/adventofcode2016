{- stack
  script
  --resolver lts-10.3
-}

-- This code I actually ran by O2 complation first. Then running. IT took too much time otherwise.
module Main where

import Data.Ord (comparing)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.List (transpose, unfoldr)
import Control.Monad (guard, join)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Exception ( assert )
import Data.Sequence (Seq (..),(><),(|>))
import qualified Data.Sequence as SS


-------------------------------------------------------------------------
-- https://stackoverflow.com/a/40581425/4050510

data Heap a = EmptyHeap | Heap a [Heap a]
    deriving Show

merge :: Ord a => Heap a -> Heap a -> Heap a
merge EmptyHeap h = h
merge h EmptyHeap = h
merge h1@(Heap x hs1) h2@(Heap y hs2)
    | x < y     = Heap x (h2:hs1)
    | otherwise = Heap y (h1:hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []           = EmptyHeap
mergePairs [h]          = h
mergePairs (h1:h2:hs)   = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap x hs) = mergePairs hs
deleteMin EmptyHeap = EmptyHeap


-- My own convenience functions
singleton :: a -> Heap a
singleton a = Heap a [EmptyHeap]

fromList :: (Ord a) =>  [a] -> Heap a
fromList = foldl (flip insert) EmptyHeap

-------------------------------------------------------------------------
-- My utilities


extractEvery m = map snd . filter (\(x,y) -> mod x m == 0) . zip [1..]
evenElems = extractEvery 2
oddElems xs = head xs : extractEvery 2 ( tail xs )


isElement :: Eq a => a -> [a] -> Bool
isElement _ [] = False
isElement a (x:xs)
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
    guard $ a<b -- one must be smaller for them to be distict
    pure [a,b]

pickOneOrTwo :: Ord a => [a] -> [[a]]
pickOneOrTwo list = singles list ++ distictUnorderedPairs list

safeHead [] = Nothing
safeHead (x:_) = Just x

backupVal y Nothing = y
backupVal y (Just x) = x

defaultHead y = backupVal y . safeHead

-------------------------------------------------------------------------
--- Domain specific data types and associated functions

data State = State {elevator::Int,items::[Int]} deriving (Eq)

-- The Ord must be a total ordering
-- It is also used as a heuristic - small values is close to the win-state
instance Ord State where
    compare s1 s2 = defaultHead EQ . filter (/=EQ) $ comps
        where
             comps = [comparing negNSafe s1 s2, comparing nFloorFour s1 s2, comparing elevator s1 s2,comparing items s1 s2]
             nFloorFour = length . filter (/=4) .items
             negNSafe State{items=items} = inner items 0
                            where inner [] n = n
                                  inner [_] n = n
                                  inner (a:b:xs) n = if a==b then inner xs (n-1) else inner xs n


instance Show State where
  show State{elevator=e,items=items} =
      let itemsLine k = zipWith (\floor label -> if floor ==k then label else ". ")  items [show i ++a | i <- [1.. (length items `div` 2)], a <- ["G","M"]]
          fullLine k = ["F"++show k , if e==k then "E " else ". "] ++ itemsLine k
          allLineData =  [ fullLine k | k<- reverse [1..4]]
      in unlines $ map unwords allLineData

isValidState s@State {elevator=elevator,items=items} = all (\i -> i `elem` [1,2,3,4]) (elevator:items) && isSafeState s
isSafeState State{items=items} = isSafe items
isWinState :: State -> Bool
isWinState s = all (==4) (items s)

isSafe :: [Int] -> Bool
isSafe items =
    and $ zipWith (||) shieldedChips lonelyChips
    where generators = oddElems items
          chips = evenElems items
          shieldedChips = zipWith (==) generators chips
          lonelyChips = do
              elementNr <- [1..(length generators)]
              let floor = chips !! (elementNr -1)
              let collisions = [ (gElem /= elementNr) && (gFloor == floor) | (gElem,gFloor) <- zip [1..] generators ]
              pure . not $  or collisions


itemMoveList State{elevator=e,items=items} = pickOneOrTwo itemsOnThisFloor
    where itemsOnThisFloor =  map snd . filter fst $zipWith (\itemFloor b-> (itemFloor==e,b))  items [1..]

neighborStates :: State -> [State]
neighborStates s@State{elevator=e,items=items} = catMaybes $ join [ [ moveElevator (+1) itemsToMove s  , moveElevator (+ (-1)) itemsToMove s ] | itemsToMove <- itemMoveList s]

-- run the elevator up or down a floor, with some specific items in it
moveElevator dir itemsToMove State{elevator=elevator,items=items} =
        let newFloor = dir elevator
            movedItems = zipWith (\item floor -> if item `isElement` itemsToMove then newFloor else floor) [1..] items :: [Int]
            newS = State newFloor movedItems
        in if isValidState newS then Just newS else Nothing

-------------------------------------------------------------------------
-- Solving the problem. I have kept my various search implementations for later comparison

-- breadth first enumeration of states, using Seq as a queue (it is a doubly linked list with fast appends in both ends)
-- so much more suitable for queues than normal haskell lists
bfe :: State -> Seq [State]
bfe s = inner (SS.singleton [s]) S.empty SS.empty
    where
        inner SS.Empty _ outlist = outlist
        inner (currTrack :<| checkLater) seen outList = inner (checkLater >< SS.fromList moreToCheck) (S.fromList newStates `S.union` seen) (outList |> currTrack)
            where newStates = [ newS | newS <- neighborStates (head currTrack), not (newS `S.member` seen)]
                  moreToCheck = map (: currTrack) newStates

-- like above, but sort each "level" in the BFE by using a Heap and sort "tracks" by length and then some heuristic.
-- the heuristic is very naive. this does not seem to win much time.
newtype StateTrack  = StateTrack [State] deriving Eq

instance Ord StateTrack where 
    compare (StateTrack ss1) (StateTrack ss2) = if length ss1 /= length ss2 then comparing length ss1 ss2 else comparing head ss1 ss2

bfeH :: State -> Seq [State]
bfeH s = inner (singleton (StateTrack [s])) S.empty SS.empty
    where
        inner EmptyHeap _ outlist = outlist
        inner (Heap (StateTrack currTrack) checkLater) seen outList = inner (mergePairs (fromList moreToCheck: checkLater)) (S.fromList newStates `S.union` seen) (outList |> currTrack)
            where newStates = [ newS | newS <- neighborStates (head currTrack), not (newS `S.member` seen)]
                  moreToCheck = map (StateTrack . (: currTrack)) newStates
-------------------------------------------------------------------------
-- Runners
simpleTests = do
    assert (not $ isSafeState State{elevator=1,items=[1,1,2,1]}) (pure ())
    assert (not $ isSafeState State{elevator=3,items=[3,4,4,3]}) (pure ())
    assert (not $ isSafe [3,4,4,3]) (pure ())
    assert (not $ isSafe [1,4,4,4]) (pure ())
    assert (not $ isValidState (State 4 [1,4,4,4])) (pure ())
    assert (not $ isValidState (State 6 [1,1,4,4])) (pure ())
    assert (isSafeState State{elevator=1,items=[2,1,2,1]}) (pure ())
    assert (isWinState $ State{elevator=4,items=[4,4,4,4]}) (pure ())
    assert (not . isWinState $ State{elevator=4,items=[4,4,4,3]}) (pure ())
    assert ("[[1],[2],[4],[1,2],[1,4],[2,4]]" == show ( itemMoveList State{elevator=1,items=[1,1,3,1]})) (pure ())

humanReadableTests = do
    print $ State{elevator=1,items=[2,1]} --ok 
    print $ State{elevator=1,items=[2,1,3,1]} --ok 
    print $ State{elevator=1,items=[1,2,3,4,3,2]} --ok 
    print $ State{elevator=1,items=[1,2,3,4,3,2,1,2]} --ok 
    print $ neighborStates $ State{elevator=1,items=[1,1,4,4]} -- ok
    print $ neighborStates $ State{elevator=1,items=[1,2,3,4]} -- ok
    print $ neighborStates $ State{elevator=2,items=[1,2,3,4]} -- ok


solve state = flip (-) 1. length . head . filter ( isWinState . head) . toList $ bfeH state

testState = State{elevator=1,items=[2,1,3,1]}
inputStateA = State 1 [1,1,2,3,2,3,2,3,2,3] --element order is Promethium, Cobalt, Curium, Ruthenium, Plutinoim
inputStateB = State 1 [1,1,2,3,2,3,2,3,2,3,1,1,1,1] --element order is Promethium, Cobalt, Curium, Ruthenium, Plutinoim, Elerium, Dilithium

main = do
    simpleTests
    --humanReadableTests -- these, I have to check they look good by eye
    print "Test case - should be 11"
    print $ solve testState 
    print "Part A - should be 33"
    print $ solve inputStateA -- 33 is correct. Takes ca 6 seconds.
    print "ANS B:"
    print $ solve inputStateB -- 57 is correct. Takes ca 600 seconds.
