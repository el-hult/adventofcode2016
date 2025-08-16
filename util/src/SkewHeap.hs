module SkewHeap (SkewHeap (), extractMin, singleton, insert, null, (+++)) where

import Prelude hiding (null)

-- | A SkewHeap is a type of priority queue
--
-- Implemented like a binary tree with empty nodes and forking nodes, based on the presentation in
-- The Monad Reader (Issue 16) article "Playing with priority queues" by Louis Wasserman
-- available at https://themonadreader.files.wordpress.com/2010/05/issue16.pdf
--
-- For a simpler discussion, see
-- https://stackoverflow.com/questions/6976559/comparison-of-priority-queue-implementations-in-haskell
--
-- Every operation on a skew heap takes amortized O(log(n)) time
--
-- Invariant: all nodes are inserted by (+++) and nothing else
data SkewHeap a = Empty | SkewNode a (SkewHeap a) (SkewHeap a) deriving (Show)

-- | Union of two SkewHeaps
--
-- Not associative, so we cannot make a semigroup instance. :(
(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2)
  | x1 <= x2 = SkewNode x1 (heap2 +++ r1) l1
  | otherwise = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

infixr 5 +++ -- I same fixity as list concatenation, so it feels the same

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (SkewNode x l r) = Just (x, l +++ r)

singleton :: a -> SkewHeap a
singleton a = SkewNode a Empty Empty

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert a sh = sh +++ singleton a

null :: SkewHeap a -> Bool
null Empty = True
null _ = False