module PairingHeap where

-- | A pairing  heap implementation as per 
-- https://stackoverflow.com/a/40581425/4050510
-- insertions and and merge operations in amortized O(1) and deletion in O(log n)

-- Standard interface section

data Heap a = EmptyHeap | Heap a [Heap a]
  deriving (Show)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge EmptyHeap h = h
merge h EmptyHeap = h
merge h1@(Heap x hs1) h2@(Heap y hs2)
  | x < y = Heap x (h2 : hs1)
  | otherwise = Heap y (h1 : hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = EmptyHeap
mergePairs [h] = h
mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap _ hs) = mergePairs hs
deleteMin EmptyHeap = EmptyHeap

-- My own convenience functions are below from here
singleton :: a -> Heap a
singleton a = Heap a [EmptyHeap]

fromList :: (Ord a) => [a] -> Heap a
fromList = foldl (flip insert) EmptyHeap