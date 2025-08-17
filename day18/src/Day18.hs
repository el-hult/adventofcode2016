module Day18 where

import Control.Monad (join)
import Util (windows)

-- |
-- Module: Day18
--
-- A quite direct implementation.
-- Since Haskell is lazy, keeping 400k lines in memory is a not an issue.
-- The computation is somewhat quick as well. part B takes a few seconds in compiled mode, and maybe 10-15 seconds in GHCi.
--
-- Parts to improve could be:
-- - I guess some memoization would be beneficial. See https://hackage.haskell.org/package/MemoTrie
-- - Implementing the Row as a comonad can be more theory-nice than the window function. See http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html
-- - Splitting the nextR method into chunks acting on e.g. groups of 6 and memoizing these subpatterns would be nice. See https://en.wikipedia.org/wiki/Hashlife
data Tile = Safe | Trap deriving (Eq)

newtype Row = Row {unRow :: [Tile]}

instance Show Tile where
  show Safe = "."
  show Trap = "^"

instance Read Tile where
  readsPrec _ ('^' : s) = [(Trap, s)]
  readsPrec _ ('.' : s) = [(Safe, s)]
  readsPrec _ _ = []
  readList xs = maybe [] (\l -> [(reverse l, "")]) $ go xs []
    where
      go "" acc = Just acc
      go ('^' : s) acc = go s (Trap : acc)
      go ('.' : s) acc = go s (Safe : acc)
      go _ _ = Nothing

instance Show Row where
  show (Row []) = ""
  show (Row (x : xs)) = show x ++ show (Row xs)

-- | Given three tiles, decide the tile on next row
rule :: [Tile] -> Tile
rule [Safe, _, Trap] = Trap
rule [Trap, _, Safe] = Trap
rule _ = Safe

-- | Given a row, produce the next row
nextR :: Row -> Row
nextR (Row bs) = Row . map rule . windows 3 $ padded
  where
    padded = Safe : bs ++ [Safe]


-- | The initial row for my specific puzzle input
initRow :: Row
initRow = Row . read $ "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."


solveA n = length . filter (== Safe) . join . map unRow . take n . iterate nextR
