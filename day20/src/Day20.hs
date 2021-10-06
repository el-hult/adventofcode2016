module Day20 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sort)
import Data.Word (Word32)
import Util (splitFirst)

-- | An IP adress is encoded as a 32 bit unsigned integer
type Address = Word32

-- | A range in the blacklist has a inclusive start- and end point. These are maybe not ordered.
type Range = (Address, Address)

-- | A blacklist is a SORTED NONOVERLAPPING collection of adress ranges
newtype Blacklist = Blacklist [Range] deriving (Eq, Show)

inRange :: Address -> Range -> Bool
n `inRange` (n1, n2) = (n1 <= n && n <= n2) || (n1 >= n && n >= n2)

sortTuple :: Ord b => (b, b) -> (b, b)
sortTuple (a, b) = (min a b, max a b)

-- | Attempts to merge two ranges.
-- Either they can be merged, and it returns Right <newrange>
-- or they cannot, and returns Left <NormalizedRange1> <NormalizedRange2>
-- Where the normalized ranges are ordered
mergeRanges :: Range -> Range -> Either (Range, Range) Range
mergeRanges (n1, n2) (n3, n4)
  | mergable = Right (o1, o2)
  | otherwise = Left (min rM1 rM2, max rM1 rM2)
  where
    m1 = min n1 n2
    m2 = max n1 n2
    m3 = min n3 n4
    m4 = max n3 n4
    o1 = minimum [m1, m2, m3, m4]
    o2 = maximum [m1, m2, m3, m4]
    rM1 = (m1, m2)
    rM2 = (m3, m4)
    mergable =
      (m1 + 1 <= m3 && m4 + 1 <= m3)
        || (m3 + 1 <= m1 && m2 + 1 <= m3)
        || (m1 `inRange` rM2)
        || (m2 `inRange` rM2)
        || (m3 `inRange` rM1)
        || (m4 `inRange` rM1)

-- | take a SORTED list of ranges, and normalize it to nonoverlapping ranges
-- Holds a temporary range that it merges on two. When there is a gap, it puts the
-- old tmp-range on the accumulator, and picks a new temp-range
mergeRangeList :: [Range] -> [Range]
mergeRangeList [] = []
mergeRangeList (r : rs) = go [] r rs
  where
    go acc tmp [] = acc ++ [tmp]
    go acc tmp (inR : inRs) = case tmp `mergeRanges` inR of
      Right r2 -> go acc r2 inRs
      Left (r1, r2) -> go (acc ++ [r1]) r2 inRs

-- | Parse input, and make a Blacklist of it
makeBlacklist :: String -> Blacklist
makeBlacklist = Blacklist . mergeRangeList . sort . map (sortTuple . bimap read read . splitFirst '-') . lines

totalBlocked :: Blacklist -> Word32
totalBlocked (Blacklist rs) = sum . map (\(a, b) -> b - a + 1) $ rs
