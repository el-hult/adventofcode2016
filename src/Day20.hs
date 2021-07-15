module Day20 where

import Data.Word ( Word32 )
import Data.List ( sort )
import Data.Bifunctor ( Bifunctor(bimap) )
import Util ( splitFirst )

-- | An IP adress is encoded as a 32 bit unsigned integer
type Address = Word32
-- | A range in the blacklist has a inclusive start- and end point. These are maybe not ordered.
type Range = (Address,Address) 
-- | A blacklist is a SORTED NONOVERLAPPING collection of adress ranges
newtype Blacklist = Blacklist [Range] 

inRange :: Address -> Range -> Bool
n `inRange` (n1,n2) = (n1<=n && n <= n2) || (n1>=n && n >= n2)

sortTuple :: Ord b => (b, b) -> (b, b)
sortTuple (a,b) = (min a b, max a b)

{- | Attempts to merge two ranges.
Either they can be merged, and it returns Right <newrange> 
or they cannot, and returns Left <NormalizedRange1> <NormalizedRange2>
Where the normalized ranges are ordered
-}
mergeRanges :: Range -> Range -> Either (Range, Range) Range
mergeRanges (n1,n2) (n3,n4)
    | mergable = Right (o1,o2)
    | otherwise = Left (min rM1 rM2, max rM1 rM2)
    where m1 = min n1 n2
          m2 = max n1 n2
          m3 = min n3 n4
          m4 = max n3 n4
          o1 = minimum [m1,m2,m3,m4]
          o2 = maximum [m1,m2,m3,m4]
          rM1 = (m1,m2)
          rM2 = (m3,m4)
          mergable = (m1+1<=m3 && m4+1 <= m3) || (m3+1<=m1 && m2+1 <= m3) || (m1 `inRange` rM2) || (m2 `inRange` rM2) || (m3 `inRange` rM1) || (m4 `inRange` rM1)


-- | take a SORTED list of ranges, and normalize it to nonoverlapping ranges
-- Holds a temporary range that it merges on two. When there is a gap, it puts the
-- old tmp-range on the accumulator, and picks a new temp-range
mergeRangeList :: [Range] -> [Range]
mergeRangeList [] = []
mergeRangeList (r:rs) = go [] r rs
    where go acc tmp [] = acc ++ [tmp]
          go acc tmp (inR:inRs) = case tmp `mergeRanges` inR of
                    Right r2 -> go acc r2 inRs
                    Left (r1,r2) -> go (acc++[r1]) r2 inRs
                    
-- | Parse input, and make a Blacklist of it
makeBlacklist :: String -> Blacklist
makeBlacklist = Blacklist . mergeRangeList . sort. map (sortTuple . bimap read read . splitFirst '-'). lines

-- | Get the first non-blocked address
solveA :: Blacklist -> Address
solveA (Blacklist rs) = if n1 == 0 then n2+1 else n1-1
    where (n1,n2) = head rs

{- | Compute the number of non-blocked IP adresses by counting the blocked adresses
And then take all allowed adresses minus the blocked ones. A critical step here is that the 
input to solveB is a BlackList (especially nonoverlapping ranges!).

Note that Word32 cannot hold 4294967296, so I need to do the computation in two steps. 😂
-}
solveB :: Blacklist -> Address
solveB (Blacklist rs) =(+1). (4294967295 -)  . sum . map (\(a,b) -> b-a+1) $ rs

main :: IO ()
main = do
    bList <- makeBlacklist <$> readFile "inputs/day20.txt"
    print $ solveA bList -- 17348573 is right
    print $ solveB bList -- 104 is right