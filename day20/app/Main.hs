import Data.Word (Word32)
import Day20

-- | Get the first non-blocked address
solveA :: Blacklist -> Address
solveA (Blacklist rs) = if n1 == 0 then n2 + 1 else n1 -1
  where
    (n1, n2) = head rs

-- | Compute the number of non-blocked IP adresses by counting the blocked adresses
-- And then take all allowed adresses minus the blocked ones. A critical step here is that the
-- input to solveB is a BlackList (especially nonoverlapping ranges!).
--
-- Note that Word32 cannot hold 4294967296, so I need to do the computation in two steps. 😂
-- I take 0- first, since I feel like a baller, and that is exactly 2**32  (if omitting the first 1)
-- Then, since I got an unexpected off-by-one-error, I add another 1. I honestly don't know where that is from. :/
solveB :: Blacklist -> Word32
solveB = (1 +) . (0 -) . totalBlocked

main :: IO ()
main = do
  bList <- makeBlacklist <$> readFile "inputs/day20.txt"
  print $ solveA bList -- 17348574 is right
  print $ solveB bList -- 104 is right