module Day16 where

-- | BinaryVector type
type BV = [Bool]

-- | Convert a string of '0' and '1' to a binary vector
toBV :: String -> BV
toBV = map (== '1')

-- | Convert a binary vector to a string representation
toStr :: BV -> String
toStr = map (\s -> if s then '1' else '0')

-- | One step in dragon curve expansion
dragon1 :: BV -> BV
dragon1 bv = bv ++ [False] ++ map not (reverse bv)

-- | Expand the dragon curve until the length is at least n
dragonUntil :: Int -> BV -> BV
dragonUntil n bv
  | length bv > n = bv
  | otherwise = dragonUntil n $ dragon1 bv

getChecksum :: [Bool] -> [Bool]
getChecksum bv
  | odd (length bv) = bv
  | otherwise = getChecksum (crunch bv)
  where
    crunch (a : b : xs) = (a == b) : crunch xs
    crunch _  = []

solve :: Int -> String -> String
solve diskSize = toStr . getChecksum . take diskSize . dragonUntil diskSize . toBV

input :: String
input = "11110010111001001"

diskLenA :: Int
diskLenA = 272

diskLenB :: Int
diskLenB = 35651584

main :: IO ()
main = do
  print . solve diskLenA $ input -- 01110011101111011
  print . solve diskLenB $ input -- 11001111011000111
