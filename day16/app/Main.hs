module Day16 where

type BV = [Bool]

toBV = map (== '1')

toStr = map (\s -> if s then '1' else '0')

-- | One step in dragon curve expansion
dragon1 bv = bv ++ [False] ++ map not (reverse bv)

dragonUntil n bv
  | length bv > n = bv
  | otherwise = dragonUntil n $ dragon1 bv

getChecksum :: [Bool] -> [Bool]
getChecksum bv
  | odd (length bv) = bv
  | otherwise = getChecksum (crunch bv)
  where
    crunch (a : b : xs) = (a == b) : crunch xs
    crunch [a] = []
    crunch [] = []

solve diskSize = toStr . getChecksum . take diskSize . dragonUntil diskSize . toBV

input = "11110010111001001"

diskLenA = 272

diskLenB = 35651584

main = do
  print . solve diskLenA $ input -- 01110011101111011
  print . solve diskLenB $ input -- 11001111011000111 takes a little while to compute. a minute or so.
