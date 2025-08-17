{-# OPTIONS_GHC -Wno-x-partial #-}


import Data.List (sortOn)

-- |
-- Module: Day15
--
-- A classical chinese-remained-theorem problem
--
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
--
-- For the capsule dropped at time t to pass disc i, having size si and initial offset oi,
-- we must fulfill
--
-- > t+i+oi `mod` si = 0
--
-- So we have a set of congruences over index i, and define \[ai=-i-oi `mod` si\]
--
-- > t `mod` si = ai
type DiscSize = Int

type Offset = Int

type Modulus = Int

type Remainder = Int

-- | Input. Hand parsed. A list of (discSize,initialOffset)
myInput :: [(DiscSize, Offset)]
myInput =
  [ (13, 1),
    (19, 10),
    (3, 2),
    (7, 1),
    (5, 3),
    (17, 5)
  ]

-- | take a list of domain specific settings and convert to a list of moduli and remainders to solve the CRT on
modsAndRems :: [(DiscSize, DiscSize)] -> [(Int, Int)]
modsAndRems = sortOn fst . zipWith (\i (si, oi) -> (si, (- i - oi) `mod` si)) [1 ..]

-- | Produce a list of solutions to the Chinese Remainder Theorem by use of Sieving.
crtSieve :: [(Modulus, Remainder)] -> [Int]
crtSieve xs = filter isSolution [1 ..]
  where
    isSolution t = and [t `mod` si == ai | (si, ai) <- xs]

solveA :: [(DiscSize, Offset)] -> Int
solveA = head . crtSieve . modsAndRems

solveB :: [(DiscSize, Offset)] -> Int
solveB input = head . crtSieve $ modsAndRems (input ++ [(11, 0)]) -- somewhat idiotic, I dont reuse results from A in B. but this is BLAZINGLY fast anyways.

main :: IO ()
main = do
  print . solveA $ myInput -- 376777 is correct
  print . solveB $ myInput -- 3903937 is correct
