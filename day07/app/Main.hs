module Day7 where

import Util (splitOneOf)

-- Data structures
newtype IPv7 = IPv7 [String]

instance Show IPv7 where
  show (IPv7 ss) =
    let tmp = concat $ zipWith (\c s -> s ++ [c]) (cycle "[]") ss
     in init tmp

instance Eq IPv7 where
  (IPv7 a) == (IPv7 b) = a == b

supernetSequences (IPv7 ss) = every 2 ss

hypernetSequences (IPv7 ss) = every 2 (tail ss)

parseIPv7 :: String -> IPv7
parseIPv7 s = IPv7 $ splitOneOf "[]" s

-- specific tools
isAbba :: Char -> Char -> Char -> Char -> Bool
isAbba a b c d
  | a == b = False
  | c == d = False
  | (a == d) && (b == c) = True
  | otherwise = False

strHasABBA (a : b : c : d : tail)
  | isAbba a b c d = True
  | otherwise = strHasABBA (b : c : d : tail)
strHasABBA _ = False

isABA (c1 : c2 : c3 : _) = c1 == c3 && c1 /= c2

allABAs ss = filter isABA $ windows 3 ss

correspondingABABAB (a : b : _) (c : d : _) = a == d && b == c && a /= c

-- generic tools
every :: Int -> [a] -> [a]
every n (x : xs) = x : every n (drop (n -1) xs)
every _ [] = []

windows n xs
  | n <= length xs = take n xs : windows n (tail xs)
  | otherwise = []

-- value adding functions

supportsTLS :: IPv7 -> Bool
supportsTLS ip =
  let hyperBad = any strHasABBA $ hypernetSequences ip
      superGood = any strHasABBA $ supernetSequences ip
   in superGood && not hyperBad

supportsSSL :: IPv7 -> Bool
supportsSSL ip =
  let hyperABA = concatMap allABAs $ hypernetSequences ip
      superBAB = concatMap allABAs $ supernetSequences ip
      correspondences =
        [ correspondingABABAB aba bab
          | aba <- hyperABA,
            bab <- superBAB
        ]
   in or correspondences

-- Tests --------------------------------------------------------------

-- TASK A ------------------------------
taskA x = do
  putStrLn "Answer to Task A:"
  print . length . filter id . map (supportsTLS . parseIPv7) . lines $ x

-- TASK B -------------------------------------
taskB x = do
  putStrLn "Answer to Task B:"
  print . length . filter id . map (supportsSSL . parseIPv7) . lines $ x

-- RUNNER -------------------------------------
main = do
  x <- readFile "inputs/day07.txt"
  taskA x
  taskB x
