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

supernetSequences :: IPv7 -> [String]
supernetSequences (IPv7 ss) = every 2 ss

hypernetSequences :: IPv7 -> [String]
hypernetSequences (IPv7 (_:ss)) = every 2 ss
hypernetSequences _ = undefined

parseIPv7 :: String -> IPv7
parseIPv7 s = IPv7 $ splitOneOf "[]" s

-- specific tools
isAbba :: Char -> Char -> Char -> Char -> Bool
isAbba a b c d
  | a == b = False
  | c == d = False
  | (a == d) && (b == c) = True
  | otherwise = False

strHasABBA :: String -> Bool
strHasABBA (a : b : c : d : rest)
  | isAbba a b c d = True
  | otherwise = strHasABBA (b : c : d : rest)
strHasABBA _ = False

isABA :: String -> Bool
isABA (c1 : c2 : c3 : _) = c1 == c3 && c1 /= c2
isABA _ = False

allABAs :: String -> [String]
allABAs ss = filter isABA $ windows 3 ss

correspondingABABAB :: String -> String -> Bool
correspondingABABAB (a : b : _) (c : d : _) = a == d && b == c && a /= c
correspondingABABAB _ _ = False

-- generic tools
every :: Int -> [a] -> [a]
every n (x : xs) = x : every n (drop (n -1) xs)
every _ [] = []

windows :: Int -> [a] -> [[a]]
windows n xs@(_:ss)
  | n <= length xs = take n xs : windows n ss
  | otherwise = []
windows _ _ = undefined

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


taskA :: String -> IO ()
taskA x = do
  putStrLn "Answer to Task A:"
  print . length . filter id . map (supportsTLS . parseIPv7) . lines $ x

taskB :: String -> IO ()
taskB x = do
  putStrLn "Answer to Task B:"
  print . length . filter id . map (supportsSSL . parseIPv7) . lines $ x

