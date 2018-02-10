{- stack
  script
  --resolver lts-10.3
-}
import Data.List.Split (splitOneOf)


-- Data structures
newtype IPv7 = IPv7 [String]
instance Show IPv7 where
    show (IPv7 ss) = 
        let tmp = concat $ zipWith (\c s -> s ++ [c]) (cycle "[]") ss
        in init tmp

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

strHasABBA (a:b:c:d:tail)
    | isAbba a b c d = True
    | otherwise = strHasABBA (b:c:d:tail)
strHasABBA _ = False

isABA (c1:c2:c3:_) = c1 == c3 && c1 /= c2

allABAs ss = filter isABA $ windows 3 ss

correspondingABABAB (a:b:_) (c:d:_) = a == d && b == c && a /= c

-- generic tools
every :: Int -> [a] -> [a]
every n (x:xs) = x : every n (drop (n-1) xs) 
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
        correspondences = [correspondingABABAB aba bab | 
                                    aba <- hyperABA,
                                    bab <- superBAB]
    in or correspondences


-- Tests --------------------------------------------------------------
testSnippets :: IO()
testSnippets = sequence_ [
    putStrLn "# Testing",
    putStrLn "# Other",
    print $ windows 2 "abcde", -- ["ab", "bc", "cd", "de"]
    putStrLn "## Parsing",
    print $ IPv7 ["abba","qwerty","bajs"],
    print . parseIPv7 $ "abba[qwerty]bajs",
    putStrLn "## ABBA detection",
    print . strHasABBA $ "abba", --true
    print . strHasABBA $ "qwery", --false
    print . strHasABBA $ "xxxx", --false
    print . strHasABBA $ "aaaab", --false
    print . strHasABBA $ "aaaabba", --true
    putStrLn "## TLS validation",
    print . supportsTLS . parseIPv7 $ "abba[mnop]qrst", --true
    print . supportsTLS . parseIPv7 $ "abcd[bddb]xyyx", --false 
    print . supportsTLS . parseIPv7 $ "aaaa[qwer]tyui", --false 
    print . supportsTLS . parseIPv7 $ "ioxxoj[asdfgh]zxcvbn", --true
    print . supportsTLS . parseIPv7 $ "ioxxoj[asdfgh]zxcvbn[qwertr]alll", --true
    putStrLn "## Correct row count",
    readFile "input.txt" >>= print . length . lines, -- 2000
    putStrLn "## ABA detection",
    print $ correspondingABABAB "aba" "bab",
    print $ correspondingABABAB "aba" "qyq",
    putStrLn "## SSL Validation",
    print . supportsSSL . parseIPv7 $ "aba[bab]xyz", --true
    print . supportsSSL . parseIPv7 $ "xyx[xyx]xyx", --false 
    print . supportsSSL . parseIPv7 $ "aaa[kek]eke", --true 
    print . supportsSSL . parseIPv7 $ "zazbz[bzb]cdb" --true
    ]

    -- TASK A ------------------------------
taskA = do
    putStrLn "Answer to Task A:"
    x <- readFile "input.txt"
    print . length . filter id . map (supportsTLS . parseIPv7) . lines $ x
    
    -- TASK B -------------------------------------
taskB = do
    putStrLn "Answer to Task B:"
    x <- readFile "input.txt"
    print . length . filter id . map (supportsSSL . parseIPv7) . lines $ x


-- RUNNER -------------------------------------
main = testSnippets >> taskA >> taskB
