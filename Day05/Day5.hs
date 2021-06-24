{- stack
  script
  --resolver lts-10.3
-}
import System.IO
import Data.List
import Data.Hash.MD5
import Data.Maybe
import Data.Char (digitToInt)

inputString = "wtnhxymk"

main :: IO ()
main = do
 putStrLn (task5a inputString)
 putStrLn (task5b inputString)

hashString :: String -> String
hashString s = md5s ( Str s )

getMaybeKey :: String -> Maybe Char
getMaybeKey ('0':'0':'0':'0':'0':x:_) = Just x
getMaybeKey _ = Nothing

makeCandidate :: String -> Int -> String
makeCandidate i x = i ++ show x

task5a :: [Char] -> [Char]
task5a input = do
 let maybeChars = map (getMaybeKey . hashString . makeCandidate input ) [0..]
 let chars = catMaybes maybeChars
 show $ take 8 chars

getMaybeKeyAndPos :: String -> Maybe (Int,Char)
getMaybeKeyAndPos ('0':'0':'0':'0':'0':x:y:_)
 | elem x ['0'..'7'] = Just (digitToInt x,y) -- NB the key has 8 positions that are 0 indexed.
 | otherwise = Nothing
getMaybeKeyAndPos _ = Nothing

evalMoves :: [Char] -> [(Int,Char)] -> [Char]
evalMoves codeState []  = codeState
evalMoves codeState [(i,c)]
 | not ( elem '-' codeState  ) = codeState -- we are done!
 | codeState !! i  == '-' = replaceNth i c codeState -- set the new value and finish
 | otherwise = codeState  -- it stopped here.... 
evalMoves codeState ((i,c):moreMoves)
 | not ( elem '-' codeState  ) = codeState -- we are done!
 | codeState !! i == '-' = evalMoves (replaceNth i c codeState) moreMoves -- set the new value and try next move
 | otherwise = evalMoves codeState moreMoves -- try next move

-- http://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceNth :: Int -> t -> [t] -> [t]
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

task5b :: [Char] -> [Char]
task5b input = do
 let maybeMoves = map (getMaybeKeyAndPos . hashString . makeCandidate input ) [0..]
 let allowedMoves = catMaybes maybeMoves
 evalMoves "--------" allowedMoves
 --show $ take 5 allowedMoves
