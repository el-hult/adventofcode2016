module Day5 where

import Data.Maybe ( catMaybes )
import Data.Char ( digitToInt, ord )
import Distribution.Utils.MD5 (md5, showMD5)
import qualified Data.ByteString as B

hashString :: String -> String
hashString = showMD5 . md5 . B.pack . map (fromIntegral . ord)

getMaybeKey :: String -> Maybe Char
getMaybeKey ('0':'0':'0':'0':'0':x:_) = Just x
getMaybeKey _ = Nothing

makeCandidate :: String -> Int -> String
makeCandidate i x = i ++ show x

getMaybeKeyAndPos :: String -> Maybe (Int,Char)
getMaybeKeyAndPos ('0':'0':'0':'0':'0':x:y:_)
 | x `elem` ['0'..'7'] = Just (digitToInt x,y) -- NB the key has 8 positions that are 0 indexed.
 | otherwise = Nothing
getMaybeKeyAndPos _ = Nothing

evalMoves :: String -> [(Int,Char)] -> String
evalMoves codeState []  = codeState
evalMoves codeState [(i,c)]
 | '-' `notElem` codeState = codeState -- we are done!
 | codeState !! i  == '-' = replaceNth i c codeState -- set the new value and finish
 | otherwise = codeState  -- it stopped here.... 
evalMoves codeState ((i,c):moreMoves)
 | '-' `notElem` codeState = codeState -- we are done!
 | codeState !! i == '-' = evalMoves (replaceNth i c codeState) moreMoves -- set the new value and try next move
 | otherwise = evalMoves codeState moreMoves -- try next move

-- http://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceNth :: Int -> t -> [t] -> [t]
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs
replaceNth _ _ [] = []

task5b :: String -> String
task5b input = do
 let maybeMoves = map (getMaybeKeyAndPos . hashString . makeCandidate input ) [0..]
 let allowedMoves = catMaybes maybeMoves
 evalMoves "--------" allowedMoves
 --show $ take 5 allowedMoves

task5a :: String -> String
task5a input = do
 let maybeChars = map (getMaybeKey . hashString . makeCandidate input ) [0..]
 let chars = catMaybes maybeChars
 show $ take 8 chars


inputString = "wtnhxymk"

main :: IO ()
main = do
 putStrLn (task5a inputString) -- 2414bc77
 putStrLn (task5b inputString) -- 437e60fc
