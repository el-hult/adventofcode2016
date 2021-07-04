module Util where

import Data.List (unfoldr)
import qualified Data.Map as M

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf cs = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (`elem` cs) $ l

-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell/7108719#7108719
-- added in explicit typing. not needed but i find it helping...
makeCountMap ::  [Char] -> [(Char, Int)]
makeCountMap inputString =
 M.toList $ M.fromListWith (+) [(c, 1) | c <- inputString]

splitLast :: Char -> [Char] -> [[Char]]
splitLast c s = do
 let z = splitOn c s
 let j = length z -1
 if j > 0
  then [ concat(take j z), z !! j]
  else [s]

removePunc xs = [ x | x <- xs, x `notElem` ",.?!-:;\"\'[]" ]

repeatM n f  
  | n <= 1 = f
  | otherwise  = f >> repeatM (n-1) f -- do monadic action n times

untilM_ :: (Monad m) => 
  m a  -- the action to perform
  -> m Bool -- the predicate to check
  -> m ()
action `untilM_` predicate = do
  action
  b <- predicate
  if b then pure () else action `untilM_` predicate