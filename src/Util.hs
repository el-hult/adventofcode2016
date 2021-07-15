module Util where

import Control.Monad (MonadPlus (mplus))
import Control.Monad.State (State, modify, runState)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.List (tails, unfoldr)
import qualified Data.Map as M
import Distribution.Utils.MD5 (md5, showMD5)

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn chr = unfoldr sep
  where
    sep [] = Nothing
    sep l = Just . fmap (drop 1) . break (== chr) $ l

splitOneOf :: Eq a => [a] -> [a] -> [[a]]
splitOneOf cs = unfoldr sep
  where
    sep [] = Nothing
    sep l = Just . fmap (drop 1) . break (`elem` cs) $ l

-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell/7108719#7108719
-- added in explicit typing. not needed but i find it helping...
makeCountMap :: [Char] -> [(Char, Int)]
makeCountMap inputString =
  M.toList $ M.fromListWith (+) [(c, 1) | c <- inputString]

splitLast :: Char -> [Char] -> [[Char]]
splitLast c s = do
  let z = splitOn c s
  let j = length z -1
  if j > 0
    then [concat (take j z), z !! j]
    else [s]

removePunc xs = [x | x <- xs, x `notElem` ",.?!-:;\"\'[]"]

safeHead [] = Nothing
safeHead (x : _) = Just x

hashString :: String -> String
hashString = showMD5 . md5 . B.pack . map (fromIntegral . ord)

-- | Generate sliding windows from a list
windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

-- | Split a list on first occurrence of some element. Drops the delimiter.
-- If there is no delimiter, all is returned in the first component.
-- I think this State monad solution was cute. Instead of passing an accumulator, I
-- just use a State as an acculumator.
-- Since the sequences are very short, I use linked lists, despite the append-rights.
splitFirst :: Eq a => a -> [a] -> ([a], [a])
splitFirst y xs = runState (go y xs) []
  where
    go y [] = pure [] :: State [a] [a]
    go y (x : xs)
      | y == x = pure xs
      | otherwise = modify (\s -> s ++ [x]) >> go y xs

------------------------------------------------------------------------------------
-- Inspired or stolen from https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html

repeatM n f
  | n <= 1 = f
  | otherwise = f >> repeatM (n -1) f -- do monadic action n times

untilM_ ::
  (Monad m) =>
  m a -> -- the action to perform
  m Bool -> -- the predicate to check
  m ()
action `untilM_` predicate = do
  action
  b <- predicate
  if b then pure () else action `untilM_` predicate

whileM' :: (Monad m) => m Bool -> m a -> m [a]
whileM' p f = go
  where
    go = do
      x <- p
      if x
        then do
          x <- f
          xs <- go
          return (x : xs)
        else return []

untilM' :: (Monad m) => m a -> m Bool -> m [a]
f `untilM'` p = do
  x <- f
  xs <- whileM' (fmap not p) f
  return (x : xs)
