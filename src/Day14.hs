{-# LANGUAGE Arrows #-}

module Day14 where

import Util (hashString,safeHead)
import Data.List (tails,group)


import Control.Arrow
import qualified Control.Category as Cat
import qualified Data.Sequence as SQ
import Data.Sequence (Seq(..))
import Data.Maybe (isJust)

{-| Curcuits are from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Hangman:_Main_program
 They are neat, since they hold accumulators, so they are stateful. :)
 It seems, that the trend in general is away from arrows, towards applicatives, but I kind of like arrows :)
 -}
newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }
instance Cat.Category Circuit where
    id = Circuit $ \a -> (Cat.id, a)
    (.) = dot
      where
        (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
            let (cir1', b) = cir1 a
                (cir2', c) = cir2 b
            in  (cir2' `dot` cir1', c)
instance Arrow Circuit where
    arr f = Circuit $ \a -> (arr f, f a)
    first (Circuit cir) = Circuit $ \(b, d) ->
        let (cir', c) = cir b
        in  (first cir', (c, d))
runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _   []     = []
runCircuit cir (x:xs) =
    let (cir',x') = unCircuit cir x
    in  x' : runCircuit cir' xs

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in  (accum acc' f, output)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b in (b', b'))
total = accum' 0 (+)
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

-----------------------------------------------------------------------------------------------
-- END OF THE CIRCUIT EXAMPLE CODE
-----------------------------------------------------------------------------------------------

slide :: Int -> Circuit a (Maybe (SQ.Seq a))
slide n =
    let step a acc = (b, acc2)
         where acc2 = if length acc < n then acc SQ.:|> a else SQ.drop 1 (acc SQ.:|> a)
               b = if length acc2 < n then Nothing else Just acc2
    in accum SQ.empty step

firstTriplet :: String -> Maybe Char
firstTriplet =  fmap snd . safeHead . filter fst . map ( \s -> (3 <= length s,head s)) . group

allQuintuplets :: String -> [Char]
allQuintuplets =  fmap snd . filter fst . map ( \s -> (5 <= length s,head s)) . group

collapseSeq ::  Seq (Maybe Char, [Char]) -> Maybe (Char,[Char])
collapseSeq Empty = Nothing
collapseSeq (h :<| t) = case fst h of
                              Nothing -> Nothing
                              Just c -> Just (c, quints)
                                          where quints = concatMap snd t

inputSalt = "ngcjuoqr" -- the input of the day
testSalt = "abc"

pipe salt hash = arr (\x -> x-999) &&& (arr (\x -> salt++show x)
        >>> arr hash
        >>> arr firstTriplet &&& arr allQuintuplets
        >>> slide 1000
        >>> arr (>>= collapseSeq)
        >>> Cat.id &&& arr (maybe False (uncurry elem))
        )

main = do
    print . (!! 63) . filter (snd . snd) .  runCircuit (pipe inputSalt hashString) $ [1..] -- check that the 64th element (index 63) in that list comes from original index 18626, which is true answer. it  goes REALLY FAST to compute.
    mapM_ print $ take 64. filter (snd . snd) .  runCircuit (pipe inputSalt (\s -> iterate hashString s !! 2017) ) $ [1..] -- the answer is 20092, which is the last output. it takes ca 10 minutes to run though. That could well be 2017 times more than before.