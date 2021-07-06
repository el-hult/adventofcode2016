{-# LANGUAGE Arrows #-}

{-|
Module      :  Day14
Maintainer  :  ludvig.hult@gmail.com

= Day 14
This is my solution to Day 14 in the 2016 installment of Advent of Code. 
The full description can be found [here](https://adventofcode.com/2016/day/14)

= Solution structure
I realized that we were to do a data pipeline (a set of incoming increments transformed by a set of rules and then filtered)
so I thought it would be time to learn about Arrows.

It seems Arrows is an invention from 1998 and has since fallen slightly out of style. Often, a monad or a applicative is
at least as expressive, and they are more flexible.

Still, the arrow style i suitable whn doing things that seems like a pipeline, and several libraries use it, so I have used them here.

The specific Arrow is one called `Circuit`, and I have stolen it from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Hangman:_Main_program
It is essentially stateful mappings over lists.

There are some more recent readings on arrows. I have ordered them in the way I think one should read them.

- https://www.youtube.com/watch?v=W_NARxJEU5I
- https://stackoverflow.com/questions/44632182/haskell-arrow-tutorial-loop-state
- https://blog.paulme.ng/posts/2012-04-22-haskell-arrow.html
- https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Arrow.html

Lastly, there is a GHC extension that introduces so-called Arrow syntax. It is a variant of do-notation for arrows. I have enabled it
since the `Circuit` example uses it in `mean2`

-}
module Day14 where

import Util (hashString,safeHead)
import Data.List (tails,group)


import Control.Arrow
import qualified Control.Category as Cat
import qualified Data.Sequence as SQ
import Data.Sequence (Seq(..))
import Data.Maybe (isJust)

-- * The Cicruit Code

{-|
 Curcuits are from https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Hangman:_Main_program
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

{-| Run a circuit over a list, emitting a new list.
Fulfils the rule 

> map f xs = runCircuit (id f) xs

but circuits can also be stateful, and are thus more expressive than normal maps
-}
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


-- ** Circuit application examples
total = accum' 0 (+)
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 = proc value -> do
    t <- total -< value
    n <- total -< 1
    returnA -< t / n

-- * My own Solution code

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

{-|
The Meat of my solution. a *Pipeline*. 
It transforms integers into nested tuples that say what index they are from, what characters are relevant
for identification of triplets etc and if they are a valid OTP.
-}
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