{- stack
  script
  --resolver lts-10.3
-}
import Data.Map (insert,Map,insertWith,findWithDefault, fromList)
import Control.Monad.State
import Data.List (foldr, isPrefixOf, groupBy, sortOn)
import Data.List.Split (splitOn)

type Node = String
type NetworkState = Map Node [Int]
type Rule = (Node,Node,Node) -- (from, toLow, toHi)

swapPair:: (a,b) -> (b,a)
swapPair (a,b) = (b,a)

getInputs :: [String] -> [(Int,Int)]
getInputs x = 
    map (getTagetAndChip . splitOn " ") $
    filter (isPrefixOf "value")  x
    where getTagetAndChip (_:val:_:_:_:botNum:_) = (read val, read botNum) -- :: [String] -> (Int,Int)

makeInitialState inputLines = 
    let inputs = getInputs inputLines
        tmp  = groupBy (\a b-> fst a == fst b) . sortOn fst $ map swapPair inputs
        tmp2 = [ ("bot " ++ show ( fst (head x)), map snd x) | x <- tmp] :: [(Node,[Int])]
    in fromList tmp2 :: NetworkState

getRuleset :: [String] -> [Rule ]
getRuleset x = 
    map (getRule . splitOn " ") $
    filter (isPrefixOf "bot")  x
    where getRule (_:fromNum:_:_:_:loType:loNum:_:_:_:hiType:hiNum:_) = ("bot "++fromNum, loType++" "++loNum, hiType++" "++hiNum ) :: Rule

applyOneRule :: Rule -> NetworkState -> NetworkState
applyOneRule (from,toLow,toHi) state =  if nChips < 2 then state else updatedState
    where nChips = length(findWithDefault [] from state)
          chips = findWithDefault [] from state -- we get a bug if the bot holds more than 2 chips!
          chipMin = minimum chips
          chipMax = maximum chips
          update1 = insertWith (++) toLow [chipMin]  state -- send to lo
          update2 = insertWith (++) toHi [chipMax]  update1 -- send to Hi
          update3 = insert from [] update2 -- empty the old list
          updatedState = update3


myStateProcessing :: [Rule] -> State NetworkState Int
myStateProcessing ruleset = do
    networkState <- get
    let applyAllRules state = foldr applyOneRule state ruleset
    let updatedState = iterate applyAllRules networkState !! 100 -- apply the rule n times. should be enough to stop...
    -- I should probably do the stateful control flow of https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html and make a loop until all chips are in outputs
    -- In the code here, I *really* did not need the state monad. but wtf... I am getting there....
    put updatedState
    return $ head (findWithDefault [] "output 0" updatedState) * head (findWithDefault [] "output 1" updatedState) *head (findWithDefault [] "output 2" updatedState)


main = do

    x <- readFile "input.txt"
    let inputLines = lines x :: [String]
    let initialNetworkState = makeInitialState inputLines ::NetworkState
    let ruleSet = getRuleset inputLines

    let (output,networkState) = runState (myStateProcessing ruleSet) initialNetworkState

    print output -- 13272 iscorrect