module Day10 where

import Data.Map (insert,Map,insertWith,findWithDefault, fromList)
import Control.Monad.State
    ( MonadState(get, put), evalState, State )
import Data.List (foldr, isPrefixOf, groupBy, sortOn)
import Util (splitOn,repeatM)

import Data.Bifunctor (first, second)

data Command =  Input {recipient :: Target
                    , chipValue :: Chip} |
                TransferRule {bot :: Target
                           , lowOutput :: Target
                           , highOutput :: Target}
                deriving (Show)
data Target = Bot Int |
              Output Int 
              deriving (Show,Eq,Ord)
newtype Chip = Chip Int deriving Show
type BotSlot = (Int,[Int])

parseInputCommand :: [String] -> Command
parseInputCommand s = Input (Bot (read (last s)::Int))  (Chip (read (head s)::Int))

parseTransferRuleCommand :: [String] -> Command
parseTransferRuleCommand (bi:_:_:_:"bot":i1:_:_:_:"bot":i2:xs)       = TransferRule (Bot (read bi::Int)) (Bot    (read i1::Int)) (Bot    (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"bot":i1:_:_:_:"output":i2:xs)    = TransferRule (Bot (read bi::Int)) (Bot    (read i1::Int)) (Output (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"output":i1:_:_:_:"bot":i2:xs)    = TransferRule (Bot (read bi::Int)) (Output (read i1::Int)) (Bot    (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"output":i1:_:_:_:"output":i2:xs) = TransferRule (Bot (read bi::Int)) (Output (read i1::Int)) (Output (read i2::Int))

parseLinetoCommand :: [String] -> Command
parseLinetoCommand (first:rest)
  | first == "bot" = parseTransferRuleCommand rest
  | first == "value" = parseInputCommand rest

isInput :: Command -> Bool
isInput (Input _ _) = True
isInput _         = False
isRule :: Command -> Bool
isRule TransferRule{} = True
isRule _ = False

isBot :: Target -> Bool
isBot (Bot _) = True
isBot _ = False

createBotSlot :: Int -> (Int,[Int])
createBotSlot x = (x, [])

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

botHasTwo :: BotSlot -> Bool
botHasTwo (i,[]) = False
botHasTwo (i,[a]) = False
botHasTwo (i,a:b:_) = True

getTargetInt :: Target -> Int
getTargetInt (Bot i) = i
getTargetInt (Output i) = i


applyOneInput :: [BotSlot] -> Command -> [BotSlot]
applyOneInput xs (Input (Bot i) (Chip j)) = replaceAtIndex i (i,j:snd (xs !! i)) xs -- add chip j to bot i
applyOneInput _ _ = []

applyOneTransfer :: [BotSlot] -> Command -> [BotSlot]
applyOneTransfer botList (TransferRule (Bot i) targetLow targetHigh)
  | not ( botHasTwo $ botList !! i ) = botList -- bot dont have two chips - dont pass any chips on!
  | otherwise = do 
    let lowChip = minimum ( snd $ botList !! i) ::Int
    let highChip = maximum $ snd $ botList !! i
    let resetGiveBot = replaceAtIndex i (i,[]::[Int]) botList
    let lowInt = getTargetInt targetLow
    let highInt = getTargetInt targetHigh
    let newBotSlotLow = (lowInt,lowChip:snd (resetGiveBot !! lowInt))
    let newBotSlotHigh = (highInt,highChip:snd (resetGiveBot !! highInt))
    let giveLow = if isBot targetLow 
            then  replaceAtIndex lowInt newBotSlotLow resetGiveBot
            else  resetGiveBot
    let giveHigh = if isBot targetHigh 
            then  replaceAtIndex highInt newBotSlotHigh giveLow
            else  giveLow
    giveHigh
applyOneTransfer _ _ = []


-- if we are done, return Right and the bot number, else return the list itself.
checkRound :: [BotSlot]  -> Either [BotSlot] Int
checkRound x
  | any myBotGetter x = Right $ fst $ head $ filter myBotGetter x
  | otherwise = Left x
  where myBotGetter = \bot -> botHasTwo bot && (elem 17 . snd) bot && (elem 61 . snd) bot

applyTransfers :: Either [BotSlot] Int  -> [Command] -> Either [BotSlot] Int
applyTransfers (Right x) _  = Right x
applyTransfers (Left botList) (x:xs) = applyTransfers (checkRound (applyOneTransfer botList x)) xs
applyTransfers (Left botList) _ = Left botList

applyTransferLoop :: Either [BotSlot] Int  -> [Command] -> Either [BotSlot] Int
applyTransferLoop (Right x) _  = Right x
applyTransferLoop (Left botList) [] = Left botList
applyTransferLoop (Left botList) x = applyTransferLoop (Left botList) x


partA x =
  let asdf = map (parseLinetoCommand . words) $ lines x
      (rules,commandsFeedingBotsInputChips) =foldl (\s c -> if isRule c then first (c :) s else second (c :) s ) ([],[]) asdf
      botMaxNr = maximum $ filter isBot $ map recipient commandsFeedingBotsInputChips ++ map bot rules ++ filter isBot ( map highOutput rules) ++ filter isBot ( map lowOutput rules)
      outputMaxNr = maximum $ filter (not.isBot) (map highOutput rules) ++ filter (not.isBot) ( map lowOutput rules)
      -- we can let (Int,[Maybe Int]) represent a bot with id Int and a list of its 0, 1 or 2 chips
      null_state = map createBotSlot [0..(getTargetInt botMaxNr)]
      -- go through all the commandsFeedingBotsInputChips crap (feed the relevant bots wit their first chip!)
      s0 = foldl applyOneInput null_state commandsFeedingBotsInputChips
      -- check if the test is on initial state
      -- do one more round
      s1 = applyTransfers (Left s0) rules
      s2 = applyTransfers s1 rules
      s3 = applyTransfers s2 rules
      s4 = applyTransfers s3 rules
      s5 = applyTransfers s4 rules
  -- five rounds was enough...
  in s5



-------
-- B --
-------

type Node = String
type NetworkState = Map Node [Int]
type Rule = (Node,Node,Node) -- (from, toLow, toHi)

swapPair:: (a,b) -> (b,a)
swapPair (a,b) = (b,a)

getInputs :: [String] -> [(Int,Int)]
getInputs x = 
    map (getTagetAndChip . splitOn ' ') $
    filter (isPrefixOf "value")  x
    where getTagetAndChip (_:val:_:_:_:botNum:_) = (read val, read botNum) -- :: [String] -> (Int,Int)

makeInitialState inputLines = 
    let inputs = getInputs inputLines
        tmp  = groupBy (\a b-> fst a == fst b) . sortOn fst $ map swapPair inputs
        tmp2 = [ ("bot " ++ show ( fst (head x)), map snd x) | x <- tmp] :: [(Node,[Int])]
    in fromList tmp2 :: NetworkState

getRuleset :: [String] -> [Rule ]
getRuleset x = 
    map (getRule . splitOn ' ') $
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

applyAllRules :: [Rule] -> State NetworkState ()
applyAllRules ruleset = do
  state <- get
  let state' = foldr applyOneRule state ruleset
  put state'

myStateProcessing :: [Rule] -> State NetworkState Int
myStateProcessing ruleset = do
    repeatM 100 (applyAllRules ruleset) -- 100 was more than enough to converge, it seems
    -- I should probably do the stateful control flow of https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html and make a loop until all chips are in outputs
    state <- get
    return $ head (findWithDefault [] "output 0" state ) * head (findWithDefault [] "output 1" state ) *head (findWithDefault [] "output 2" state )


partB x = 
    let inputLines = lines x :: [String]
        initialNetworkState = makeInitialState inputLines ::NetworkState
        ruleSet = getRuleset inputLines
    in evalState (myStateProcessing ruleSet) initialNetworkState

main = do
  x <- readFile "inputs/day10.txt"
  print $ partA x -- see that 27 is in the output state, which is correct
  print $ partB x --13272