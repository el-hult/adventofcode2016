module Main where

import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
  )
import Data.Bifunctor (first, second)
import Data.List (groupBy, isPrefixOf, sortOn)
import Data.Map (Map, findWithDefault, fromList, insert, insertWith)
import Util (repeatM, splitOn, replaceAtIndex)

data Command
  = Input
      { recipient :: Target,
        chipValue :: Chip
      }
  | TransferRule
      { bot :: Target,
        lowOutput :: Target,
        highOutput :: Target
      }
  deriving (Show)

data Target
  = Bot Int
  | Output Int
  deriving (Show, Eq, Ord)

newtype Chip = Chip Int deriving (Show)

type BotSlot = (Int, [Int])

parseInputCommand :: [String] -> Command
parseInputCommand (s:ss) = Input (Bot (read (last ss) :: Int)) (Chip (read s :: Int))
parseInputCommand _ = error "Invalid input command format"

parseTransferRuleCommand :: [String] -> Command
parseTransferRuleCommand (bi : _ : _ : _ : "bot" : i1 : _ : _ : _ : "bot" : i2 : _) = TransferRule (Bot (read bi :: Int)) (Bot (read i1 :: Int)) (Bot (read i2 :: Int))
parseTransferRuleCommand (bi : _ : _ : _ : "bot" : i1 : _ : _ : _ : "output" : i2 : _) = TransferRule (Bot (read bi :: Int)) (Bot (read i1 :: Int)) (Output (read i2 :: Int))
parseTransferRuleCommand (bi : _ : _ : _ : "output" : i1 : _ : _ : _ : "bot" : i2 : _) = TransferRule (Bot (read bi :: Int)) (Output (read i1 :: Int)) (Bot (read i2 :: Int))
parseTransferRuleCommand (bi : _ : _ : _ : "output" : i1 : _ : _ : _ : "output" : i2 : _) = TransferRule (Bot (read bi :: Int)) (Output (read i1 :: Int)) (Output (read i2 :: Int))
parseTransferRuleCommand _ = error "Invalid transfer rule command format"

parseLinetoCommand :: [String] -> Command
parseLinetoCommand (x : xs)
  | x == "bot" = parseTransferRuleCommand xs
  | x == "value" = parseInputCommand xs
parseLinetoCommand _ = error "Invalid command format"

isInput :: Command -> Bool
isInput (Input _ _) = True
isInput _ = False

isRule :: Command -> Bool
isRule TransferRule {} = True
isRule _ = False

isBot :: Target -> Bool
isBot (Bot _) = True
isBot _ = False

createBotSlot :: Int -> (Int, [Int])
createBotSlot x = (x, [])

botHasTwo :: BotSlot -> Bool
botHasTwo (_, chips) = length chips == 2

getTargetInt :: Target -> Int
getTargetInt (Bot i) = i
getTargetInt (Output i) = i

applyOneInput :: [BotSlot] -> Command -> [BotSlot]
applyOneInput xs (Input (Bot i) (Chip j)) = replaceAtIndex i (i, j : snd (xs !! i)) xs -- add chip j to bot i
applyOneInput _ _ = []

applyOneTransfer :: [BotSlot] -> Command -> [BotSlot]
applyOneTransfer botList (TransferRule (Bot i) targetLow targetHigh)
  | not (botHasTwo $ botList !! i) = botList -- bot dont have two chips - dont pass any chips on!
  | otherwise = do
    let lowChip = minimum (snd $ botList !! i) :: Int
    let highChip = maximum $ snd $ botList !! i
    let resetGiveBot = replaceAtIndex i (i, [] :: [Int]) botList
    let lowInt = getTargetInt targetLow
    let highInt = getTargetInt targetHigh
    let newBotSlotLow = (lowInt, lowChip : snd (resetGiveBot !! lowInt))
    let newBotSlotHigh = (highInt, highChip : snd (resetGiveBot !! highInt))
    let giveLow =
          if isBot targetLow
            then replaceAtIndex lowInt newBotSlotLow resetGiveBot
            else resetGiveBot
    let giveHigh =
          if isBot targetHigh
            then replaceAtIndex highInt newBotSlotHigh giveLow
            else giveLow
    giveHigh
applyOneTransfer _ _ = []

-- if we are done, return Right and the bot number, else return the list itself.
checkRound :: [BotSlot] -> Either [BotSlot] Int
checkRound x
  | any myBotGetter x = case filter myBotGetter x of 
    (b : _) -> Right $ fst b
    _ -> error "unreachable"
  | otherwise = Left x
  where
    myBotGetter = \b -> botHasTwo b && (elem 17 . snd) b && (elem 61 . snd) b

applyTransfers :: Either [BotSlot] Int -> [Command] -> Either [BotSlot] Int
applyTransfers (Right x) _ = Right x
applyTransfers (Left botList) (x : xs) = applyTransfers (checkRound (applyOneTransfer botList x)) xs
applyTransfers (Left botList) _ = Left botList

applyTransferLoop :: Either [BotSlot] Int -> [Command] -> Either [BotSlot] Int
applyTransferLoop (Right x) _ = Right x
applyTransferLoop (Left botList) [] = Left botList
applyTransferLoop (Left botList) x = applyTransferLoop (Left botList) x

partA :: String -> Either [BotSlot] Int
partA x =
  let asdf = map (parseLinetoCommand . words) $ lines x
      (rules, commandsFeedingBotsInputChips) = foldl (\s c -> if isRule c then first (c :) s else second (c :) s) ([], []) asdf
      botMaxNr = maximum $ filter isBot $ map recipient commandsFeedingBotsInputChips ++ map bot rules ++ filter isBot (map highOutput rules) ++ filter isBot (map lowOutput rules)
      -- we can let (Int,[Maybe Int]) represent a bot with id Int and a list of its 0, 1 or 2 chips
      null_state = map createBotSlot [0 .. (getTargetInt botMaxNr)]
      -- go through all the commandsFeedingBotsInputChips crap (feed the relevant bots wit their first chip!)
      s0 = foldl applyOneInput null_state commandsFeedingBotsInputChips
      -- check if the test is on initial state
      -- do one more round
      s1 = applyTransfers (Left s0) rules
      s2 = applyTransfers s1 rules
      s3 = applyTransfers s2 rules
      s4 = applyTransfers s3 rules
      s5 = applyTransfers s4 rules
   in -- five rounds was enough...
      s5

-------
-- B --
-------

type Node = String

type NetworkState = Map Node [Int]

type Rule = (Node, Node, Node) -- (from, toLow, toHi)

swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

-- | getInputs x
-- finds all strings of the form "value 5 goes to bot 2" and returns a list of pairs (chip, bot)
getInputs :: [String] -> [(Int, Int)]
getInputs x =
  map (getTagetAndChip . splitOn ' ') $
    filter (isPrefixOf "value") x
  where
    getTagetAndChip (_ : val : _ : _ : _ : botNum : _) = (read val, read botNum) -- :: [String] -> (Int,Int)
    getTagetAndChip _ = error "Invalid input format"

-- | groupSort pairs
-- groups a list of pairs on the first element of the pair
groupSort :: (Ord a) => [(a,b)] -> [(a, [b])]
groupSort = map dedup . groupBy (\a b -> fst a == fst b) . sortOn fst
  where
    dedup xs@(x : _) = (fst x, map snd xs)
    dedup _ = error "unreachable"

makeInitialState :: [String] -> NetworkState
makeInitialState inputLines =
  let inputs = getInputs inputLines -- [(chip, bot)]
      botPerChip = map swapPair inputs -- [(bot, chip)]
      chipsPerBot = groupSort botPerChip :: [(Int,[Int])]
      tmp3 = map (first (\a -> "bot " ++ show a)) chipsPerBot -- [ ("bot 2", [5, 3]), ("bot 3", [2])] etc
   in fromList tmp3 :: NetworkState

getRuleset :: [String] -> [Rule]
getRuleset x =
  map (getRule . splitOn ' ') $
    filter (isPrefixOf "bot") x
  where
    getRule (_ : fromNum : _ : _ : _ : loType : loNum : _ : _ : _ : hiType : hiNum : _) = ("bot " ++ fromNum, loType ++ " " ++ loNum, hiType ++ " " ++ hiNum) :: Rule
    getRule _ = error "Invalid rule format"

applyOneRule :: Rule -> NetworkState -> NetworkState
applyOneRule (from, toLow, toHi) state = if nChips < 2 then state else updatedState
  where
    nChips = length (findWithDefault [] from state)
    chips = findWithDefault [] from state -- we get a bug if the bot holds more than 2 chips!
    chipMin = minimum chips
    chipMax = maximum chips
    update1 = insertWith (++) toLow [chipMin] state -- send to lo
    update2 = insertWith (++) toHi [chipMax] update1 -- send to Hi
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
  let o1 = case findWithDefault [] "output 0" state of
        [] -> error "Nothing in output 0"
        (x : _) -> x
  let o2 = case findWithDefault [] "output 1" state of
        [] -> error "Nothing in output 1"
        (x : _) -> x
  let o3 = case findWithDefault [] "output 2" state of
        [] -> error "Nothing in output 2"
        (x : _) -> x
  return $ o1 * o2 * o3

partB :: String -> Int
partB x =
  let inputLines = lines x :: [String]
      initialNetworkState = makeInitialState inputLines :: NetworkState
      ruleSet = getRuleset inputLines
   in evalState (myStateProcessing ruleSet) initialNetworkState

main :: IO ()
main = do
  x <- readFile "inputs/day10.txt"
  print $ partA x -- see that 27 is in the output state, which is correct
  print $ partB x --13272
