{- stack
  script
  --resolver lts-10.3
  --package regex-posix
-}
data Command =  Input {recipient :: Target
                    , chipValue :: Chip} |
                TransferRule {bot :: Target
                           , lowOutput :: Target
                           , highOutput :: Target}
                deriving (Show)
data Target = Bot Int |
              Output Int 
              deriving (Show,Eq,Ord)
data Chip = Chip Int
            deriving (Show)

type BotSlot = (Int,[Int])

parseInputCommand :: [String] -> Command
parseInputCommand s = Input (Bot (read (last s)::Int))  (Chip (read (head s)::Int))

parseTransferRuleCommand :: [String] -> Command
parseTransferRuleCommand (bi:_:_:_:"bot":i1:_:_:_:"bot":i2:xs)       = TransferRule (Bot (read (bi)::Int)) (Bot    (read i1::Int)) (Bot    (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"bot":i1:_:_:_:"output":i2:xs)    = TransferRule (Bot (read (bi)::Int)) (Bot    (read i1::Int)) (Output (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"output":i1:_:_:_:"bot":i2:xs)    = TransferRule (Bot (read (bi)::Int)) (Output (read i1::Int)) (Bot    (read i2::Int))
parseTransferRuleCommand (bi:_:_:_:"output":i1:_:_:_:"output":i2:xs) = TransferRule (Bot (read (bi)::Int)) (Output (read i1::Int)) (Output (read i2::Int))

parseLinetoCommand :: [String] -> Command
parseLinetoCommand (first:rest)
  | first == "bot" = parseTransferRuleCommand $ rest
  | first == "value" = parseInputCommand $ rest

isInput :: Command -> Bool
isInput (Input _ _) = True
isInput _         = False
isRule :: Command -> Bool
isRule (TransferRule _ _ _) = True
isRule _ = False

isBot :: Target -> Bool
isBot (Bot _) = True
isBot _ = False

createBotSlot :: Int -> (Int,[Int])
createBotSlot x = (x, [])

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

botHasTwo :: BotSlot -> Bool
botHasTwo (i,[]) = False
botHasTwo (i,(a:[])) = False
botHasTwo (i,(a:b:_)) = True

getTargetInt :: Target -> Int
getTargetInt (Bot i) = i
getTargetInt (Output i) = i


applyOneInput :: [BotSlot] -> Command -> [BotSlot]
applyOneInput xs (Input (Bot i) (Chip j)) = replaceAtIndex i (i,j:(snd (xs !! i))) xs -- add chip j to bot i
applyOneInput _ _ = []

applyOneTransfer :: [BotSlot] -> Command -> [BotSlot]
applyOneTransfer botList (TransferRule (Bot i) targetLow targetHigh)
  | not ( botHasTwo $ botList !! i ) = botList -- bot dont have two chips - dont pass any chips on!
  | otherwise = do 
    let lowChip = (minimum ( snd $ botList !! i))::Int
    let highChip = maximum $ snd $ botList !! i
    let resetGiveBot = replaceAtIndex i (i,[]::[Int]) botList
    let lowInt = getTargetInt targetLow
    let highInt = getTargetInt targetHigh
    let newBotSlotLow = (lowInt,lowChip:(snd (resetGiveBot !! lowInt)))
    let newBotSlotHigh = (highInt,highChip:(snd (resetGiveBot !! highInt)))
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
  | length ( filter (\x -> ((elem 17 . snd) x) && ((elem 61 . snd) x)) ( filter botHasTwo x ) )> 0 = 
    Right $ fst $ head (filter (\x -> ((elem 17 . snd) x) && ((elem 61 .  snd) x)) ( filter botHasTwo x ))
  | otherwise = Left x

applyTransfers :: Either [BotSlot] Int  -> [Command] -> Either [BotSlot] Int
applyTransfers (Right x) _  = Right x
applyTransfers (Left botList) (x:xs) = applyTransfers (checkRound (applyOneTransfer botList x)) xs
applyTransfers (Left botList) _ = Left botList

applyTransferLoop :: Either [BotSlot] Int  -> [Command] -> Either [BotSlot] Int
applyTransferLoop (Right x) _  = Right x
applyTransferLoop (Left botList) [] = Left botList
applyTransferLoop (Left botList) x = applyTransferLoop (Left botList) x



-- do all magic here.
main = do
  x <- readFile "input.txt"
  let asdf = map parseLinetoCommand $ map words $ lines x
  let (rules,commandsFeedingBotsInputChips) =foldl (\s c -> if isRule c then (c:(fst s),snd s) else (fst s ,c:(snd s)) ) ([],[]) asdf
  let botMaxNr = maximum $ filter isBot $ (map recipient commandsFeedingBotsInputChips) ++ (map bot rules) ++ (filter isBot $ map highOutput rules) ++ (filter isBot $ map lowOutput rules)
  let outputMaxNr = maximum $ (filter (not.isBot) $ map highOutput rules) ++ (filter (not.isBot) $ map lowOutput rules)

  -- we can let (Int,[Maybe Int]) represent a bot with id Int and a list of its 0, 1 or 2 chips
  let null_state = map createBotSlot [0..(getTargetInt botMaxNr)]
  -- go through all the commandsFeedingBotsInputChips crap (feed the relevant bots wit their first chip!)
  let s0 = foldl applyOneInput null_state commandsFeedingBotsInputChips
  -- check if the test is on initial state
  

  -- do one more round
  let s1 = applyTransfers (Left s0) rules
  let s2 = applyTransfers s1 rules
  let s3 = applyTransfers s2 rules
  let s4 = applyTransfers s3 rules
  let s5 = applyTransfers s4 rules
  
  -- five rounds was enough...
  putStrLn $ show $ s5
