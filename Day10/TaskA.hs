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

botHasTwo :: (Int,[Int]) -> Bool
botHasTwo (i,[]) = False
botHasTwo (i,(a:[])) = False
botHasTwo (i,(a:b:_)) = True

getTargetInt :: Target -> Int
getTargetInt (Bot i) = i
getTargetInt (Output i) = i


applyOneInput :: [(Int,[Int])] -> Command -> [(Int,[Int])]
applyOneInput xs (Input (Bot i) (Chip j)) = replaceAtIndex i (i,j:(snd (xs !! i))) xs
applyOneInput _ _ = []


checkRound :: [(Int,[Int])]  -> Either [(Int,[Int])] Int
checkRound x
	| length ( filter (\x -> (elem 17 snd x) && (elem 61 snd x) $ filter botHasTwo x ) )> 0 = 
		Right fst head (filter (\x -> (elem 17 snd x) && (elem 61 snd x) $ filter botHasTwo x ))

-- do all magic here.
main = do
  x <- readFile "input.txt"
  let asdf = map parseLinetoCommand $ map words $ lines x
  let (rules,input) =foldl (\s c -> if isRule c then (c:(fst s),snd s) else (fst s ,c:(snd s)) ) ([],[]) asdf
  let bM = maximum $ filter isBot $ (map recipient input) ++ (map bot rules) ++ (filter isBot $ map highOutput rules) ++ (filter isBot $ map lowOutput rules)
  let oM = maximum $ (filter (not.isBot) $ map highOutput rules) ++ (filter (not.isBot) $ map lowOutput rules)

  -- we can let (Int,[Maybe Int]) represent a bot with id Int and a list of its 0, 1 or 2 
  let null_state = map createBotSlot [0..(getTargetInt bM)]
  let start_state = foldl applyOneInput null_state input
  let theBotWeLookFor = checkRound start_state

  --putStrLn $ show $ rules
  --putStrLn $ show $ input
  --putStrLn $ show $ bM
  --putStrLn $ show $ i1
  putStrLn $ show $ theBotWeLookFor