module Day1 where
import Text.Parsec (oneOf,parse,digit,char,sepEndBy1,many1, string, (<|>))
import Control.Monad.State (State, state, get, put, runState, modify, execState, withState, evalState)
import Data.Either (Either(Right))

data Instruction = Ins TurnDirection Int deriving Show;
data Direction = R | L | U | D deriving Show;
data TurnDirection = CW | CCW deriving Show;
type Coordinate = (Int,Int);
type Trail = [Coordinate];
type FoundHQ = Bool

turn CW U = R
turn CW R = D
turn CW D = L
turn CW L = U
turn CCW U = L
turn CCW L = D
turn CCW D = R
turn CCW R = U

goA :: Instruction -> State (Trail, Direction) ()
goA (Ins turnDir num) = do
    (trail,dir) <- get
    let dir' = turn turnDir dir
        trail' = iterate (go1 dir') trail !! num
    put (trail', dir')
    return ()


goB :: Instruction -> State (Trail, Direction, FoundHQ) ()
goB (Ins turnDir num) = do
    (trail,dir, foundHQ) <- get
    if foundHQ
    then return ()
    else do
        let dir' = turn turnDir dir
        put (trail, dir', foundHQ)
        let loop True _ = return ()
            loop False n = do
                foundNow <- move1AndCheck
                let moveDone = foundNow || (n <= 1)
                loop moveDone (n-1)

        loop False num

move1AndCheck :: State (Trail, Direction, FoundHQ) FoundHQ
move1AndCheck = do
    (trail, dir, foundHQ) <- get
    let trail' = go1 dir trail
        cNow = head trail'
        hasBeenVisited = cNow `elem` trail
    put (trail', dir, hasBeenVisited)
    return hasBeenVisited

go1 :: Direction -> Trail -> Trail
go1 U trail@((x,y):_) = (x,y+1) : trail
go1 D trail@((x,y):_) = (x,y-1) : trail
go1 L trail@((x,y):_) = (x-1,y) : trail
go1 R trail@((x,y):_) = (x+1,y) : trail
go1 _ _ = error "Illegal ipnut?"

manhattanDist :: Coordinate -> Int
manhattanDist (x,y) = abs x + abs y

insP = (char 'L' *> (Ins CCW . read <$> many1 digit))
   <|> (char 'R' *> (Ins CW . read <$> many1 digit))
inputParser = insP `sepEndBy1` string ", "

-- If we got any successful parse, take it. If we did not, say we just parsed zero instances of it
unWrapMonoid (Right x) = x
unWrapMonoid (Left _) = mempty

stringToInstructions :: String -> [Instruction]
stringToInstructions x = unWrapMonoid $ parse inputParser "" x


taskA' x =
    let ins =  stringToInstructions x
        compositeMove = mapM_ goA ins
        finalPos = compositeMove >> do
            (trail,_) <- get
            return $ head trail
        startState = ([(0,0)],U)
    in manhattanDist $ evalState finalPos startState

taskB' x =
    let ins =  stringToInstructions x
        compositeMove = mapM_ goB ins
        finalPos = compositeMove >> do
            (trail,_,_) <- get
            return $ head trail
        finalDist = fmap manhattanDist finalPos
        startState = ([(0,0)], U, False)
    in evalState finalDist startState

-- TASK A ------------------------------
taskA = do
    x <- readFile "inputs/day01.txt"
    print$ taskA' x

-- TASK B -------------------------------------
taskB = do
    x <- readFile "inputs/day01.txt"
    print $ taskB' x

-- RUNNER -------------------------------------
main = taskA >> taskB
