{- stack
  script
  --resolver lts-10.3
-}
{- LanguageExtensions OverloadedStrings -}
import Data.Text (splitOn, unpack)
import Data.String (fromString)
import Control.Monad.State (State, state, get, put, runState, modify, execState, withState, evalState)
import Control.Monad (replicateM_, (<=<))

data Instruction = Ins TurnDirection Int deriving Show;
data Direction = R | L | U | D deriving Show;
data TurnDirection = CW | CCW deriving Show;
type Coordinate = (Int,Int);
type Trail = [Coordinate];
type FoundHQ = Bool

parseInstruction ('R':ns) = Ins CW $ read ns
parseInstruction ('L':ns) = Ins CCW $ read ns

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

manhattanDist :: Coordinate -> Int
manhattanDist (x,y) = abs x + abs y 

stringToInstructions x =
    let y =  splitOn (fromString ", ") (fromString x)
        ins = map (parseInstruction . unpack) y
    in ins


taskA' :: String -> IO ()
taskA' x = do
    let ins =  stringToInstructions x
        compositeMove = mapM_ goA ins
        finalPos = compositeMove >> do
            (trail,_) <- get
            return $ head trail
        startState = ([(0,0)],U)
    print $ manhattanDist $ evalState finalPos startState

taskB' x = do
    let ins =  stringToInstructions x
        compositeMove = mapM_ goB ins
        finalPos = compositeMove >> do
            (trail,_,_) <- get
            return $ head trail
        finalDist = fmap manhattanDist finalPos
        startState = ([(0,0)], U, False)
    print $ evalState finalDist startState

-- Tests --------------------------------------------------------------
testSnippets = sequence_ [
    print $ Ins CW 1,
    print $ parseInstruction "L132",
    print $ splitOn (fromString ", ") (fromString "asd, gre, asd, qw11"),
    print $ go1 U [(0,0)],
    print $ go1 D [(0,0)],
    print $ go1 R [(0,0)],
    print $ go1 L [(0,0)],
    print $ execState (goA (Ins CW 3)) ([(0,0)],U),
    print $ execState (mapM_ goA [Ins CW 1, Ins CCW 1]) ([(0,0)],U),
    taskA' "R2, L3",
    taskA' "R2, R2, R2",
    taskA' "R5, L5, R5, R3",
    taskB' "R8, R4, R4, R8" --should return 4
    ]

-- TASK A ------------------------------
taskA = do
    x <- readFile "data.txt"
    taskA' x

-- TASK B -------------------------------------
taskB = do
    x <- readFile "data.txt"
    taskB' x

-- RUNNER -------------------------------------
-- main = testSnippets -- only tests
main = sequence_ [testSnippets, taskA, taskB] -- show test output
-- main = sequence_ [taskA, taskB] -- dont show test output
