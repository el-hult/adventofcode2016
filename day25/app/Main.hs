{-
  
Day 25

We are to implement assembunny again.
This time, the question is to do some analysis of the program.

Part A:
What is the lowest positive integer to initizle register 'a' with, so that the program outputs 0,1,0,1, forever?
The program has no `tgl` instruction, so static analysis is fine.
It has a single `out` instruction, which is `out b` so we need to know when we would have 0 and 1 alternatingly in register 'b' when arriving at that instruction.
I also find a couple of `jnz 0 0` instructions, which is a noop, indicating that part two of the problem will be gnarlier.
What does a pen-and-paper analysis say?
I don't know. It was harder. 
So I implemented an interactive interpreter (with StateT over IO) that did the pen-and-paper analysis for me.
I realized there was a multiplication loop in the code, so I implemented a quick step for that.
Then I found an obscured multiplier! it has a jump in it! My normal quick step cannot handle that.
I also feel it is not a normal AoC thing to solve that with dynamic analysis either.
But it seemed pretty fast. For a few example inputs, it was plenty fast!
So instead of writing a multiplier with dynamic analysis, maybe we should just implement loop detection?
At a first try, I never got stuck in infinite loops (jnz or so...) except loops that did output, 
so I think I should record the state I am in when outputting, and see if I return to states.

My hypothesis is that we are looking for a "simple loop", meaning that we have registers in state r1
Next time we are asked to run the Out instruction, we are in state r2
Third time we are asked to run the Out instruction, we are in state r3
If r1==r3, then we have a simple loop!
We also must check that b2=1-b1, where bj is the b register value when we output in state rj.
We utilize the fact that the program is
 - not self-modifying
 - has only one `out` instruction.
 - that is `out b`
Therefore, the state when outputting is just determined by the registers and nothing more
This solution was too slow, it seemes. or buggy. But here it is.

-}
import Control.Monad.State
import Control.Monad (when, guard, forM, mapM, forM_)
import Util (replaceAtIndex)
import Data.List ((!?))
import Debug.Trace (trace)


type Registers = (Int, Int, Int, Int)
type Program = [Instruction]

data Interpreter = Interpreter
  { pc :: Int -- program counter
  , regs :: Registers
  , prog :: Program
  } deriving (Show)


data Val = 
  -- | Integer value
  I Int 
  -- | Register value
  -- INVARIANT: The register names are always a, b, c or d
  | R Char deriving (Show, Eq)

toVal :: String -> Val
toVal s@(c:_) 
  | c `elem` ['a', 'b', 'c', 'd'] = R c
  | otherwise = I (read s)
toVal _ = error "unreachable"

data Instruction = 
  Cpy Val Val
  | Inc Val
  | Dec Val
  | Jnz Val Val
  | Tgl Val
  | Out Val
  deriving (Show)

toInst :: [String] -> Instruction
toInst ["cpy", x, y] = Cpy (toVal x) (toVal y)
toInst ["inc", x] = Inc (toVal x)
toInst ["dec", x] = Dec (toVal x)
toInst ["jnz", x, y] = Jnz (toVal x) (toVal y)
toInst ["tgl", x] = Tgl (toVal x)
toInst ["out", x] = Out (toVal x)
toInst _ = error "unreachable"

-- | get the value of a register
getR :: Registers -> Char -> Int
getR (a, _, _, _) 'a' = a
getR (_, b, _, _) 'b' = b
getR (_, _, c, _) 'c' = c
getR (_, _, _, d) 'd' = d
getR _ _ = error "unreachable"

-- | set regs r 
setR ::  Char -> Int -> Registers -> Registers
setR 'a' x (_, b, c, d) = (x, b, c, d)
setR 'b' x (a, _, c, d) = (a, x, c, d)
setR 'c' x (a, b, _, d) = (a, b, x, d)
setR 'd' x (a, b, c, _) = (a, b, c, x)
setR _ _ _ = error "unreachable"

-- | modify regs r f
modifyR :: Char -> (Int -> Int) -> Registers -> Registers
modifyR r f regs = setR r (f (getR regs r)) regs

data ExitCode = BadLoop | GoodLoop | Terminated | Running deriving (Show, Eq)

run :: StateT (Interpreter, [Registers]) IO ExitCode
run = do
  (int, history) <- get
  let pc_ = pc int
  -- lift $ putStrLn $ "Registers: " ++ show (regs int)
  -- lift $ putStrLn $ "Running instruction at pc=" ++ show pc_ ++ ": " ++ show (prog int !? pc_)
  -- _ <- lift $ getLine
  let foundGoodLoop = case history of
            ((r1):(r2):(r3):_) -> (r1 == r3 && (getR r2 'b' == 1 - getR r1 'b'))
            _ -> False

  if foundGoodLoop
    then return GoodLoop -- we found a good loop, so we stop running
    else do
      -- general loop detection
      let foundBadLoop = case history of
                (r:_) -> (r `elem` (drop 1 history))
                _ -> False
      if foundBadLoop
        then return BadLoop
        else do
          if (pc_ > (length . prog $ int))
            then (lift $ print "Terminated") >> return Terminated
            else (step >> run)


-- | step 
-- will run the interpreter for one step
-- either it will take a quick step, or a slow step
step :: StateT (Interpreter,[Registers]) IO ()
step = do
  (int, history) <- get
  case quickStep int of
    Just newInt -> put (newInt, history)
    Nothing -> let 
      (newInt, output) = slowStep int
      newHistory = case output of
        True -> (regs newInt) : history
        False -> history
      in put (newInt, newHistory)

-- | quickStep state
-- check if we can do a multiply to speed things up.
-- 
-- cpy x r3
-- inc r1
-- dec r3
-- jnz r3 -2
-- dec r4
-- jnz r4 -5
-- ===> r1 = r1 + x * r4; r3=0; r4=0; pc+=6
--
-- where x is either a register or an integer.
--
-- returns Nothing if no optimization was possible
quickStep :: Interpreter -> Maybe Interpreter
quickStep s@(Interpreter pc regs prog) = case prog !? pc of
  Just (Cpy x (R r3)) -> case prog !? (pc + 1) of
    Just (Inc (R r1)) -> case prog !? (pc + 2) of
      Just (Dec (R r3')) | r3' == r3 -> case prog !? (pc + 3) of
        Just (Jnz (R r3'') (I y)) | r3'' == r3 && y == -2 -> case prog !? (pc + 4) of
          Just (Dec (R r4)) -> case prog !? (pc + 5) of
            Just (Jnz (R r4') (I y')) | r4' == r4 && y' == -5 ->
              let r1Val = getR regs r1
                  xVal = case x of
                    R r2 -> getR regs r2
                    I n -> n
                  r4Val = getR regs r4
                  regs' = setR r1 (r1Val + xVal * r4Val) . setR r3 0 . setR r4 0 $ regs
              in Just (Interpreter (pc+6) regs' prog)
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | slowStep state
-- run a single instruction of the interpreter
-- returns the new state of the interpreter, and a bool, indicating whether the instruction was an `out` instruction
slowStep :: Interpreter -> (Interpreter, Bool)
slowStep int@(Interpreter pc regs prog) =
  case prog !? pc of
    Just inst -> step' inst int
    Nothing -> error "Program counter out of bounds"

-- | step' inst regs prog pc
-- Run the instruction `inst` at the current state of the registers `regs`, program `prog` and instruction pointer `pc`.
-- Returns the new state of the registers, program and instruction pointer.
step' :: Instruction -> Interpreter -> (Interpreter, Bool)
step' (Cpy (I x) (R y)) (Interpreter pc regs prog) = (Interpreter (pc + 1) (setR y x regs) prog, False)
step' (Cpy (R x) (R y)) (Interpreter pc regs prog) = (Interpreter (pc + 1) (setR y (getR regs x) regs) prog, False)
step' (Inc (R x)) (Interpreter pc regs prog) = (Interpreter (pc + 1) (modifyR x (\v -> v+1) regs) prog, False)
step' (Dec (R x)) (Interpreter pc regs prog) = (Interpreter (pc + 1) (modifyR x (\v -> v-1) regs) prog, False)
step' (Jnz (R x) (I y)) (Interpreter pc regs prog) = (Interpreter (pc + if getR regs x /= 0 then y else 1) regs prog, False)
step' (Jnz (I x) (I y)) (Interpreter pc regs prog) = (Interpreter (pc + if x /= 0 then y else 1) regs prog, False)
step' (Jnz (I x) (R y)) (Interpreter pc regs prog) = (Interpreter (pc + if x /= 0 then getR regs y else 1) regs prog, False)
step' (Out (R r)) i@(Interpreter pc regs _) = 
  --(trace ("Output: " ++ show (getR regs r)) $
  ( i { pc = pc + 1 }, True)
step' inst _ = error ("Non-implemented instruction: " ++ show inst)

main :: IO ()
main = do
  program <- map (toInst .  words) . lines <$> readFile "inputs/day25.txt"


  let initializers = [0..10000]
  let initInt a = Interpreter { pc = 1, regs = (a, 0, 0, 0), prog = program }
  -- evalStateT will return the final output value
  -- execStateT will return the final state
  -- runStateT  will return (value, state), i.e. both!
  forM_ initializers $ \initA -> do 
    val <- evalStateT run (initInt initA, [])
    print ("Initial value: " ++ show initA ++ ", exit code: " ++ show val)


  