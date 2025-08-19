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

At this point, I grew bored and checked around online.
From the top post on reddit, it seems my approach should NOT work because the loops will not be simple.
Even when the program outputs 01010101... correctly, it will cycle through a couple of different states outputting this.
My loop detection must thus be more sophisticated.

So I implemented that, and it did not work. Why? Because of an off-by-one error. BLARGH. Haskell is 0-indexed, not 1-indexed.

With that, all was finally good!

-}
import Control.Monad.State (StateT, get, put, lift, evalStateT)
import Control.Monad (guard)
import Data.List ((!?))


type Registers = (Int, Int, Int, Int)
type Program = [Instruction]

data Interpreter = Interpreter
  { p :: Int -- program counter
  , a :: Int -- register a
  , b :: Int -- register b
  , c :: Int -- register c
  , d :: Int -- register d
  , prog :: Program
  } deriving (Show)


data Val = 
  -- | Integer value
  I Int 
  -- | Register value
  -- INVARIANT: The register names are always a, b, c or d
  | R Char deriving (Show, Eq)

toVal :: String -> Val
toVal s@(q:_) 
  | q `elem` ['a', 'b', 'c', 'd'] = R q
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
getR :: Char -> Interpreter -> Int
getR 'a' i = a i
getR 'b' i = b i
getR 'c' i = c i
getR 'd' i = d i
getR 'p' i = p i
getR _ _ = error "unreachable"

-- | set regs r 
setR ::  Char -> Int -> Interpreter -> Interpreter
setR 'a' x i = i { a = x }
setR 'b' x i = i { b = x }
setR 'c' x i = i { c = x }
setR 'd' x i = i { d = x }
setR 'p' x i = i { p = x }
setR _ _ _ = error "unreachable"

-- | stepP
-- advance the program counter by one
stepP :: Interpreter -> Interpreter
stepP i = modifyR 'p' (+1) i

-- | modify regs r f
modifyR :: Char -> (Int -> Int) -> Interpreter -> Interpreter
modifyR r f i = setR r v i where v = f (getR r i)

-- | regs
-- get all registers as a tuple
regs :: Interpreter -> Registers
regs int = (a int, b int, c int, d int)

data ExitCode = BadLoop | GoodLoop | Terminated | Running deriving (Show, Eq)

-- | loopDetection history
-- check if there are any loops in the history of registers
-- we want to find if there is a cycle in the history
-- the newest state is at the head of the list
-- if this state has occurred before, we have a loop
-- lets say the sequence is
-- r q w e t r a s d f g h j 
-- from our analysis of the code, we know that this program must revisit 
-- the Out instruction in this cycle going forwards from now
-- repeating the cycle from r to r.
-- N.B. the history is appended on the left, so the newest state is at the head of the list
-- i.e. we may have transient states before the loop
-- so finding this loop, we must simply check that the sequence going from RIGHT TO LEFT
-- has values 0 1 0 1 0 1 ... in the `b` register
loopDetection :: [Registers] -> Maybe ExitCode
loopDetection hist | length hist < 3 = Nothing
loopDetection (r:rs) | r `notElem` rs = Nothing -- no loop, we have not seen this state before
loopDetection hist@(r:rs) | r `elem` rs =
  let 
      expectedOutput = cycle [0, 1]
      realOutput = map (\(_,x,_,_) -> x) . reverse $ hist
      loopOutputIsGood = all (uncurry (==)) $ zip realOutput expectedOutput
  in  if loopOutputIsGood then Just GoodLoop else Just BadLoop
loopDetection _ = error "unreachable"
      
          
      

run :: StateT (Interpreter, [Registers]) IO ExitCode
run = do
  (int, history) <- get
  case loopDetection history of
    Just GoodLoop -> return GoodLoop
    Just BadLoop -> return BadLoop
    Just _ -> error "Unexpected loop detection result"
    Nothing -> do
      if (p int > (length . prog $ int))
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
quickStep s@(Interpreter{p=pc,prog=prog'}) = do
  Cpy x (R r3)         <- prog' !? pc
  Inc (R r1)           <- prog' !? (pc + 1)
  Dec (R r3')          <- prog' !? (pc + 2)
  guard (r3' == r3)
  Jnz (R r3'') (I y)   <- prog' !? (pc + 3)
  guard (r3'' == r3 && y == -2)
  Dec (R r4)           <- prog' !? (pc + 4)
  Jnz (R r4') (I y')   <- prog' !? (pc + 5)
  guard (r4' == r4 && y' == -5)

  let r1Val = getR r1 s
      xVal  = case x of
        R r2 -> getR r2 s
        I n  -> n
      r4Val = getR r4 s

  pure $ modifyR 'p' (+6)
       . setR r1 (r1Val + xVal * r4Val)
       . setR r3 0
       . setR r4 0
       $ s


-- | slowStep state
-- run a single instruction of the interpreter
-- returns the new state of the interpreter, and a bool, indicating whether the instruction was an `out` instruction
slowStep :: Interpreter -> (Interpreter, Bool)
slowStep int =
  case (prog int) !? (p int) of
    Just inst -> step' inst int
    Nothing -> error "Program counter out of bounds"

-- | step' inst regs prog pc
-- Run the instruction `inst` at the current state of the registers `regs`, program `prog` and instruction pointer `pc`.
-- Returns the new state of the registers, program and instruction pointer.
step' :: Instruction -> Interpreter -> (Interpreter, Bool)
step' (Cpy (I x) (R y)) i = ( setR y x . stepP $ i , False)
step' (Cpy (R x) (R y)) i = ( setR y (getR x i) . stepP $ i , False)
step' (Inc (R x)) i = ( modifyR x (+1) . stepP $ i , False)
step' (Dec (R x)) i = ( modifyR x (\v -> v-1) . stepP $ i , False)
step' (Jnz (R x) (I w)) i = ( modifyR 'p' (\pc -> if v /= 0 then pc+w else pc+1) i, False) where v = getR x i
step' (Jnz (I v) (I w)) i = ( modifyR 'p' (\pc -> if v /= 0 then pc+w else pc+1) i, False)
step' (Jnz (I v) (R y)) i = ( modifyR 'p' (\pc -> if v /= 0 then pc+w else pc+1) i, False) where w = getR y i
step' (Out (R _)) i = ( stepP i, True)
step' inst _ = error ("Non-implemented instruction: " ++ show inst)


search :: [Int] -> Program -> IO ()
search [] _ = error "Unreachable"
search (i:is) program = do
  let s = Interpreter { p=0, a=i, b=0, c=0, d=0, prog=program }
  exitCode <- evalStateT run (s, [])
  if exitCode == GoodLoop
    then putStrLn $ "Found a good loop with initial value: " ++ show i
    else search is program

main :: IO ()
main = do
  program <- map (toInst .  words) . lines <$> readFile "inputs/day25.txt"
  search [0..] program


  