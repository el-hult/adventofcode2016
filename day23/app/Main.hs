{-
Day 23: Safe Cracking

It is supposed that you have a assembunny interpreter around from day 11.
But I wrote that YEARS ago when coming to this. So I will write a new one!

Just 10 minutes in, I also realize that my old assembunny interpreter would not easily support the new problem anyways...

So I wrote a new one, and it was fast. Then I spent an hour or more debugging.
In the debugging process I manually ran the program on pen and paper, and I saw that the program does some very very inefficient adds in order to compute multiplications.
I wondered if my program was slow and that was the issue.
Do I have to implement some optimization step, so that the multiplications are fast?
Then I re-read the problem poorly, straighened out a misunderstanding in like three seconds, and I got the right answer for part one!

Part B gives a hint about multiplications.
I suppose we must do that optimization now...
My naive implementation completed in ca 530 seconds. So no optimzations were really needed.
But when adding the optimization for multiplications, it took ca 450 milliseconds instead.
I tried some similar optmimizations for repeated 'inc' and 'dec' instructions, but they did not help.
Only the multiplication optimization survived in my code

-}
import Util (replaceAtIndex)
import Data.List ((!?))


-- | Program counter
type PC = Int 

-- | Program is a list of instructions
type Program = [Instruction]

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
  deriving (Show)

toInst :: [String] -> Instruction
toInst ["cpy", x, y] = Cpy (toVal x) (toVal y)
toInst ["inc", x] = Inc (toVal x)
toInst ["dec", x] = Dec (toVal x)
toInst ["jnz", x, y] = Jnz (toVal x) (toVal y)
toInst ["tgl", x] = Tgl (toVal x)
toInst _ = error "unreachable"

-- | toggle i
-- toggle the instruction i
-- For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
-- For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
toggle :: Instruction -> Instruction
-- one argument instructions
toggle (Inc x) = Dec x
toggle (Dec x) = Inc x
toggle (Tgl x) = Inc x
-- two argument instructions
toggle (Cpy x y) = Jnz x y
toggle (Jnz x y) = Cpy x y


-- | The machine has 5 registers. 'a', 'b', 'c', 'd'
-- the 'p' register is used to store the instruction pointer, also known as the program counter.
type Registers = (Int, Int, Int, Int)

-- | get the value of a register
get :: Registers -> Char -> Int
get (a, _, _, _) 'a' = a
get (_, b, _, _) 'b' = b
get (_, _, c, _) 'c' = c
get (_, _, _, d) 'd' = d
get _ _ = error "unreachable"

-- | set regs r 
set ::  Char -> Int -> Registers -> Registers
set 'a' x (_, b, c, d) = (x, b, c, d)
set 'b' x (a, _, c, d) = (a, x, c, d)
set 'c' x (a, b, _, d) = (a, b, x, d)
set 'd' x (a, b, c, _) = (a, b, c, x)
set _ _ _ = error "unreachable"

-- | modify regs r f
modify :: Registers -> Char -> (Int -> Int) -> Registers
modify regs r f = set r (f (get regs r)) regs



-- | runComp (regs, prog, pc)
-- Run the assembunny program `prog` from the state when the instruction pointer is at pc, and registers are `regs`.
-- Returns the final state of the registers when the program halts
runComp :: (Registers, [Instruction], Int) -> Registers
runComp s = 
  let (regs', prog', pc') = quickStep s --  first try optimized steps. they are safe in the sense that if pc is out of bounds, we do nothing
  in case prog' !? pc' of
    Nothing -> regs'
    Just inst' -> --(trace . show $ regs)
      runComp (step inst' regs' prog' pc')

-- | step inst regs prog pc
-- Run the instruction `inst` at the current state of the registers `regs`, program `prog` and instruction pointer `pc`.
-- Returns the new state of the registers, program and instruction pointer.
step :: Instruction -> Registers -> [Instruction] -> Int -> (Registers, [Instruction], Int)
step (Cpy (I x) (R y)) regs prog pc = (set y x regs, prog, pc + 1)
step (Cpy (R x) (R y)) regs prog pc = (set y (get regs x) regs, prog, pc + 1)
step (Inc (R x)) regs prog pc = (modify regs x (+1), prog, pc + 1)
step (Dec (R x)) regs prog pc = (modify regs x (\v -> v-1), prog, pc + 1)
step (Jnz (R x) (I y)) regs prog pc = (regs, prog, pc + if get regs x /= 0 then y else 1)
step (Jnz (I x) (I y)) regs prog pc = (regs, prog, pc + if x /= 0 then y else 1)
step (Jnz (I x) (R y)) regs prog pc = (regs, prog, pc + if x /= 0 then get regs y else 1)
step (Tgl (R x)) regs prog pc = case prog !? (pc + get regs x) of
  Nothing -> (regs, prog, pc + 1) -- if the index is out of bounds, we do nothing
  Just inst -> (regs, replaceAtIndex (pc + get regs x) (toggle inst) prog, pc + 1)
step inst _ _ _ = error ("Non-implemented instruction: " ++ show inst)


-- | quickStep state
-- check if we can do a multiply to speed things up.
-- 
-- cpy r2 r3
-- inc r1
-- dec r3
-- jnz r3 -2
-- dec r4
-- jnz r4 -5
-- ===> r1 = r1 + r2 * r4; r3=0; r4=0; pc+=6
quickStep :: (Registers, Program, PC) -> (Registers, Program, Int)
quickStep s@(regs, prog, pc) = case prog !? pc of
  Just (Cpy (R r2) (R r3)) -> case prog !? (pc + 1) of
    Just (Inc (R r1)) -> case prog !? (pc + 2) of
      Just (Dec (R r3')) | r3' == r3 -> case prog !? (pc + 3) of
        Just (Jnz (R r3'') (I y)) | r3'' == r3 && y == -2 -> case prog !? (pc + 4) of
          Just (Dec (R r4)) -> case prog !? (pc + 5) of
            Just (Jnz (R r4') (I y')) | r4' == r4 && y' == -5 ->
              let r1Val = get regs r1 
                  r2Val = get regs r2
                  r4Val = get regs r4
                  regs' = set r1 (r1Val + r2Val * r4Val) . set r3 0 . set r4 0 $ regs
              in (regs', prog, pc + 6)
            _ -> s
          _ -> s
        _ -> s
      _ -> s
    _ -> s
  _ -> s


main :: IO ()
main = do
  program <- map (toInst .  words) . lines <$> readFile "inputs/day23.txt"
  print $ runComp ((7, 0, 0, 0), program, 0) -- 11739 is correct for part one
  print $ runComp ((12, 0, 0, 0), program, 0) -- 479008299 is correct for part B
