module Day12 where

import Control.Monad.State (State, execState, get, modify, gets)
import qualified Data.Map as M
import Text.Parsec
  ( char,
    choice,
    digit,
    many1,
    option,
    parse,
    sepEndBy1,
    space,
    string,
  )
import Text.Parsec.String (Parser)
import Util (untilM_)

data Adress = A | B | C | D deriving (Eq, Show, Ord)

data InstValue = Value Int | Ref Adress deriving (Show, Eq)

data Instruction = Cpy InstValue Adress | Inc Adress | Dec Adress | Jnz InstValue Int
  deriving (Eq, Show)

type Program = [Instruction]

type Memory = M.Map Adress Int

data Runtime = Runtime
  { mem_ :: Memory,
    prog_ :: Program,
    ptr_ :: Int
  }
  deriving (Eq, Show)

addrParser :: Parser Adress
addrParser =
  choice
    [ char 'a' >> return A,
      char 'b' >> return B,
      char 'c' >> return C,
      char 'd' >> return D
    ]

ivParser :: Parser InstValue
ivParser =
  choice
    [ Ref <$> addrParser,
      Value <$> intParser
    ]

intParser :: Parser Int
intParser = do
  sign <- option id (negate <$ char '-')
  ds <- many1 digit
  return $ sign $ read ds

cpyParser :: Parser Instruction
cpyParser = do
  _ <- string "cpy "
  iv <- ivParser
  _ <- space
  Cpy iv <$> addrParser

jnzParser :: Parser Instruction
jnzParser = do
  _ <- string "jnz "
  iv <- ivParser
  _ <- space
  Jnz iv <$> intParser

incParser :: Parser Instruction
incParser = do
  _ <- string "inc "
  Inc <$> addrParser

decParser :: Parser Instruction
decParser = do
  _ <- string "dec "
  Dec <$> addrParser

instrParser :: Parser Instruction
instrParser = choice [cpyParser, jnzParser, incParser, decParser]

hasHalted :: State Runtime Bool
hasHalted = do
  pos <- gets ptr_
  progLen <- gets (length . prog_)
  return (pos >= progLen)

step' :: Instruction -> Runtime -> Runtime
step' (Cpy (Value v) addr) s@(Runtime mem _ ptr) = s {mem_ = M.insert addr v mem, ptr_ = ptr + 1}
step' (Cpy (Ref a) addr) s@(Runtime mem _ ptr) = s {mem_ = M.insert addr v mem, ptr_ = ptr + 1}
  where
    v = M.findWithDefault 0 a mem
step' (Inc addr) s@(Runtime mem _ ptr) = s {mem_ = M.insertWith (+) addr 1 mem, ptr_ = ptr + 1}
step' (Dec addr) s@(Runtime mem _ ptr) = s {mem_ = M.insertWith (+) addr (-1) mem, ptr_ = ptr + 1}
step' (Jnz (Value v) i) s@(Runtime _ _ ptr) = if v /= 0 then s {ptr_ = ptr + i} else s {ptr_ = ptr + 1}
step' (Jnz (Ref a) i) s@(Runtime mem _ ptr) = if v /= 0 then s {ptr_ = ptr + i} else s {ptr_ = ptr + 1}
  where
    v = M.findWithDefault 0 a mem

step :: State Runtime ()
step = do
  Runtime {ptr_ = ptr, prog_ = prog} <- get
  modify (step' (prog !! ptr))

runProg :: State Runtime ()
runProg = step `untilM_` hasHalted

parseProg :: String -> Program
parseProg x = case parse (instrParser `sepEndBy1` string "\n") "ProgramParse" x of
  Right prog -> prog
  Left msg -> error (show msg)

main :: IO ()
main = do
  prog' <- parseProg <$> readFile "inputs/day12.txt"
  let initialStateA = Runtime M.empty prog' 0
      initialStateB = Runtime (M.fromList [(C, 1)]) prog' 0
      finalStateA = execState runProg initialStateA
  print finalStateA
  let finalStateB = execState runProg initialStateB
  print finalStateB
