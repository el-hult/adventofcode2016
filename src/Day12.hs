module Day12 where
import Text.Parsec
    ( char,
      digit,
      space,
      string,
      choice,
      many1,
      option,
      sepEndBy1,
      parse, getInput
      )
import Text.Parsec.String (Parser)
import Control.Monad.State (State, modify, get, runState, execState)
import qualified Data.Map as M
import Util (untilM_, repeatM)
import System.Environment (getArgs)


data Adress = A | B | C | D deriving (Eq, Show, Ord)
data InstValue = Value Int | Ref Adress deriving (Show, Eq)
data Instruction = Cpy InstValue Adress | Inc Adress | Dec Adress | Jnz InstValue Int
                     deriving (Eq, Show)
type Program = [Instruction]
type Memory = M.Map Adress Int
data Runtime = Runtime {
     mem::Memory,
     prog::Program,
     ptr::Int
} deriving (Eq, Show)
addrParser :: Parser Adress
addrParser = choice [
                char 'a'>>return A,
                char 'b'>>return B,
                char 'c'>>return C,
                char 'd'>>return D
                ]

ivParser :: Parser InstValue
ivParser = choice [
    Ref <$> addrParser,
    Value <$> intParser
    ]

intParser :: Parser Int
intParser = do
    sign <- option id (negate <$ char '-')
    ds <- many1 digit
    return $ sign $ read ds


cpyParser :: Parser Instruction
cpyParser = do
    string "cpy "
    iv <- ivParser
    space
    Cpy iv <$> addrParser
jnzParser :: Parser Instruction
jnzParser = do
    string "jnz "
    iv <- ivParser
    space
    Jnz iv <$> intParser
incParser = do
    string "inc "
    Inc <$> addrParser
decParser = do
    string "dec "
    Dec <$> addrParser

instrParser :: Parser Instruction
instrParser = choice [cpyParser, jnzParser, incParser, decParser]

hasHalted :: State Runtime Bool
hasHalted = get >>= (\rt ->
    pure $ ptr rt >= (length . prog) rt
    )

step' :: Instruction -> Runtime ->  Runtime
step' (Cpy (Value v) addr) s@(Runtime mem  prog ptr) = s{mem=M.insert addr v mem,ptr = ptr+1}
step' (Cpy (Ref a) addr) s@(Runtime mem  prog ptr) = s{mem=M.insert addr v mem,ptr = ptr+1}
    where v = M.findWithDefault 0 a mem
step' (Inc addr) s@(Runtime mem  prog ptr) = s{mem=M.insertWith (+) addr 1 mem,ptr = ptr+1}
step' (Dec addr) s@(Runtime mem  prog ptr) = s{mem=M.insertWith (+) addr (-1) mem,ptr = ptr+1}
step' (Jnz (Value v) i) s@(Runtime mem  prog ptr) = if v /= 0 then s{ptr = ptr+i} else s{ptr=ptr+1}
step' (Jnz (Ref a) i) s@(Runtime mem  prog ptr) = if v /= 0 then s{ptr = ptr+i} else s{ptr=ptr+1}
    where v = M.findWithDefault 0 a mem

step :: State Runtime ()
step = do
    Runtime{mem=mem,ptr=ptr,prog=prog} <- get
    modify (step' (prog !! ptr))

runProg = step `untilM_` hasHalted

parseProg :: String -> Program
parseProg x = case parse (instrParser `sepEndBy1` string "\n") "ProgramParse" x of
    Right prog -> prog
    Left msg -> error (show msg)

main = do
    prog' <- parseProg <$> readFile "inputs/day12.txt"
    let initialStateA = Runtime M.empty prog' 0
        initialStateB = Runtime (M.fromList [(C,1)]) prog' 0
        finalStateA = execState runProg initialStateA
    print finalStateA
    let finalStateB = execState runProg initialStateB
    print finalStateB