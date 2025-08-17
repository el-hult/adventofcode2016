module Day21 where

import Text.Parsec
import Util

data Instruction = SwapPosition Int Int | SwapLetter Char Char | RotateLeft Int | RotateRight Int | RotateByLetter Char | InverseRotateByLetter Char | ReverseBewtween Int Int | MovePosition Int Int deriving (Show, Read)

p1 :: Parsec String () Instruction
p1 = do
  _ <- try $ string "swap position "
  x <- many1 digit
  _ <- string " with position "
  y <- many1 digit
  pure $ SwapPosition (read x) (read y)

p2 :: Parsec String () Instruction
p2 = do
  _ <- try $ string "swap letter "
  x <- letter
  _ <- string " with letter "
  SwapLetter x <$> letter

p3 :: Parsec String () Instruction
p3 = try (string "rotate left ") *> (RotateLeft . read <$> many1 digit) <* string " step" <* optional (char 's')

p4 :: Parsec String () Instruction
p4 = try (string "rotate right ") *> (RotateRight . read <$> many1 digit) <* string " step" <* optional (char 's')

p5 :: Parsec String () Instruction
p5 = try (string "rotate based on position of letter ") *> (RotateByLetter <$> letter)

p6 :: Parsec String () Instruction
p6 = do
  _ <- try $ string "reverse positions "
  x <- many1 digit
  _ <- string " through "
  y <- many1 digit
  pure $ ReverseBewtween (read x) (read y)

p7 :: Parsec String () Instruction
p7 = do
  _ <- try $ string "move position "
  x <- many1 digit
  _ <- string " to position "
  y <- many1 digit
  pure $ MovePosition (read x) (read y)

parseOneInstruction :: Parsec String () Instruction
parseOneInstruction = foldl1 (<|>) [p1, p2, p3, p4, p5, p6, p7]

inputParser :: Parsec String () [Instruction]
inputParser = parseOneInstruction `sepEndBy1` newline <* eof

parseInput :: String -> Either ParseError [Instruction]
parseInput = parse inputParser "input parser"

applyInstruction :: String -> Instruction -> String
applyInstruction s (SwapPosition x y) =
  let a = s !! x
      b = s !! y
      swap i c
        | i == x = b
        | i == y = a
        | otherwise = c
   in zipWith swap [0 ..] s
applyInstruction s (SwapLetter a b) = map swap s
  where
    swap c
      | c == a = b
      | c == b = a
      | otherwise = c
applyInstruction s (RotateLeft x) = rotL x s
applyInstruction s (RotateRight x) = rotR x s
applyInstruction s (RotateByLetter a) =
  let x = unsafeIndexOf a s
      y = if x >= 4 then x + 2 else x + 1
   in rotR y s
applyInstruction s (InverseRotateByLetter a) =
  let x = unsafeIndexOf a s
      y = if odd x then (x + 1) `div` 2 else case x of 2 -> 6; 4 -> 7; 6 -> 0; 0 -> 1; _ -> error "unreachable"
   in rotL y s
applyInstruction s (ReverseBewtween x y) =
  let a = take x s
      b = drop x s
      z = y - x + 1
      c = take z b
      d = drop z b
   in a ++ reverse c ++ d
applyInstruction s (MovePosition x y) =
  let (s', a) = unsafePop x s
   in insertAt y s' a

invertInstruction :: Instruction -> Instruction
invertInstruction (SwapPosition a b) = SwapPosition a b
invertInstruction (SwapLetter a b) = SwapLetter a b
invertInstruction (RotateLeft x) = RotateRight x
invertInstruction (RotateRight x) = RotateLeft x
invertInstruction (RotateByLetter a) = InverseRotateByLetter a
invertInstruction (InverseRotateByLetter _) = undefined
invertInstruction (ReverseBewtween x y) = ReverseBewtween x y
invertInstruction (MovePosition x y) = MovePosition y x
