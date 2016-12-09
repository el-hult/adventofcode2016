import System.IO
import Data.List (transpose)
import Data.List.Split (splitOn)

data Screen = Screen [[Bool]]
instance Show Screen where
 show (Screen b) = foldr (++) "" $ map (\y -> y ++ "\n") $ map (foldr (++) "") $ (map (map (\x -> if x then "#" else ".")) b)

countPixels :: Screen -> Int
countPixels (Screen b) = sum ( map countPixelRow b)

countPixelRow :: [Bool] -> Int
countPixelRow l = foldr (\ b z -> z + (fromEnum b)) 0 l

instruct :: Screen -> String-> Screen
instruct sc instruction = case (words instruction) of
 ("rect":argList) -> rect argList sc
 ("rotate":"row":argList) -> rotateR argList sc
 ("rotate":"column":argList) -> rotateC argList sc

rect :: [String] -> Screen -> Screen
rect args (Screen b) = 
 let (rows,cols) = parseRectArgs args in
  Screen $ map (\ (i, bs ) -> if i < rows then drawRow cols bs else bs ) $ zip [0..] b

drawRow :: Int -> [Bool] -> [Bool]
drawRow i b = (replicate i True) ++ (drop i b)

rotateR :: [String] -> Screen -> Screen
rotateR args (Screen b) = 
 let (rowNo,n) = parseRotateArgs args in
  Screen $ map (\ (i, bs ) -> if i == rowNo then rotate n bs else bs ) $ zip [0..] b

rotateC :: [String] -> Screen -> Screen
rotateC args sc = transScreen $ rotateR args $transScreen sc

transScreen :: Screen -> Screen
transScreen (Screen b) = Screen (transpose b)

parseRotateArgs :: [String] -> (Int,Int)
parseRotateArgs w = do
 let n = read (tail $ tail (w !! 0)) :: Int
 let m = read (w !! 2) :: Int
 (n,m)

parseRectArgs :: [String] -> (Int,Int)
parseRectArgs w = let z = splitOn "x" ( w !! 0) in (read (z!!1) :: Int, read (z!!0 ):: Int)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- rotates to the right in a normal list. 
rotate :: Int -> [a] -> [a]
rotate i l = let (a,b) = splitAt (length l - i) l in b ++ a

taskA  :: [Char] -> String
taskA s = do
 let blank = Screen $ replicate 6 $ replicate 50 False
 let done = foldl instruct blank $ lines s
 (show  done) ++ (show $ countPixels done) ++ "pixels are on"

main =
 readFile "input.txt" >>= \inString ->
  putStrLn "Answer to A:" >>
  putStrLn (taskA inString) >> -- 27 is too low. 42 is too low. 91 is too low.
  putStrLn "Answer to B: is shown above! :)" >>