import System.IO (readFile,putStrLn)
import Text.Regex.Posix

pattern :: String
pattern = "\\(([0-9]+)x([0-9]+)\\)"

parseStep :: String -> (String, Int, Int, String)
parseStep x = do
  let (l1,l2) = x =~ pattern :: (Int,Int)
  if l1 == -1
    then
      (x,l1,l2,"")
    else do
      let y = x =~ pattern :: [[String]]
      let intOne = read $ y !! 0 !! 1 :: Int
      let intTwo = read $ y !! 0 !! 2 :: Int
      let head = take l1 x
      let tail = drop (l1+l2) x
      (head,intOne,intTwo,tail)
 
applyStep :: (String, Int, Int, String) -> ([Char],[Char])
applyStep (head,i1,i2,tail) = (head ++ concat((replicate i2 (take i1 tail))), drop i1 tail)

goTakeStep :: String -> ([Char],[Char])
goTakeStep = applyStep . parseStep

takeManySteps :: ([Char],[Char]) -> ([Char],[Char])
takeManySteps (s1,"") = (s1,"")
takeManySteps (s1,s2) = takeManySteps (s1 ++ fst (goTakeStep s2), snd ( goTakeStep s2))

processString :: String -> String
processString s = fst ( takeManySteps ("",s))

taskA :: String -> IO ()
taskA indata = do
  let l = (lines indata) !! 0 -- only parse the first line
  let pl = processString l
  let n = length pl
  putStrLn $ "Task A:" ++ (show n)

main :: IO ()
main = do 
  readFile "input.txt" >>= (\file -> taskA file )