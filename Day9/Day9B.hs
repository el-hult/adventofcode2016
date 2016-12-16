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

processString :: String -> Int
processString s = do
  let (head,inLen,multiple,midAndTail) = parseStep s
  if inLen == -1 then
    length head
  else do
    let (mid,tail) = splitAt inLen midAndTail
    let headVal = length head
    let inVal = processString mid
    let midVal = inVal * multiple -- force evaluation!
    let tailVal = processString tail
    headVal + midVal + tailVal

taskB :: String -> IO ()
taskB indata = do
  let n = processString indata
  putStrLn $ "Task B:" ++ (show n)

main :: IO ()
main = do 
  readFile "input_t.txt" >>= (\file -> taskB file)

-- 10774309174 is too high.

-- program yields correct result on thomas lyckens input