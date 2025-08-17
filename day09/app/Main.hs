import Day9 (taskA, taskB)

main :: IO ()
main = do
  readFile "inputs/day09.txt"
    >>= ( \file ->
            taskA file -- 110346
              >> taskB file -- 10774309173
        )
