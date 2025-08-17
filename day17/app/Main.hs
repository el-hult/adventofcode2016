import Day17

initialState :: State
initialState = initializeState "qljzarfv"

main :: IO ()
main = do
  print $ solveA initialState -- DRLRDDURDR
  print $ solveB initialState --500 as well.
