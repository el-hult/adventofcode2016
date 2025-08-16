import Day17

initialState :: State
initialState = makeState2 "qljzarfv"

main :: IO ()
main = do
  print $ solveA initialState -- DRLRDDURDR
  print $ solveB' initialState --500. with debug output!
  print $ solveB initialState --500 as well.
