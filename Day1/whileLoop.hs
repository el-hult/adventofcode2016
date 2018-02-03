{- stack
  script
  --resolver lts-10.3
-}
import Control.Monad.State

type IsZero = Bool

decrementAndCheck :: State (Int, IsZero) ()
decrementAndCheck = do
    (m,_) <- get
    let n = m-1
    put (n, n==0)
    return ()

stateProcessor :: State (Int, IsZero) ()
stateProcessor = do
    let loop True = return ()
        loop False = do
            decrementAndCheck
            (_,isDone) <- get
            loop isDone
    
    loop False

main = print $ runState stateProcessor (12,False)


-- This is a little code snippets for fiddling with state monad.
-- see also https://stackoverflow.com/questions/42213986/example-of-while-loop-in-haskell-using-monads