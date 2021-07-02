module Lib where
import Text.Read (readMaybe)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Data.Map as M


dayRunners = M.fromList [
    (1,D1.main),
    (2,D2.main),
    (3,D3.main),
    (4,D4.main),
    (5,D5.main),
    (6,D6.main)
    ]

runDay s = case readMaybe s of
    Just n-> case M.lookup n dayRunners of
        Just runner -> runner
        _ -> putStrLn "invalid day"
    _ -> putStrLn "invalid day"