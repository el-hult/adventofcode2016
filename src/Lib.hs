module Lib where
import Text.Read (readMaybe)
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
-- import qualified Day9 as D9
-- import qualified Day10 as D10
-- import qualified Day11 as D11
-- import qualified Day12 as D12
-- import qualified Day13 as D13
-- import qualified Day14 as D14
-- import qualified Day15 as D15
-- import qualified Day16 as D16
-- import qualified Day17 as D17
-- import qualified Day18 as D18
-- import qualified Day19 as D19
-- import qualified Day20 as D20
-- import qualified Day21 as D21
-- import qualified Day22 as D22
-- import qualified Day23 as D23
-- import qualified Day24 as D24
import qualified Data.Map as M


dayRunners = M.fromList [
    (1,D1.main)
    ,(2,D2.main)
    ,(3,D3.main)
    ,(4,D4.main)
    ,(5,D5.main)
    ,(6,D6.main)
    ,(7,D7.main)
    ,(8,D8.main)
    -- ,(9,D9.main)
    -- ,(10,D10.main)
    -- ,(11,D11.main)
    -- ,(12,D12.main)
    -- ,(13,D13.main)
    -- ,(14,D14.main)
    -- ,(15,D15.main)
    -- ,(16,D16.main)
    -- ,(17,D17.main)
    -- ,(18,D18.main)
    -- ,(19,D19.main)
    -- ,(20,D20.main)
    -- ,(21,D21.main)
    -- ,(22,D22.main)
    -- ,(23,D23.main)
    -- ,(24,D24.main)
    ]

runDay s = case readMaybe s of
    Just n-> case M.lookup n dayRunners of
        Just runner -> runner
        _ -> putStrLn "invalid day"
    _ -> putStrLn "invalid day"