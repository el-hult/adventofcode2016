module Main where

import Lib (day1)
import System.Environment (getArgs)
import Text.Read (readMaybe)

runDay s = case readMaybe s of
    Just 1->day1
    _ -> print "invalid day"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "Select Day in command line"
        else runDay . head $ args
