module Main where

import Lib (runDay)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "Select Day in command line"
        else runDay . head $ args
