module Main where

import Data.Either (isRight)
import Data.Map (fromList)
import Day22
import Test.Hspec

-- | A helper that reads the whole input file, but from the test directory.
-- This is not the same file on disk as the "true" input file, but I guess that makes sense, since I have
-- this copy on hand only for testing purposes.
withInputFile :: IO String
withInputFile = do
  readFile "test/day22.txt"

main :: IO ()
main = hspec $ do
  describe "Instructions" $ do
    it "Pases line with 2 digit data" $ do
      lineToNode "/dev/grid/node-x0-y0     89T   67T    22T   75%" `shouldBe` Right DataNode {x = 0, y = 0, size = 89, used = 67}
    it "Pases line with 3 digit data" $ do
      lineToNode "/dev/grid/node-x24-y9   501T  499T     2T   99%" `shouldBe` Right DataNode {x = 24, y = 9, size = 501, used = 499}
    describe "Can handle the state" $ do
      it "Move a simple one" $ do
        doMove (0, 0) D (S {_ptr = (0, 0), _board = fromList [((0, 0), (3, 3)), ((0, 1), (6, 3))]}) `shouldBe` (S {_ptr = (0, 1), _board = fromList [((0, 0), (3, 0)), ((0, 1), (6, 6))]})
    before withInputFile $
      context "with the input file" $
        it "Can parse the whole input file with no fails" (\content -> all (isRight . lineToNode) (drop 2 . lines $ content))