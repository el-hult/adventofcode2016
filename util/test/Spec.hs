module Main where

import Test.Hspec
import Util

main :: IO ()
main = hspec $ do
  describe "rotR" $ do
    it "simple rotate" $ do
      rotR 2 [1, 2, 3, 4] `shouldBe` [3, 4, 1, 2]
  describe "rotR" $ do
    it "long rotate" $ do
      rotR 5 [1, 2, 3, 4] `shouldBe` [4, 1, 2, 3]
  describe "rotL" $ do
    it "simple rotate" $ do
      rotL 2 [1, 2, 3, 4] `shouldBe` [3, 4, 1, 2]
  describe "rotL" $ do
    it "long rotate" $ do
      rotL 5 [1, 2, 3, 4] `shouldBe` [2, 3, 4, 1]
