module Main where

import Day20
import Test.Hspec

testStr = "5-8\n0-2\n4-7"

spec :: Spec
spec = do
  it "parses the list" $ makeBlacklist testStr `shouldBe` Blacklist [(0, 2), (4, 8)]
  it "counts the blocked ones correctly" $ totalBlocked (makeBlacklist testStr) `shouldBe` 8

main = hspec $ spec