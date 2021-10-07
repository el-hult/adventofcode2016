module Main where

import Day21
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Instructions" $ do
    it "Swaps 1" $ do
      applyInstruction "abcde" (SwapPosition 0 4) `shouldBe` "ebcda"
    it "Swaps 2" $ do
      applyInstruction "ebcda" (SwapLetter 'd' 'b') `shouldBe` "edcba"
    it "Reverses all" $ do
      applyInstruction "edcba" (ReverseBewtween 0 4) `shouldBe` "abcde"
    it "Reverses mid" $ do
      applyInstruction "edcba" (ReverseBewtween 1 3) `shouldBe` "ebcda"
    it "move position 1 to position 4" $ do
      applyInstruction "bcdea" (MovePosition 1 4) `shouldBe` "bdeac"
    it "move position 3 to position 0" $ do
      applyInstruction "bdeac" (MovePosition 3 0) `shouldBe` "abdec"
    it "Rotates1left" $ do
      applyInstruction "abcde" (RotateLeft 1) `shouldBe` "bcdea"
    it "Rotates1" $ do
      applyInstruction "abdec" (RotateByLetter 'b') `shouldBe` "ecabd"
    it "Rotates2" $ do
      applyInstruction "ecabd" (RotateByLetter 'd') `shouldBe` "decab"
