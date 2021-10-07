import Day18
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "testA" $ (  unlines . map show . take 3 . iterate nextR $ initRowTest1 ) `shouldBe` "..^^.\n.^^^^\n^^..^\n"
    it "testB" $ (unlines . map show . take 10 . iterate nextR $ initRowTest2 ) `shouldBe` ".^^.^.^^^^\n^^^...^..^\n^.^^.^.^^.\n..^^...^^^\n.^^^^.^^.^\n^^..^.^^..\n^^^^..^^^.\n^..^^^^.^^\n.^^^..^.^^\n^^.^^^..^^\n"
    it "testC" $ (solveA 10 initRowTest2 ) `shouldBe` 38