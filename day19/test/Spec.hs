import           Data.List     (unfoldr)
import qualified Data.Sequence as SS
import           Day19
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "foo" $ do
    it "testA" $ solveB 2 `shouldBe` 1
    it "testB" $ solveB 3 `shouldBe` 3
    it "testC" $ solveB 4 `shouldBe` 1
    it "testD" $ solveB 5 `shouldBe` 2
    it "testE" $ solveB 6 `shouldBe` 3
    it "testF" $ [(SS.fromList [1, 2, 3, 5, 6], 1),
                  (SS.fromList [1, 2, 3, 6], 2),
                  (SS.fromList [2, 3, 6], 2),
                  (SS.fromList [3, 6], 0),
                  (SS.fromList [3], 1)]
                    `shouldBe`
                      unfoldr stealOne (SS.fromList [1 .. 6], 0)
