import Lib
import Data.Either
import Test.Hspec

-- |
unwrapRight :: Show l => Either l r -> r
unwrapRight = either (error . show) id

main :: IO ()
main = hspec $ do
    it "returns the maximum value when given 'max'" $ do
      let result = performAggregation "max" [1, 5, 3, 2, 4]
      unwrapRight result `shouldBe` 5

    it "returns the minimum value when given 'min'" $ do
      let result = performAggregation "min" [1, 5, 3, 2, 4]
      unwrapRight result `shouldBe` 1

    it "returns the sum of values when given 'sum'" $ do
      let result = performAggregation "sum" [1, 5, 3, 2, 4]
      unwrapRight result `shouldBe` 15

    it "returns the count of values when given 'count'" $ do
      let result = performAggregation "count" [1, 5, 3, 2, 4]
      unwrapRight result `shouldBe` 5

    it "returns an error when given an invalid aggregation function" $ do
        let result = performAggregation "invalid" [1, 5, 3, 2, 4]
        isLeft result `shouldBe` True
