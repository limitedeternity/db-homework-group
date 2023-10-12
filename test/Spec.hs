import Lib
import Control.Exception

import Conduit
import Test.Hspec
import qualified Data.Conduit.Combinators as CC
import qualified Data.Text as T
import qualified Data.Map as Map


-- |
anyPatternMatchFail :: Selector PatternMatchFail
anyPatternMatchFail = const True

main :: IO ()
main = hspec $ do
    it "returns maximum values when given 'max'" $ do
        result <- runConduit $
            yieldMany
             [  (,) (T.pack "Key1") (1 :: Double)
              , (,) (T.pack "Key2") (5 :: Double)
              , (,) (T.pack "Key1") (3 :: Double)
              , (,) (T.pack "Key2") (2 :: Double)
              , (,) (T.pack "Key1") (4 :: Double)
             ]
            .| CC.foldl (getFoldAggregate "max") Map.empty

        Map.lookup (T.pack "Key1") result `shouldBe` Just 4
        Map.lookup (T.pack "Key2") result `shouldBe` Just 5

    it "returns minimum values when given 'min'" $ do
        result <- runConduit $
            yieldMany
             [  (,) (T.pack "Key1") (1 :: Double)
              , (,) (T.pack "Key2") (5 :: Double)
              , (,) (T.pack "Key1") (3 :: Double)
              , (,) (T.pack "Key2") (2 :: Double)
              , (,) (T.pack "Key1") (4 :: Double)
             ]
            .| CC.foldl (getFoldAggregate "min") Map.empty

        Map.lookup (T.pack "Key1") result `shouldBe` Just 1
        Map.lookup (T.pack "Key2") result `shouldBe` Just 2

    it "returns sums of values when given 'sum'" $ do
        result <- runConduit $
            yieldMany
             [  (,) (T.pack "Key1") (1 :: Double)
              , (,) (T.pack "Key2") (5 :: Double)
              , (,) (T.pack "Key1") (3 :: Double)
              , (,) (T.pack "Key2") (2 :: Double)
              , (,) (T.pack "Key1") (4 :: Double)
             ]
            .| CC.foldl (getFoldAggregate "sum") Map.empty

        Map.lookup (T.pack "Key1") result `shouldBe` Just 8
        Map.lookup (T.pack "Key2") result `shouldBe` Just 7

    it "returns amounts of values when given 'count'" $ do
        result <- runConduit $
            yieldMany
             [  (,) (T.pack "Key1") (1 :: Double)
              , (,) (T.pack "Key2") (5 :: Double)
              , (,) (T.pack "Key1") (3 :: Double)
              , (,) (T.pack "Key2") (2 :: Double)
              , (,) (T.pack "Key1") (4 :: Double)
             ]
            .| CC.foldl (getFoldAggregate "count") Map.empty

        Map.lookup (T.pack "Key1") result `shouldBe` Just 3
        Map.lookup (T.pack "Key2") result `shouldBe` Just 2

    it "throws an error when given an invalid aggregation function" $
        let action = evaluate $ runConduitPure $ yieldMany [
                (,) (T.pack "Key1") (1 :: Double)
              , (,) (T.pack "Key2") (5 :: Double)
              , (,) (T.pack "Key1") (3 :: Double)
              , (,) (T.pack "Key2") (2 :: Double)
              , (,) (T.pack "Key1") (4 :: Double)
             ]
             .| CC.foldl (getFoldAggregate "invalid") Map.empty
        in
        action `shouldThrow` anyPatternMatchFail
