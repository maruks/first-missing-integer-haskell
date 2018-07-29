import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck
import Data.List (sort, find)

import Lib (lowestIntegerNotInList)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "lowestIntegerNotInList function" $ do
          it "returns lowest positive integer not in specified list" $ do
            lowestIntegerNotInList [3,4,-1,1] `shouldBe` 2
            lowestIntegerNotInList [1,2,0] `shouldBe` 3
            lowestIntegerNotInList [1,2,3] `shouldBe` 4
            lowestIntegerNotInList [3,2,1] `shouldBe` 4
            lowestIntegerNotInList [1..1000000] `shouldBe` 1000001
            lowestIntegerNotInList (filter (/= 123123) [1..1000000]) `shouldBe` 123123

          it "returns number that is not in list" $
            property $ \xs -> lowestIntegerNotInList xs `notElem` xs

          it "returns positive number" $
            property $ \xs -> lowestIntegerNotInList xs > 0
