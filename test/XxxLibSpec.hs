module XxxLibSpec where

import Test.Hspec
import Test.QuickCheck
import XxxLib

spec :: Spec
spec = do
  describe "XxxLib" $ do
    it "has a function and can add two natural number" $ do
      add 17 4 `shouldBe` 21
  describe "read" $ do
    it "is inverse to show" $ property $
      \x -> (read . show) x `shouldBe` (x :: Int)

main :: IO ()
main = hspec spec