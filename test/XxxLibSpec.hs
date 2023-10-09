module XxxLibSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "read" $ do
    it "is inverse to show" $
      property $
        \x -> (read . show) x `shouldBe` (x :: Int)

main :: IO ()
main = hspec spec
