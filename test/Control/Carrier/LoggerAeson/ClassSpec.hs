{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Carrier.LoggerAeson.ClassSpec where

import Data.Text.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "LogMessage" $ do
    prop "roundtrips to JSON" $ \x ->
      (read . show) x `shouldBe` (x :: Int)

main :: IO ()
main = hspec spec
