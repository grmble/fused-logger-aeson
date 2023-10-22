{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Carrier.LoggerAeson.ClassSpec where

import Control.Carrier.LoggerAeson.Class
import Data.Aeson
import Data.Text.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

spec :: Spec
spec = do
  describe "LogMessage" $
    prop "roundtrips to JSON" $ \m ->
      (eitherDecode . encode) m `shouldBe` (Right m :: Either String LogMessage)

instance Arbitrary LogMessage where
  arbitrary = LogMessage <$> arbitrary <*> arbitrary

main :: IO ()
main = hspec spec
