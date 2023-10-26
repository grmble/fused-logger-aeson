{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Carrier.LoggerAeson.ClassSpec where

import Control.Carrier.LoggerAeson.Class
import Control.Effect.LoggerAesonSpec ()
import Data.Aeson
import Data.Text.Arbitrary ()
import Data.Time (UTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (arbitrary))

spec :: Spec
spec = do
  describe "LogItem" $ do
    prop "roundtrips to JSON" $ \msg ->
      (eitherDecode . encode) msg `shouldBe` (Right msg :: Either String LogItem)

instance Arbitrary LogItem where
  arbitrary = LogItem <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . secondsToNominalDiffTime <$> arbitrary

main :: IO ()
main = hspec spec
