{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Effect.LoggerAesonSpec where

import Control.Effect.LoggerAeson
import Data.Aeson
import Data.Text.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.DList qualified as DL
import Data.Aeson.KeyMap qualified as KM

spec :: Spec
spec = do
  describe "LogLevel" $
    prop "roundtrips to JSON" $ \lvl ->
      (eitherDecode . encode) lvl `shouldBe` (Right lvl :: Either String LogLevel)
  describe "Message" $
    prop "roundtrips to JSON" $ \msg ->
      (eitherDecode . encode) msg `shouldBe` (Right msg :: Either String Message)

instance Arbitrary LogLevel where
  arbitrary =
    oneof
      [ return LevelError,
        return LevelWarn,
        return LevelInfo,
        return LevelDebug,
        return (LevelOther "trace")
      ]

instance Arbitrary Message where
  arbitrary = do
    text <- arbitrary
    -- FromJSON will read a KeyMap, which will remove duplicates and change the order
    -- so we use KM.toList as well to mirror the change
    meta <- Meta . DL.fromList . KM.toList <$> arbitrary
    return $ text :# meta

main :: IO ()
main = hspec spec
