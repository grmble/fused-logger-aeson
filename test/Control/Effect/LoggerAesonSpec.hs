{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Effect.LoggerAesonSpec where

import Control.Effect.LoggerAeson
import Data.Aeson
import Data.Text.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "LogLevel" $
    prop "roundtrips to JSON" $ \lvl ->
      (eitherDecode . encode) lvl `shouldBe` (Right lvl :: Either String LogLevel)

instance Arbitrary LogLevel where
  arbitrary =
    oneof
      [ return LevelError,
        return LevelWarn,
        return LevelInfo,
        return LevelDebug,
        return (LevelOther "trace")
      ]

main :: IO ()
main = hspec spec
