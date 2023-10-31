{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Effect.LoggerAesonSpec where

import Control.Effect.LoggerAeson
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.DList qualified as DL
import Data.Text.Arbitrary ()
import GHC.Exts (toList)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "LogLevel" $
    prop "roundtrips to JSON" $ \lvl ->
      (eitherDecode . encode) lvl `shouldBe` (Right lvl :: Either String LogLevel)
  describe "Message" $ do
    it "can be given as string literal" $
      ("look ma! no hands" :: Message) `shouldBe` ("look ma! no hands" :# mempty)
    prop "roundtrips to JSON" $ \msg ->
      (eitherDecode . encode) msg `shouldBe` (Right msg :: Either String Message)
  describe "Meta" $ do
    it "preserves order with monoid construction" $
      toList ("foo" .= True <> "bar" .= False :: Meta) `shouldBe` [("foo", Bool True), ("bar", Bool False)]
    it "preserves order with list construction" $
      toList (["foo" .= True, "bar" .= False] :: Meta) `shouldBe` [("foo", Bool True), ("bar", Bool False)]
    it "last pair wins with list construction" $
      toList (["foo" .= True, "foo" .= False] :: Meta) `shouldBe` [("foo", Bool False)]
    it "last pair wins with monoid construction" $
      toList ("foo" .= True <> "foo" .= False :: Meta) `shouldBe` [("foo", Bool False)]

instance Arbitrary LogLevel where
  arbitrary =
    oneof
      [ pure LevelError,
        pure LevelWarn,
        pure LevelInfo,
        pure LevelDebug,
        pure (LevelOther "trace")
      ]

instance Arbitrary Message where
  arbitrary = do
    text <- arbitrary
    -- FromJSON will read a KeyMap, which will remove duplicates and change the order
    -- so we use KM.toList as well to mirror the change
    meta <- Meta . DL.fromList . KM.toList <$> arbitrary
    pure $ text :# meta

main :: IO ()
main = hspec spec
