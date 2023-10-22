{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.LoggerAesonSpec where

import Control.Carrier.LoggerAeson
import Control.Carrier.LoggerAeson.Class (LoggerEnv (..))
import Control.Carrier.Reader
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.Function ((&))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Example qualified as E
import Test.Hspec

spec :: Spec
spec = do
  describe "LoggerAeson Reader IO Carrier" $
    it "combines contexts in correct order (inner context wins)" $ do
      bref <- newIORef @Builder mempty
      env <- defaultLoggerEnv
      runLogger E.example
        & runReader env {handle = builderLogger bref, fromItem = jsonItem}
        & runReader defaultContext
      builder <- readIORef bref
      -- LB8.putStr (toLazyByteString builder)
      LB8.unpack (toLazyByteString builder) `shouldNotContain` "666"

builderLogger :: IORef Builder -> Builder -> IO ()
builderLogger builder msg = modifyIORef' builder (<> msg <> byteString "\n")

main :: IO ()
main = hspec spec
