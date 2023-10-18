module LoggerAesonReaderSpec where

import Control.Carrier.LoggerAeson.Reader
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.IORef (newIORef, readIORef)
import Example qualified as E
import LoggerAesonSpec (builderLogger)
import Test.Hspec

spec :: Spec
spec = do
  describe "LoggerAeson Reader IO Carrier" $
    it "combines contexts in correct order (inner context wins)" $ do
      bref <- newIORef @Builder mempty
      runLogger (builderLogger bref) E.example
      builder <- readIORef bref
      LB8.unpack (toLazyByteString builder) `shouldNotContain` "666"

main :: IO ()
main = hspec spec
