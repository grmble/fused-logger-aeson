module LoggerAesonSpec where

import Control.Carrier.LoggerAeson (TCLoggingIOC (..))
import Control.Monad.Logger (Loc, LogSource, LogStr, LoggingT (runLoggingT), fromLogStr)
import Control.Monad.Logger.Aeson (LogLevel)
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Example qualified as E
import Test.Hspec

spec :: Spec
spec = do
  describe "LoggerAeson ThreadContext IO Carrier" $
    it "combines contexts in correct order (inner context wins)" $ do
      bref <- newIORef @Builder mempty
      runLoggingT (runTCLoggingIOC E.example) (builderLogger bref)
      builder <- readIORef bref
      LB8.putStrLn (toLazyByteString builder)
      LB8.unpack (toLazyByteString builder) `shouldNotContain` "666"

builderLogger :: IORef Builder -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
builderLogger builder _ _ _ msg = modifyIORef' builder (<> byteString (fromLogStr msg))

main :: IO ()
main = hspec spec
