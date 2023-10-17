module LoggerAesonSpec where

import Control.Carrier.LoggerAeson (TCLoggingIOC (..))
import Control.Monad.Logger.Aeson (runStderrLoggingT)
import Example qualified as E
import Test.Hspec (Spec, describe, hspec, it, shouldReturn)

spec :: Spec
spec = do
  describe "LoggerAeson IO Carrier" $
    it "runs the logging example" $ do
      runStderrLoggingT (runTCLoggingIOC E.example) `shouldReturn` ()

main :: IO ()
main = hspec spec
