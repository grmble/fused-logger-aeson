module LoggerAesonReaderSpec where

import Control.Carrier.LoggerAeson.Reader (runStderrLogger)
import Example qualified as E
import Test.Hspec (Spec, describe, hspec, it, shouldReturn)

spec :: Spec
spec = do
  describe "LoggerAeson IO Carrier" $
    it "runs the logging example" $ do
      runStderrLogger E.example `shouldReturn` ()

main :: IO ()
main = hspec spec
