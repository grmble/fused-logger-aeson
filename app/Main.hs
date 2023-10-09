{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Carrier.Katip (KatipIOC (..))
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import Control.Effect.Katip (Namespace)
import Control.Exception (bracket)
import Data.Function ((&))
import Katip qualified as K
import System.IO (stdout)
import XxxLib (app)

main :: IO ()
main = do
  scribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" scribe K.defaultScribeSettings =<< K.initLogEnv "MyApp" "prod"
  bracket mkLogEnv K.closeScribes runApp

runApp :: K.LogEnv -> IO ()
runApp le =
  runKatipIO app
    & runReader @K.LogContexts mempty
    & runReader le
    & runReader @Namespace "main"
    & runM
