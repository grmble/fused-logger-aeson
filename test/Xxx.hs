{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Xxx where

import Control.Carrier.LoggerAeson
import Control.Carrier.Reader
import Control.Effect.LoggerAeson
import Data.Function ((&))
import System.IO (stdout)

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# "foo" .= ("bar" :: String) <> "xxx" .= ("666" :: String)
  withContext ["foo" .= ("666" :: String)] $
    withContext ["foo" .= ("bar" :: String), "foo" .= ("xxx" :: String)] $ do
      logInfo "does it have a context?"
      logWarn $ "huh?" :# ["xxx" .= ("foo" :: String)]

main :: IO ()
main = do
  withAsyncHandler stdout $ \itemHandler -> do
    env <- loggerEnv LogVerbose LogJSON stdout
    runLogger example
      & runReader (env {itemHandler})
      & runReader defaultContext
