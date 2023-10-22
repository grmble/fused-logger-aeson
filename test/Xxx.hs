{-# LANGUAGE OverloadedStrings #-}

module Xxx where

import Control.Carrier.LoggerAeson.RIO
import Control.Carrier.Reader
import Control.Effect.LoggerAeson
import Data.Function ((&))

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# ["foo" .= ("bar" :: String)]
  withContext ["foo" .= ("666" :: String)] $
    withContext ["foo" .= ("bar" :: String)] $
      logInfo "does it have a context?"

main :: IO ()
main = do
  env <- defaultLoggerEnv
  runLogger example
    & runReader env
    & runReader defaultContext
