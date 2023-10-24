{-# LANGUAGE OverloadedStrings #-}

module Xxx where

import Control.Carrier.LoggerAeson
import Control.Carrier.Reader
import Control.Effect.LoggerAeson
  ( Logger,
    Message ((:#)),
    logDebug,
    logError,
    logInfo,
    logWarn,
    withContext,
    (.=),
  )
import Data.Function ((&))

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# "foo" .= ("bar" :: String)
  withContext ["foo" .= ("666" :: String)] $
    withContext ["foo" .= ("bar" :: String), "foo" .= ("xxx" :: String)] $ do
      logInfo "does it have a context?"
      logWarn "oopsie"
      logError "ouchie"

main :: IO ()
main = do
  env <- defaultLoggerEnv
  runLogger example
    & runReader env
    & runReader defaultContext
