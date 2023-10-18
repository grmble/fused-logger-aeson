{-# LANGUAGE OverloadedStrings #-}

module Example where

import Control.Algebra (Has)
import Control.Effect.LoggerAeson

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# ["foo" .= ("bar" :: String)]
  withContext ["foo" .= ("666" :: String)] $
    withContext ["foo" .= ("bar" :: String)] $
      logInfo "does it have a context?"
