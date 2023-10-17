{-# LANGUAGE OverloadedStrings #-}

module Example where

import Control.Algebra (Has)
import Control.Effect.LoggerAeson

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# ["xxx" .= ("foo" :: String)]
  withContext ["ctx" .= ("xxx" :: String)] $ logInfo "does it have a context?"
