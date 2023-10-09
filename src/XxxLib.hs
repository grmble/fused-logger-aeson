{-# LANGUAGE OverloadedStrings #-}

module XxxLib (app) where

import Control.Algebra (Has)
import Control.Effect.Katip
  ( KatipF,
    Severity (..),
    logItem,
  )

app :: (Has KatipF sig m) => m ()
app = do
  logItem DebugS "debug output"
  logItem InfoS "some information"
  logItem WarningS "a warning"
  logItem ErrorS "an error"
