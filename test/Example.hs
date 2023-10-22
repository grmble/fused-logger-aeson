{-# LANGUAGE OverloadedStrings #-}

module Example where

import Control.Algebra (Has)
import Control.Effect.LoggerAeson
import Data.ByteString.Lazy.Char8 qualified as LB8
import GHC.IO (unsafePerformIO)

example :: (Has Logger sig m) => m ()
example = do
  logDebug $ "just a key and a value" :# ["foo" .= ("bar" :: String)]
  withContext ["foo" .= ("666" :: String)] $
    withContext ["foo" .= ("bar" :: String)] $
      logInfo "does it have a context?"

trace :: LB8.ByteString -> LB8.ByteString
trace bs = unsafePerformIO $ do
  LB8.putStrLn bs
  return bs
