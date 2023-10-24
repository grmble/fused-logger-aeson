{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson where

import Control.Algebra (Algebra (..), type (:+:) (..))
import Control.Carrier.LoggerAeson.Class
import Control.Carrier.LoggerAeson.Color (coloredItem)
import Control.Effect.LoggerAeson
  ( LogLevel,
    Logger (..),
    Message ((:#)),
    Meta (..),
    Value,
  )
import Control.Effect.Reader
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.DList qualified as DL
import Data.Time (getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)
import GHC.Stack (CallStack)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (Handle, stdout)

-- | Reader Context Logging IO Carrier
--
-- IO Carrier that propages the logging context with a @Reader@.
-- This carries over to other threads which is nice,
-- but you have to thread it through your monad stack
-- which can be a pain.
newtype LoggerRIOC m a = LoggerRIOC {runLogger :: m a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Has (Reader (KM.KeyMap Value)) sig m, Has (Reader (LoggerEnv IO LogItem)) sig m) =>
  Algebra (Logger :+: sig) (LoggerRIOC m)
  where
  alg hdl sig ctx = case sig of
    L (LogOther cs lvl msg) -> do
      env <- (askLoggerEnv :: LoggerRIOC m (LoggerEnv IO LogItem))
      lctx <- askContext
      item <- liftIO $ mkLogItem env lctx cs lvl msg
      let str = fromItem env item
      liftIO $ handle env str
      LoggerRIOC $ return ctx
    L (WithContext pairs action) ->
      LoggerRIOC $
        local (KM.fromList pairs <>) $
          runLogger (hdl $ action <$ ctx)
    R other -> LoggerRIOC $ alg (runLogger . hdl) other ctx

instance
  (Algebra sig m, Has (Reader (LoggerEnv IO LogItem)) sig m) =>
  MonadLoggerCarrier (LoggerRIOC m) IO LogItem
  where
  askLoggerEnv = LoggerRIOC ask

instance
  (Algebra sig m, Has (Reader (KM.KeyMap Value)) sig m) =>
  MonadLoggerContext (LoggerRIOC m)
  where
  askContext = LoggerRIOC ask

loggerEnv :: Handle -> IO (LoggerEnv IO LogItem)
loggerEnv handle = do
  supportsColor <- hSupportsANSIColor handle
  tz <- getCurrentTimeZone
  return
    LoggerEnv
      { mkLogItem = defaultMkLogItem,
        fromItem =
          if supportsColor
            then coloredItem tz
            else jsonItem,
        handle = outputToHandle handle
      }

defaultLoggerEnv :: IO (LoggerEnv IO LogItem)
defaultLoggerEnv = loggerEnv stdout

defaultMkLogItem :: KM.KeyMap Value -> CallStack -> LogLevel -> Message -> IO LogItem
defaultMkLogItem ctx cs lvl (text :# meta) = do
  now <- getCurrentTime
  return
    LogItem
      { time = now,
        location = fromCallStack cs,
        level = lvl,
        threadContext = ctx,
        message = LogMessage {text, meta = KM.fromList $ DL.toList $ unMeta meta}
      }

jsonItem :: LogItem -> Builder
jsonItem = lazyByteString . encode

outputToHandle :: Handle -> Builder -> IO ()
outputToHandle handle = LB8.hPutStr handle . toLazyByteString

-- | Empty context with correct type
defaultContext :: KM.KeyMap Value
defaultContext = KM.empty
