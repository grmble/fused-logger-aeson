{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson
  ( LoggerEnv (..),
    LogFilter (..),
    LogOutput (..),
    LoggerRIOC (..),
    loggerEnv,
    defaultLoggerEnv,
    defaultContext,
    withAsyncHandler,
    jsonItem,
    coloredItem,
  )
where

import Control.Algebra (Algebra (..), type (:+:) (..))
import Control.Carrier.LoggerAeson.Class
import Control.Carrier.LoggerAeson.Color (coloredItem)
import Control.Concurrent
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Effect.LoggerAeson
import Control.Effect.Reader
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder (Builder, char7, lazyByteString, toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LB8
import Data.Time (getCurrentTimeZone)
import Data.Time.Clock (getCurrentTime)
import GHC.Stack (CallStack)
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (Handle, hFlush, stdout)

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
      let LoggerEnv {logFilter} = env
      item <- liftIO $ mkLogItem env lctx cs lvl msg
      when (logFilter lvl (location item)) $
        liftIO $
          itemHandler env (fromItem env item)
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

-- | Log filter settings
data LogFilter
  = -- | quiet - only errors and warnings
    LogQuiet
  | -- | default is INFO
    LogDefault
  | -- | log everything
    LogVerbose
  | -- | custom filter
    LogFilter {logFilter :: LogLevel -> Location -> Bool}

mkLogFilter :: LogFilter -> (LogLevel -> Location -> Bool)
mkLogFilter LogQuiet lvl _ | lvl <= LevelWarn = True
mkLogFilter LogDefault lvl _ | lvl <= LevelInfo = True
mkLogFilter LogVerbose _ _ = True
mkLogFilter (LogFilter x) lvl loc = x lvl loc
mkLogFilter _ _ _ = False

-- | Log output options
data LogOutput = LogJSON | LogColor | LogColorGuess

logInColor :: LogOutput -> Handle -> IO Bool
logInColor LogJSON _ = return False
logInColor LogColor _ = return True
logInColor LogColorGuess handle = hSupportsANSIColor handle

loggerEnv :: LogFilter -> LogOutput -> Handle -> IO (LoggerEnv IO LogItem)
loggerEnv f output handle = do
  tz <- getCurrentTimeZone
  color <- logInColor output handle
  return
    LoggerEnv
      { mkLogItem = defaultMkLogItem,
        fromItem =
          if color
            then coloredItem tz
            else jsonItem,
        itemHandler = outputToHandle handle,
        logFilter = mkLogFilter f
      }

defaultLoggerEnv :: IO (LoggerEnv IO LogItem)
defaultLoggerEnv = loggerEnv LogDefault LogColorGuess stdout

defaultMkLogItem :: KM.KeyMap Value -> CallStack -> LogLevel -> Message -> IO LogItem
defaultMkLogItem ctx cs lvl message = do
  now <- getCurrentTime
  return
    LogItem
      { time = now,
        location = fromCallStack cs,
        level = lvl,
        context = ctx,
        message
      }

jsonItem :: LogItem -> Builder
jsonItem item = lazyByteString (encode item) <> char7 '\n'

outputToHandle :: Handle -> Builder -> IO ()
outputToHandle handle = LB8.hPutStr handle . toLazyByteString

-- | Empty context with correct type
defaultContext :: KM.KeyMap Value
defaultContext = KM.empty

withAsyncHandler :: Handle -> ((Builder -> IO ()) -> IO a) -> IO a
withAsyncHandler handle action = do
  chan <- newTChanIO @Builder
  bracket (forkIO (loop chan)) killThread $ const (action (handler chan))
  where
    handler chan b = atomically (writeTChan chan b)

    loop :: TChan Builder -> IO ()
    loop chan = do
      b <- atomically (readTChan chan) >>= drain chan
      LB8.hPutStr handle (toLazyByteString b)
      hFlush handle
      loop chan

    drain :: TChan Builder -> Builder -> IO Builder
    drain chan acc =
      atomically (tryReadTChan chan) >>= \case
        Just b -> drain chan (acc <> b)
        Nothing -> return acc
