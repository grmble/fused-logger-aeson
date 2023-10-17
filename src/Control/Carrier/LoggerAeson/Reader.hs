{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson.Reader where

import Control.Algebra (Algebra (..), type (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.LoggerAeson
  ( LogLevel,
    LogSource,
    Logger (..),
    Message,
    Value,
  )
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (Loc)
import Control.Monad.Logger.Aeson
  ( LogStr,
    LoggingT (..),
    ToLogStr (toLogStr),
    fromLogStr,
  )
import Control.Monad.Logger.Aeson.Internal (KeyMap, LogItem (..), keyMapUnion, locFromCS, logItemEncoding, messageEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 (hPutStrLn)
import Data.Time.Clock (getCurrentTime)
import GHC.IO.Handle (Handle)
import GHC.Stack (CallStack)
import System.IO (BufferMode (..), IOMode (..), hClose, hSetBuffering, openFile, stderr, stdout)
import System.Log.FastLogger (LoggerSet, pushLogStrLn)

-- | Reader Context Logging IO Carrier
--
-- IO Carrier that propages the logging context with a @Reader@.
-- This carries over to other threads which is nice,
-- but you have to thread it through your monad stack
-- which can be a pain.
newtype RCLoggingIOC m a = RCLoggingIOC {runRCLoggingIOC :: LoggingT (ReaderC (KeyMap Value) m) a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Logger :+: sig) (RCLoggingIOC m) where
  alg hdl sig ctx = RCLoggingIOC $ case sig of
    L (LogOther cs ls lvl msg) -> LoggingT $ \logfn -> ReaderC $ \r -> do
      item <- liftIO $ mkLogItem r cs ls lvl msg
      liftIO $ logfn (locFromCS cs) ls lvl (toLogStr $ encodingToLazyByteString $ logItemEncoding item)
      return ctx
    L (WithContext pairs action) -> LoggingT $ \logfn -> ReaderC $ \r ->
      runReader (keyMapUnion r $ KM.fromList pairs) $
        flip runLoggingT logfn $
          runRCLoggingIOC $
            hdl (action <$ ctx)
    R other -> LoggingT $ \logfn -> ReaderC $ \r ->
      alg (runReader r . flip runLoggingT logfn . runRCLoggingIOC . hdl) other ctx

runLogger :: (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> RCLoggingIOC m a -> m a
runLogger f = runReader KM.empty . flip runLoggingT f . runRCLoggingIOC

mkLogItem :: KeyMap Value -> CallStack -> LogSource -> LogLevel -> Message -> IO LogItem
mkLogItem ctx cs ls lvl msg = do
  now <- getCurrentTime
  return
    LogItem
      { logItemTimestamp = now,
        logItemLoc = locFromCS cs,
        logItemLogSource = ls,
        logItemLevel = lvl,
        logItemThreadContext = ctx,
        logItemMessageEncoding = messageEncoding msg
      }

runFileLogger :: (MonadIO m, MonadMask m) => FilePath -> LoggingT m a -> m a
runFileLogger filePath action =
  bracket (liftIO $ openFile filePath AppendMode) (liftIO . hClose) $ \h -> do
    _ <- liftIO $ hSetBuffering h LineBuffering
    runLoggingT action $ handleOutput h

runStderrLogger :: RCLoggingIOC m a -> m a
runStderrLogger = runLogger (handleOutput stderr)

runStdoutLogger :: RCLoggingIOC m a -> m a
runStdoutLogger = runLogger (handleOutput stdout)

-- | Suppress logging output
--
-- This does not actually log anything, but it still needs to run in IO.
-- If there is need for a real pure logger, a pure carrier could be made
suppressLoggerOutput :: RCLoggingIOC m a -> m a
suppressLoggerOutput = runLogger $ \_ _ _ _ -> return ()

runFastLogger :: LoggerSet -> RCLoggingIOC m a -> m a
runFastLogger loggerSet = runLogger (fastLoggerOutput loggerSet)

handleOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
handleOutput h _ _ _ = hPutStrLn h . fromLogStr

fastLoggerOutput :: LoggerSet -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
fastLoggerOutput loggerset _ _ _ = pushLogStrLn loggerset
