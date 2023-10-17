{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson where

import Control.Algebra (Algebra (..), type (:+:) (..))
import Control.Effect.LoggerAeson (Logger (..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.Aeson
  ( LoggingT (..),
    MonadLogger (monadLoggerLog),
    ToLogStr (toLogStr),
    withThreadContext,
  )
import Control.Monad.Logger.Aeson.Internal (locFromCS)

-- | Thread Context Logging IO Carrier
--
-- Simple carrier using @MonadLoggerAeson@ style contexts.
-- The contexts are in a thread local variable.
-- This makes it dead simple to set in some monad stack
-- (e.g. just use @MonadLoggerAeson@'s 'withThreadContext' from
-- some WAI middleware, this carrier will pick it up)
newtype TCLoggingIOC m a = TCLoggingIOC {runTCLoggingIOC :: (LoggingT m) a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance (MonadIO m, MonadMask m, Algebra sig m) => Algebra (Logger :+: sig) (TCLoggingIOC m) where
  alg hdl sig ctx = TCLoggingIOC $ case sig of
    L (LogOther cs ls lvl msg) ->
      ctx <$ monadLoggerLog (locFromCS cs) ls lvl (toLogStr msg)
    L (WithContext pairs action) -> LoggingT $ \logfn ->
      withThreadContext pairs $
        flip runLoggingT logfn $
          runTCLoggingIOC $
            hdl (action <$ ctx)
    R other -> LoggingT $ \logfn ->
      alg (flip runLoggingT logfn . runTCLoggingIOC . hdl) other ctx
