{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.LoggerAeson
  ( -- * Synopsis
    -- $synopsis

    -- * Logging functions

    -- ** Pushing a context
    withContext,

    -- ** Implicit call stack, no @LogSource@
    logOther,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- ** Explicit call stack no @LogSource@
    logOtherCS,
    logDebugCS,
    logInfoCS,
    logWarnCS,
    logErrorCS,

    -- ** Implicit callstack, with @LogSource@
    logOtherNS,
    logDebugNS,
    logInfoNS,
    logWarnNS,
    logErrorNS,

    -- * Types
    Logger (..),

    -- * Re-exports from @monad-logger@ or @monad-logger-aeson@
    Message (..),
    LogSource,
    LogLevel (..),
    Pair,
    Value,
    (.=),
  )
where

import Control.Algebra (Has, send)
import Control.Monad.Logger.Aeson (LogLevel (..), LogSource, Message (..))
import Data.Aeson ((.=))
import Data.Aeson.Types (Pair, Value)
import Data.Kind (Type)
import GHC.Stack (CallStack, HasCallStack, callStack)

-- $synopsis
--
-- Structured JSON logging for @fused-effect@ using
-- @monad-logger-aeson@ as underlying implementation.
--
-- @
-- example :: (Has Logger sig m) => m ()
-- example = do
--  logDebug $ "just a key and a value" :# ["xxx" .= ("foo" :: String)]
--  withContext ["ctx" .= ("xxx" :: String)] $ logInfo "does it have a context?"
-- @
--
-- There are 3 families of logging functions:
--
--   * Implicit: 'logDebug', 'logInfo', ... these will print location information from an implicit callstack
--   * Explicit CS: 'logDebugCS', ... these will print location information provided by an explicit callstack argument
--   * No stack: 'logDebugNS', ... these have no location information, but a logsource string

-- | Add context key value pairs during the exection of a code block
withContext :: (Has Logger sig m) => [Pair] -> m a -> m a
withContext pairs action = send (WithContext pairs action)

-- | Debug message with callstack but no log source
logDebug :: (HasCallStack, Has Logger sig m) => Message -> m ()
logDebug = logDebugCS callStack

-- | Info message with callstack but no log source
logInfo :: (HasCallStack, Has Logger sig m) => Message -> m ()
logInfo = logInfoCS callStack

-- | Warn message with callstack but no log source
logWarn :: (HasCallStack, Has Logger sig m) => Message -> m ()
logWarn = logWarnCS callStack

-- | Error message with callstack but no log source
logError :: (HasCallStack, Has Logger sig m) => Message -> m ()
logError = logErrorCS callStack

-- | @LogLevel@ argument, callstack but no log source
logOther :: (HasCallStack, Has Logger sig m) => LogLevel -> Message -> m ()
logOther = logOtherCS callStack

--
-- explicit
--

-- | Debug message with explicit callstack but no log source
logDebugCS :: (Has Logger sig m) => CallStack -> Message -> m ()
logDebugCS cs = logOtherCS cs LevelDebug

-- | Info message with explicit callstack but no log source
logInfoCS :: (Has Logger sig m) => CallStack -> Message -> m ()
logInfoCS cs = logOtherCS cs LevelInfo

-- | Warn message with explicit callstack but no log source
logWarnCS :: (Has Logger sig m) => CallStack -> Message -> m ()
logWarnCS cs = logOtherCS cs LevelWarn

-- | Error message with explicit callstack but no log source
logErrorCS :: (Has Logger sig m) => CallStack -> Message -> m ()
logErrorCS cs = logOtherCS cs LevelError

-- | @LogLevel@ argument, with explicit callstack but no log source
logOtherCS :: (Has Logger sig m) => CallStack -> LogLevel -> Message -> m ()
logOtherCS cs lvl msg = send (LogOther cs "" lvl msg)

--
-- logsource
--

-- | Debug message, no stack but logsource
logDebugNS :: (HasCallStack, Has Logger sig m) => LogSource -> Message -> m ()
logDebugNS ls = logOtherNS ls LevelDebug

-- | Info message, no stack but logsource
logInfoNS :: (HasCallStack, Has Logger sig m) => LogSource -> Message -> m ()
logInfoNS ls = logOtherNS ls LevelInfo

-- | Warn message, no stack but logsource
logWarnNS :: (HasCallStack, Has Logger sig m) => LogSource -> Message -> m ()
logWarnNS ls = logOtherNS ls LevelWarn

-- | Error message, no stack but logsource
logErrorNS :: (HasCallStack, Has Logger sig m) => LogSource -> Message -> m ()
logErrorNS ls = logOtherNS ls LevelError

-- | @LogLevel@ argument, no stack but logsource
logOtherNS :: (HasCallStack, Has Logger sig m) => LogSource -> LogLevel -> Message -> m ()
logOtherNS ls lvl msg = send (LogOther callStack ls lvl msg)

-- | Data type for the effect implementation
data Logger (m :: Type -> Type) k where
  LogOther :: CallStack -> LogSource -> LogLevel -> Message -> Logger m ()
  WithContext :: (Has Logger sig m) => [Pair] -> m a -> Logger m a
