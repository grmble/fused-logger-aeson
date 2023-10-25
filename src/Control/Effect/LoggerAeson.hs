{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.LoggerAeson
  ( -- * Synopsis
    -- $synopsis

    -- * Logging functions

    -- ** Pushing a context
    withContext,

    -- ** Implicit call stack
    logOther,
    logDebug,
    logInfo,
    logWarn,
    logError,

    -- ** Explicit call stack
    logOtherCS,
    logDebugCS,
    logInfoCS,
    logWarnCS,
    logErrorCS,

    -- * Types and utilites
    Logger (..),
    Message (..),
    Meta (..),
    Context (..),
    LogLevel (..),
    levelText,

    -- * Re-exports
    Value,
    Pair,
    (.=),
  )
where

import Control.Algebra (Has, send)
import Control.Monad (forM_, unless)
import Control.Monad.ST
import Data.Aeson
import Data.Aeson.Encoding qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Aeson.Types qualified as A
import Data.DList qualified as DL
import Data.Foldable qualified as F
import Data.HashSet qualified as HS
import Data.Kind (Type)
import Data.STRef
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, callStack)

-- $synopsis
--
-- Structured JSON logging for @fused-effect@.
-- 'monad-logger-aeson' and 'monad-loger' are not actually
-- used anymore because of the big dependency footprint,
-- although it would be possible to define a carrier type for
-- those systems.
--
-- The client code should require few changes though:
-- just use @Control.Effect.LoggerAeson@ instead of
-- 'Control.Monad.LoggerAeson'
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
withContext :: (Has Logger sig m) => Context -> m a -> m a
withContext (Context context) action = send (WithContext (DL.toList context) action)

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
logOtherCS cs lvl msg = send (LogOther cs lvl msg)

-- | Data type for the effect implementation
data Logger (m :: Type -> Type) k where
  LogOther :: CallStack -> LogLevel -> Message -> Logger m ()
  WithContext :: (Has Logger sig m) => [Pair] -> m a -> Logger m a

-- | Log levels 'monad-logger' style
data LogLevel
  = LevelError
  | LevelWarn
  | LevelInfo
  | LevelDebug
  | LevelOther Text
  deriving (Show, Eq, Generic, Ord)

instance ToJSON LogLevel where
  toEncoding = AE.text . levelText

instance FromJSON LogLevel where
  parseJSON (A.String txt) = return (levelFromText txt)
  parseJSON invalid = A.typeMismatch "LogLevel" invalid

levelText :: LogLevel -> Text
levelText LevelDebug = "debug"
levelText LevelInfo = "info"
levelText LevelWarn = "warn"
levelText LevelError = "error"
levelText (LevelOther x) = x

levelFromText :: Text -> LogLevel
levelFromText "error" = LevelError
levelFromText "warn" = LevelWarn
levelFromText "info" = LevelInfo
levelFromText "debug" = LevelDebug
levelFromText x = LevelOther x

data Message = Text :# Meta
  deriving (Show, Eq, Generic)

infixr 5 :#

instance IsString Message where
  fromString s = T.pack s :# mempty

newtype Meta = Meta {unMeta :: DL.DList (Key, Value)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance IsList Meta where
  type Item Meta = (Key, Value)
  fromList = Meta . DL.fromList
  toList = uniqueMeta . DL.toList . unMeta

instance KeyValue Value Meta where
  (.=) = explicitToField toJSON
  explicitToField f k v = Meta $ DL.singleton (k, f v)

instance ToJSON Message where
  toEncoding (text :# meta) =
    let textEncoding = "text" .= text
        encoding =
          if null $ unMeta meta
            then textEncoding
            else textEncoding <> "meta" .= meta
     in pairs encoding

instance FromJSON Message where
  parseJSON (Object o) = mkMessage <$> o .: "text" <*> o .:? "meta"
    where
      mkMessage txt (Just meta) = txt :# meta
      mkMessage txt Nothing = txt :# mempty
  parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Meta where
  toEncoding = pairs . foldMap (uncurry (.=)) . toList

instance FromJSON Meta where
  parseJSON (Object o) = return $ Meta $ DL.fromList $ KM.toList o
  parseJSON invalid = typeMismatch "Object" invalid

uniqueMeta :: (Foldable f) => f (Key, Value) -> [(Key, Value)]
uniqueMeta meta = runST $ do
  seenRef <- newSTRef HS.empty
  resultRef <- newSTRef []
  forM_ (reverse $ F.toList meta) $ \(k, v) -> do
    done <- HS.member k <$> readSTRef seenRef
    unless done $ do
      modifySTRef seenRef (HS.insert k)
      modifySTRef resultRef ((k, v) :)
  readSTRef resultRef

newtype Context = Context {unContext :: DL.DList (Key, Value)}
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance KeyValue Value Context where
  (.=) = explicitToField toJSON
  explicitToField f k v = Context $ DL.singleton (k, f v)

instance GHC.Exts.IsList Context where
  type Item Context = (Key, Value)
  fromList = Context . DL.fromList
  toList = DL.toList . unContext
