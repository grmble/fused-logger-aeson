{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson.Class where

import Control.Effect.LoggerAeson
import Data.Aeson (Encoding, FromJSON (parseJSON), Options (..), ToJSON (..), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder (Builder)
import Data.Char (toLower, toUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Exception (CallStack)
import GHC.Generics (Generic)
import GHC.Stack (SrcLoc (..), getCallStack)

-- m is the target monad here: usually IO or Identity
data LoggerEnv m item = LoggerEnv
  { mkLogItem :: KM.KeyMap Value -> CallStack -> LogLevel -> Message -> m item,
    fromItem :: item -> Builder,
    logFilter :: LogLevel -> Location -> Bool,
    itemHandler :: Builder -> m ()
  }

class MonadLoggerContext m where
  askContext :: m (KM.KeyMap Value)

class MonadLoggerCarrier m mx item where
  askLoggerEnv :: m (LoggerEnv mx item)

data LogItem = LogItem
  { time :: UTCTime,
    level :: LogLevel,
    location :: Location,
    context :: KM.KeyMap Value,
    message :: Message
  }
  deriving (Eq, Generic, Show)

instance ToJSON LogItem where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON LogItem where
  parseJSON = genericParseJSON defaultOptions

data Location = Location
  { locPackage :: Text,
    locModule :: Text,
    locLine :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Location where
  toEncoding :: Location -> Encoding
  toEncoding = genericToEncoding locationOptions
  toJSON = genericToJSON locationOptions

instance FromJSON Location where
  parseJSON = genericParseJSON locationOptions

locationOptions :: Options
locationOptions = defaultOptions {fieldLabelModifier = stripPrefix 3}

stripPrefix :: Int -> [Char] -> [Char]
stripPrefix n = lower1 . drop n

lower1 :: [Char] -> [Char]
lower1 (c : cs) = toLower c : cs
lower1 x = x

prefix :: [Char] -> [Char] -> [Char]
prefix pf (c : cs) = pf ++ toUpper c : cs
prefix _ x = x

fromCallStack :: CallStack -> Location
fromCallStack cs = case getCallStack cs of
  ((_, loc) : _) ->
    Location
      { locPackage = T.pack $ srcLocPackage loc,
        locModule = T.pack $ srcLocModule loc,
        locLine = srcLocStartLine loc
      }
  _ -> Location {locPackage = "", locModule = "", locLine = -1}
