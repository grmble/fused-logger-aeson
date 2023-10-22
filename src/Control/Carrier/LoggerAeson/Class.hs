{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.LoggerAeson.Class where

import Control.Effect.LoggerAeson (LogLevel, Message)
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), Options (..), ToJSON (..), Value (Object), defaultOptions, genericParseJSON, genericToEncoding, genericToJSON, object, (.:), (.:?))
import Data.Aeson.Encoding qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (typeMismatch)
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
    handle :: Builder -> m ()
  }

class MonadLoggerContext m where
  askContext :: m (KM.KeyMap Value)

class MonadLoggerCarrier m mx item where
  askLoggerEnv :: m (LoggerEnv mx item)

data LogItem = LogItem
  { time :: UTCTime,
    level :: LogLevel,
    location :: Location,
    threadContext :: KM.KeyMap Value,
    message :: LogMessage
  }
  deriving (Eq, Generic, Ord, Show)

instance ToJSON LogItem where
  toEncoding = genericToEncoding defaultOptions

data Location = Location
  { locPackage :: Text,
    locModule :: Text,
    locLine :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Location where
  toEncoding = genericToEncoding defaultOptions {fieldLabelModifier = stripPrefix 3}
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = stripPrefix 3}

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = prefix "loc"}

-- | Log Message for serialization
--
-- Message has the nicer interface, but LogMessage is actually stored
data LogMessage = LogMessage
  { text :: Text,
    meta :: KM.KeyMap Value
  }
  deriving (Show, Eq, Ord)

instance ToJSON LogMessage where
  toEncoding LogMessage {text, meta} =
    AE.pairs $
      if KM.null meta
        then "text" .= text
        else "text" .= text <> "meta" .= meta
  toJSON LogMessage {text, meta} =
    object $
      if KM.null meta
        then ["text" .= text]
        else ["text" .= text, "meta" .= meta]

instance FromJSON LogMessage where
  parseJSON (Object o) = mkMessage <$> o .: "text" <*> o .:? "meta"
    where
      mkMessage :: Text -> Maybe (KM.KeyMap Value) -> LogMessage
      mkMessage text (Just meta) = LogMessage {text, meta}
      mkMessage text Nothing = LogMessage {text, meta = KM.empty}
  parseJSON invalid = typeMismatch "Object" invalid

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
