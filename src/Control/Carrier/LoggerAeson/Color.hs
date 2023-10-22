{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.LoggerAeson.Color where

import Control.Carrier.LoggerAeson.Class
import Control.Effect.LoggerAeson (LogLevel (..), Message (..))
import Data.Aeson (Value, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Builder
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (LocalTime (localTimeOfDay), utcToLocalTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (TimeZone)
import System.Console.ANSI.Codes

coloredItem :: TimeZone -> LogItem -> Builder
coloredItem
  tz
  LogItem
    { time,
      threadContext,
      level,
      message
    } =
    do
      let text :# meta = message
      timestampBuilder tz time
        <> char7 ' '
        <> sgrBuilder (levelColor level)
        <> string7 (levelText level)
        <> sgrBuilder [Reset]
        <> keymapBuilder threadContext
        <> char7 ' '
        <> byteString (T.encodeUtf8 text)
        <> keymapBuilder (KM.fromList meta)
        <> char7 '\n'

keymapBuilder :: KM.KeyMap Value -> Builder
keymapBuilder km =
  if KM.null km
    then mempty
    else char7 ' ' <> lazyByteString (encode km)

levelText :: LogLevel -> String
levelText LevelDebug = "[debug]"
levelText LevelInfo = "[info]"
levelText LevelWarn = "[WARN]"
levelText LevelError = "[ERROR]"
levelText (LevelOther x) = T.unpack x

levelColor :: LogLevel -> [SGR]
levelColor LevelDebug = [Reset]
levelColor LevelInfo = [SetColor Foreground Vivid Green]
levelColor LevelWarn = [SetColor Foreground Vivid Yellow]
levelColor LevelError = [SetColor Foreground Vivid Red]
levelColor (LevelOther _) = [Reset]

sgrBuilder :: [SGR] -> Builder
sgrBuilder c = string8 (setSGRCode c)

timestampBuilder :: TimeZone -> UTCTime -> Builder
timestampBuilder tz t =
  utcToLocalTime tz t
    & localTimeOfDay
    & iso8601Show
    & string7
