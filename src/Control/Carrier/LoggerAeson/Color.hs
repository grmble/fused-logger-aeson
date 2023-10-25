{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.LoggerAeson.Color where

import Control.Carrier.LoggerAeson.Class
import Control.Effect.LoggerAeson (LogLevel (..), Message (..), levelText)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Builder
import Data.Function ((&))
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8Builder)
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
      context,
      level,
      message
    } =
    do
      let text :# meta = message
      timestampBuilder tz time
        <> char7 ' '
        <> sgrBuilder (levelColor level)
        <> char7 '['
        <> coloredLevelText level
        <> char7 ']'
        <> sgrBuilder [Reset]
        <> jsonBuilder context
        <> char7 ' '
        <> byteString (T.encodeUtf8 text)
        <> jsonBuilder meta
        <> char7 '\n'

jsonBuilder :: (ToJSON a) => a -> Builder
jsonBuilder a =
  let enc = encode a
   in if enc == "{}"
        then mempty
        else char7 ' ' <> lazyByteString enc

coloredLevelText :: LogLevel -> Builder
coloredLevelText lvl | lvl >= LevelInfo = encodeUtf8Builder $ levelText lvl
coloredLevelText lvl = encodeUtf8Builder $ T.toUpper $ levelText lvl

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
    & (++ "00000")
    & take 14
    & string7
