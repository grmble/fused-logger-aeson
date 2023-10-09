-- allow functions that can never be called, or rather turn off the check
{-# LANGUAGE AllowAmbiguousTypes #-}
-- finer grained kinds for use with GADTs
{-# LANGUAGE DataKinds #-}
-- allow multiple types with the same field names
{-# LANGUAGE DuplicateRecordFields #-}
-- gadt syntax with existential variables
{-# LANGUAGE GADTs #-}
-- string literals use IsString.fromString
{-# LANGUAGE OverloadedStrings #-}
-- type level programming - type level functions
{-# LANGUAGE TypeFamilies #-}
-- permit instance definitions that could bottom the type checker
{-# LANGUAGE UndecidableInstances #-}
-- no common subexpression elemination - may be needed with unsafePerformIO/cmdargs
{-# OPTIONS_GHC -fno-cse #-}

module XxxLib (someFunc, add) where

import Control.Algebra (Has)
import Control.Carrier.Lift (runM)
import Control.Carrier.Reader (runReader)
import Control.Exception (bracket)
import Data.Function ((&))
import GHC.Natural (Natural)
import Katip
import Katip.Fused
import System.IO (stdout)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Add two natural numbers.
--
-- >>> add 17 4
-- 21
add :: Natural -> Natural -> Natural
add a b = a + b

main :: IO ()
main = do
  scribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" scribe defaultScribeSettings =<< initLogEnv "MyApp" "prod"
  bracket mkLogEnv closeScribes runApp

runApp :: LogEnv -> IO ()
runApp le =
  runKatipIO app
    & runReader @LogContexts mempty
    & runReader le
    & runReader @Namespace "main"
    & runM

app :: (Has KatipF sig m) => m ()
app = Katip.Fused.logItem InfoS "asdf"
