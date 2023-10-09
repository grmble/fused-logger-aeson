{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Katip
  ( KatipF (..),
    Katip (..),
    KatipContext (..),
    Namespace (..),
    Severity (..),
    LogStr (..),
    logItem,
  )
where

import Control.Algebra (Has, send)
import Data.Kind (Type)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Katip (Katip (..), KatipContext (..), LogStr (..), Namespace (..), Severity (..))
import Katip.Core (getLoc)
import Language.Haskell.TH.Syntax (Loc)

data KatipF (m :: Type -> Type) k where
  LogItem :: Maybe Loc -> Severity -> LogStr -> KatipF m ()

logItem :: (HasCallStack, Has KatipF sig m) => Severity -> LogStr -> m ()
logItem sev str = do
  send (LogItem (withFrozenCallStack getLoc) sev str)
