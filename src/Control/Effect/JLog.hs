{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.JLog where

import Control.Algebra (Has, send)
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Stack (CallStack, HasCallStack, callStack)

data JLog (m :: Type -> Type) k where
  LogItem :: CallStack -> T.Text -> JLog m ()
  WithContext :: (Has JLog sig m) => [(T.Text, T.Text)] -> m a -> JLog m a

logItem :: (HasCallStack, Has JLog sig m) => T.Text -> m ()
logItem msg = send (LogItem callStack msg)

withContext :: (Has JLog sig m) => [(T.Text, T.Text)] -> m a -> m a
withContext pairs action = send (WithContext pairs action)
