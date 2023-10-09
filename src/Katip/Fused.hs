{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Katip.Fused where

import Control.Algebra (Algebra (..), Has, send, type (:+:) (..))
import Control.Carrier.Reader (Reader, ask, local)
import Control.Monad.IO.Class (MonadIO)
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Katip (Katip (..), KatipContext (..), LogContexts, LogEnv, LogStr, Namespace, Severity, logItemM)
import Katip.Core (getLoc)
import Language.Haskell.TH.Syntax (Loc)

data KatipF (m :: Type -> Type) k where
  LogItem :: Maybe Loc -> Severity -> LogStr -> KatipF m ()

logItem :: (HasCallStack, Has KatipF sig m) => Severity -> LogStr -> m ()
logItem sev str = do
  send (LogItem getLoc sev str)

newtype KatipIOC m a = KatipIOC {runKatipIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Has (Reader LogContexts) sig m, Has (Reader LogEnv) sig m, Has (Reader Namespace) sig m) =>
  Algebra (KatipF :+: sig) (KatipIOC m)
  where
  alg hdl sig ctx = case sig of
    L (LogItem cs sev str) -> ctx <$ logItemM cs sev str
    R other -> KatipIOC (alg (runKatipIO . hdl) other ctx)

instance
  (MonadIO m, Algebra sig m, Has (Reader LogEnv) sig m) =>
  Katip (KatipIOC m)
  where
  getLogEnv = KatipIOC $ ask @LogEnv
  localLogEnv f (KatipIOC m) = KatipIOC $ local @LogEnv f m

instance
  (MonadIO m, Algebra sig m, Has (Reader LogEnv) sig m, Has (Reader LogContexts) sig m, Has (Reader Namespace) sig m) =>
  KatipContext (KatipIOC m)
  where
  getKatipContext = KatipIOC $ ask @LogContexts
  localKatipContext f (KatipIOC m) = KatipIOC $ local @LogContexts f m
  getKatipNamespace = KatipIOC $ ask @Namespace
  localKatipNamespace f (KatipIOC m) = KatipIOC $ local @Namespace f m
