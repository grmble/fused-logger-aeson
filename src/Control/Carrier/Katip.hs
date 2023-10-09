{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Katip where

import Control.Algebra (Algebra (..), Has, type (:+:) (..))
import Control.Carrier.Reader (Reader, ask, local)
import Control.Effect.Katip
  ( Katip (..),
    KatipContext (..),
    KatipF (..),
    Namespace,
  )
import Control.Monad.IO.Class (MonadIO)
import Katip qualified as K

newtype KatipIOC m a = KatipIOC {runKatipIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  ( MonadIO m,
    Algebra sig m,
    Has (Reader K.LogContexts) sig m,
    Has (Reader K.LogEnv) sig m,
    Has (Reader Namespace) sig m
  ) =>
  Algebra (KatipF :+: sig) (KatipIOC m)
  where
  alg hdl sig ctx = case sig of
    L (LogItem cs sev str) -> ctx <$ K.logItemM cs sev str
    R other -> KatipIOC (alg (runKatipIO . hdl) other ctx)

instance
  (MonadIO m, Algebra sig m, Has (Reader K.LogEnv) sig m) =>
  Katip (KatipIOC m)
  where
  getLogEnv = KatipIOC $ ask @K.LogEnv
  localLogEnv f (KatipIOC m) = KatipIOC $ local @K.LogEnv f m

instance
  ( MonadIO m,
    Algebra sig m,
    Has (Reader K.LogEnv) sig m,
    Has (Reader K.LogContexts) sig m,
    Has (Reader Namespace) sig m
  ) =>
  KatipContext (KatipIOC m)
  where
  getKatipContext = KatipIOC $ ask @K.LogContexts
  localKatipContext f (KatipIOC m) = KatipIOC $ local @K.LogContexts f m
  getKatipNamespace = KatipIOC $ ask @Namespace
  localKatipNamespace f (KatipIOC m) = KatipIOC $ local @Namespace f m
