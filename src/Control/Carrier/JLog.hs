{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.JLog where

import Control.Algebra (Algebra (..), type (:+:) (..))
import Control.Carrier.Reader (ReaderC (..), runReader)
import Control.Effect.JLog (JLog (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T

newtype JLogIOC m a = JLogIOC {runJLog :: ReaderC [(T.Text, T.Text)] m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  ( MonadIO m,
    Algebra sig m
  ) =>
  Algebra (JLog :+: sig) (JLogIOC m)
  where
  alg hdl sig ctx = JLogIOC $ ReaderC $ \r -> case sig of
    L (LogItem cs msg) -> do
      liftIO $ T.putStrLn msg
      return ctx
    L (WithContext pairs action) -> runReader (r <> pairs) $ runJLog $ hdl (action <$ ctx)
    R other -> alg (runReader r . runJLog . hdl) other ctx
