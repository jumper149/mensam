{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.SeldaConnection where

import Mensam.Application.SeldaConnection.Class

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Pool qualified as P
import Data.Time qualified as T
import Database.Selda.Backend
import Database.Selda.SQLite

type SeldaConnectionT :: (Type -> Type) -> Type -> Type
newtype SeldaConnectionT m a = SeldaConnectionT {unSeldaConnectionT :: ReaderT (P.Pool (SeldaConnection SQLite)) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadIO m) => MonadSeldaConnection (SeldaConnectionT m) where
  runSeldaTransaction tma = do
    pool <- SeldaConnectionT ask
    lift $ liftIO $ P.withResource pool $ \connection ->
      runSeldaT tma connection

deriving via
  SeldaConnectionT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadIO (t2 m)) => MonadSeldaConnection (ComposeT SeldaConnectionT t2 m)

runSeldaConnectionT :: (MonadIO m, MonadLogger m) => SeldaConnectionT m a -> m a
runSeldaConnectionT tma = do
  let filepath :: FilePath = "mensam-example.sqlite" -- TODO: Configure.
  logInfo "Create SQLite connection pool for Selda."
  pool <- liftIO $ P.createPool (sqliteOpen filepath) seldaClose 5 (T.secondsToNominalDiffTime 5) 5
  runReaderT (unSeldaConnectionT tma) pool
