{-# LANGUAGE UndecidableInstances #-}

module Homepage.Application.Configured where

import Homepage.Application.Configured.Acquisition
import Homepage.Application.Configured.Class
import Homepage.Application.Environment.Class
import Homepage.Configuration

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind

type ConfiguredT :: (Type -> Type) -> Type -> Type
newtype ConfiguredT m a = ConfiguredT {unConfiguredT :: ReaderT Configuration m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadConfigured (ConfiguredT m) where
  configuration = ConfiguredT ask

deriving via
  ConfiguredT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadConfigured (ComposeT ConfiguredT t2 m)

runConfiguredT :: ConfiguredT m a -> Configuration -> m a
runConfiguredT = runReaderT . unConfiguredT

runAppConfiguredT ::
  (MonadEnvironment m, MonadIO m, MonadLogger m) =>
  ConfiguredT m a ->
  m a
runAppConfiguredT tma = do
  maybeConfig <- acquireConfig
  case maybeConfig of
    Nothing -> error "No configuration."
    Just config -> runConfiguredT tma config
