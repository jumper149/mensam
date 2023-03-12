{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Environment where

import Mensam.Server.Application.Environment.Class
import Mensam.Server.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Singletons

type EnvironmentT :: (Type -> Type) -> Type -> Type
newtype EnvironmentT m a = EnvironmentT {unEnvironmentT :: ReaderT Environment m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadEnvironment (EnvironmentT m) where
  environmentVariable ::
    forall envVar.
    SingI envVar =>
    ProxyEnvVarName (EnvVarName envVar) ->
    EnvironmentT m (EnvVarValue envVar)
  environmentVariable _ = do
    environment <- EnvironmentT ask
    let accessEnvVar = getEnvironment environment
    pure $ accessEnvVar $ sing @envVar

deriving via
  EnvironmentT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadEnvironment (ComposeT EnvironmentT t2 m)

runEnvironmentT :: Environment -> EnvironmentT m a -> m a
runEnvironmentT env tma = runReaderT (unEnvironmentT tma) env
