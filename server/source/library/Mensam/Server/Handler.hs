{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Handler where

import Mensam.Application.Configured.Class
import Mensam.Application.Email.Class
import Mensam.Application.SeldaPool.Class
import Mensam.Server.Handler.RequestHash

import Control.Monad.Base
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose.Stack
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Kind

type Transformers :: Stack
type Transformers =
  NilT
    :.|> RequestHashT

type HandlerT :: (Type -> Type) -> Type -> Type
newtype HandlerT m a = HandlerT {unHandlerT :: StackT Transformers m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadIO, MonadUnliftIO)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadSeldaPool)
  deriving newtype (MonadEmail)

runHandlerT :: MonadLogger m => Hash -> HandlerT m a -> m a
runHandlerT randomHash = runStackT runTransformers . unHandlerT
 where
  runTransformers =
    RunNilT
      :..> runRequestHashT randomHash
      . (logInfo "Starting HTTP request handler." >>)
