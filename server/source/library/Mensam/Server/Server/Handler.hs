{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Server.Handler where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Server.Handler.RequestHash

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
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
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadSecret)
  deriving newtype (MonadSeldaPool)
  deriving newtype (MonadEmail)

runHandlerT :: MonadLogger m => Hash -> HandlerT m a -> m a
runHandlerT randomHash = runStackT runTransformers . unHandlerT
 where
  runTransformers =
    RunNilT
      :..> runRequestHashT randomHash
        . (logInfo "Starting HTTP request handler." >>)
