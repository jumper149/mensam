{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application where

import Mensam.Client.Application.HttpClient
import Mensam.Client.Application.MensamClient
import Mensam.Client.Application.MensamClient.Class

import Control.Monad.Except
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Compose.Stack
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Kind

type Transformers :: Stack
type Transformers =
  NilT
    :.|> NoLoggingT
    :.|> HttpClientT
    :.|> MensamClientT

type ApplicationT :: (Type -> Type) -> Type -> Type
newtype ApplicationT m a = ApplicationT {unApplicationT :: StackT Transformers m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadLogger)
  deriving newtype (MonadMensamClient)

runApplicationT ::
  MonadIO m =>
  ApplicationT m a ->
  m a
runApplicationT app = do
  let
    runTransformers :: MonadIO m => RunStackT Transformers m a
    runTransformers =
      RunNilT
        :..> runNoLoggingT
        :..> runAppHttpClientT
        :..> runAppMensamClientT

  runStackT runTransformers $ unApplicationT app
