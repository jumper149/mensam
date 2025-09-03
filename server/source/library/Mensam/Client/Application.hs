{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application where

import Mensam.Client.Application.Event
import Mensam.Client.Application.Event.Class
import Mensam.Client.Application.HttpClient
import Mensam.Client.Application.MensamClient
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.Application.Options
import Mensam.Client.Application.Options.Class
import Mensam.Client.UI.Brick.Events (ClientEvent)

import Brick.BChan (BChan)
import Control.Monad.IO.Class
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
    :.|> OptionsT
    :.|> NoLoggingT
    :.|> HttpClientT
    :.|> MensamClientT
    :.|> EventT

type ApplicationT :: (Type -> Type) -> Type -> Type
type role ApplicationT _ _
newtype ApplicationT m a = ApplicationT {unApplicationT :: StackT Transformers m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadOptions)
  deriving newtype (MonadLogger)
  deriving newtype (MonadMensamClient)
  deriving newtype (MonadEvent)

runApplicationT ::
  MonadIO m =>
  BChan ClientEvent ->
  ApplicationT m a ->
  m a
runApplicationT chan app = do
  let
    runTransformers :: MonadIO m => RunStackT Transformers m a
    runTransformers =
      RunNilT
        :..> runAppOptionsT
        :..> runNoLoggingT
        :..> runAppHttpClientT
        :..> runAppMensamClientT
        :..> (`runEventT` chan)

  runStackT runTransformers $ unApplicationT app
