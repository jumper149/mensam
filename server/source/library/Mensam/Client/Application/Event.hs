{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application.Event where

import Mensam.Client.Application.Event.Class
import Mensam.Client.Brick.Events

import Control.Concurrent.Chan
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind

type EventT :: (Type -> Type) -> Type -> Type
newtype EventT m a = MkEventT {unEventT :: ReaderT (Chan ClientEvent) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadEvent (EventT m) where
  sendEvent event = MkEventT $ do
    chan <- ask
    liftIO $ writeChan chan event
  eventChannel = MkEventT ask

deriving via
  EventT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadEvent (ComposeT EventT t2 m)

runEventT :: EventT m a -> Chan ClientEvent -> m a
runEventT = runReaderT . unEventT

runAppEventT ::
  (MonadIO m, MonadLogger m) =>
  EventT m a ->
  m a
runAppEventT tma = do
  logInfo "Creating new event channel."
  chan <- liftIO newChan
  logInfo "Created new event channel."
  runEventT tma chan
