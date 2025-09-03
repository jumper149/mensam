{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application.Event where

import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Events

import Brick.BChan
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind

type EventT :: (Type -> Type) -> Type -> Type
type role EventT _ _
newtype EventT m a = MkEventT {unEventT :: ReaderT (BChan ClientEvent) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadEvent (EventT m) where
  sendEvent event = MkEventT $ do
    chan <- ask
    liftIO $ writeBChan chan event
  eventChannel = MkEventT ask

deriving via
  EventT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadEvent (ComposeT EventT t2 m)

runEventT :: EventT m a -> BChan ClientEvent -> m a
runEventT = runReaderT . unEventT

runAppEventT ::
  (MonadIO m, MonadLogger m) =>
  EventT m a ->
  m a
runAppEventT tma = do
  logInfo "Creating new event channel."
  chan <- liftIO $ newBChan 10
  logInfo "Created new event channel."
  runEventT tma chan
