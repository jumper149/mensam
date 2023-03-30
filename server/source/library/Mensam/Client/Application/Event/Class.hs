module Mensam.Client.Application.Event.Class where

import Mensam.Client.UI.Brick.Events

import Brick.BChan
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind

type MonadEvent :: (Type -> Type) -> Constraint
class Monad m => MonadEvent m where
  sendEvent :: ClientEvent -> m ()
  eventChannel :: m (BChan ClientEvent)

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadEvent m
  ) =>
  MonadEvent (Elevator t m)
  where
  sendEvent = lift . sendEvent
  eventChannel = lift eventChannel

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadEvent (t2 m)
    ) =>
    MonadEvent (ComposeT t1 t2 m)
