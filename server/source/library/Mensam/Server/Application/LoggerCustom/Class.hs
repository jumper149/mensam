module Mensam.Server.Application.LoggerCustom.Class where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind

type MonadLoggerCustom :: (Type -> Type) -> Constraint
class MonadLogger m => MonadLoggerCustom m where
  colorfulLogCapability :: m Bool

instance
  ( MonadTrans t
  , MonadLoggerCustom m
  ) =>
  MonadLoggerCustom (Elevator t m)
  where
  colorfulLogCapability = lift colorfulLogCapability

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( MonadTrans t1
    , MonadLoggerCustom (t2 m)
    ) =>
    MonadLoggerCustom (ComposeT t1 t2 m)
