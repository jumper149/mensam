module Mensam.Application.Configured.Class where

import Mensam.Configuration

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind

type MonadConfigured :: (Type -> Type) -> Constraint
class Monad m => MonadConfigured m where
  configuration :: m Configuration

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadConfigured m
  ) =>
  MonadConfigured (Elevator t m)
  where
  configuration = lift configuration

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadConfigured (t2 m)
    ) =>
    MonadConfigured (ComposeT t1 t2 m)
