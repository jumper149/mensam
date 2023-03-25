module Mensam.Server.Application.Secret.Class where

import Mensam.Server.Secrets

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind

type MonadSecret :: (Type -> Type) -> Constraint
class Monad m => MonadSecret m where
  secrets :: m Secrets

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadSecret m
  ) =>
  MonadSecret (Elevator t m)
  where
  secrets = lift secrets

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadSecret (t2 m)
    ) =>
    MonadSecret (ComposeT t1 t2 m)
