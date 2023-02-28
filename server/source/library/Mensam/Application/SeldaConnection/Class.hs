module Mensam.Application.SeldaConnection.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Database.Selda.Backend
import Database.Selda.SQLite

type MonadSeldaConnection :: (Type -> Type) -> Constraint
class Monad m => MonadSeldaConnection m where
  runSeldaTransaction :: SeldaT SQLite IO a -> m a

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadSeldaConnection m
  ) =>
  MonadSeldaConnection (Elevator t m)
  where
  runSeldaTransaction = lift . runSeldaTransaction

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadSeldaConnection (t2 m)
    ) =>
    MonadSeldaConnection (ComposeT t1 t2 m)
