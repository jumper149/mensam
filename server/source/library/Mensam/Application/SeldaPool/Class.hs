module Mensam.Application.SeldaPool.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Database.Selda.Backend
import Database.Selda.SQLite

type MonadSeldaPool :: (Type -> Type) -> Constraint
class Monad m => MonadSeldaPool m where
  runSeldaTransaction :: SeldaT SQLite IO a -> m a

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadSeldaPool m
  ) =>
  MonadSeldaPool (Elevator t m)
  where
  runSeldaTransaction = lift . runSeldaTransaction

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadSeldaPool (t2 m)
    ) =>
    MonadSeldaPool (ComposeT t1 t2 m)
