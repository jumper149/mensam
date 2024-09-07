module Mensam.Server.Server.Handler.Profiler.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind

type MonadProfiler :: (Type -> Type) -> Constraint
class Monad m => MonadProfiler m where
  profilerDuration :: m ()

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadProfiler m
  ) =>
  MonadProfiler (Elevator t m)
  where
  profilerDuration = lift profilerDuration

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadProfiler (t2 m)
    ) =>
    MonadProfiler (ComposeT t1 t2 m)
