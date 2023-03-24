module Mensam.Client.Application.HttpClient.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Network.HTTP.Client qualified as Network

type MonadHttpClient :: (Type -> Type) -> Constraint
class Monad m => MonadHttpClient m where
  httpManager :: m Network.Manager

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadHttpClient m
  ) =>
  MonadHttpClient (Elevator t m)
  where
  httpManager = lift httpManager

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadHttpClient (t2 m)
    ) =>
    MonadHttpClient (ComposeT t1 t2 m)
