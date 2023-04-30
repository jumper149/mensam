module Mensam.Client.Application.Options.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Servant.Client qualified

type MonadOptions :: (Type -> Type) -> Constraint
class Monad m => MonadOptions m where
  options :: m Options

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadOptions m
  ) =>
  MonadOptions (Elevator t m)
  where
  options = lift options

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadOptions (t2 m)
    ) =>
    MonadOptions (ComposeT t1 t2 m)

type Options :: Type
newtype Options = MkOptions
  { optionBaseUrl :: Servant.Client.BaseUrl
  }
