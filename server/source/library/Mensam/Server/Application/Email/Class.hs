module Mensam.Server.Application.Email.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Network.Mail.Mime

type MonadEmail :: (Type -> Type) -> Constraint
class Monad m => MonadEmail m where
  sendEmail :: Mail -> m ()

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadEmail m
  ) =>
  MonadEmail (Elevator t m)
  where
  sendEmail = lift . sendEmail

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadEmail (t2 m)
    ) =>
    MonadEmail (ComposeT t1 t2 m)
