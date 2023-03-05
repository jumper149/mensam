{-# LANGUAGE UndecidableInstances #-}
-- TODO: Fix with new GHC version, that doesn't complain.
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Mensam.Application.SeldaPool.Class where

import Control.Monad.Catch
import Control.Monad.Catch.OrphanInstances ()
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Reader
import Data.Kind
import Database.Selda.Backend
import Database.Selda.Backend.Internal
import Database.Selda.SQLite

type MonadSeldaPool :: (Type -> Type) -> Constraint
class (Monad m, MonadMask (SeldaTransactionT m), MonadSelda (SeldaTransactionT m)) => MonadSeldaPool m where
  runSeldaTransaction :: SeldaTransactionT m a -> m a

instance
  ( Monad (t m)
  , MonadTransControlIdentity t
  , MonadSeldaPool m
  , MonadIO m
  , MonadMask m
  ) =>
  MonadSeldaPool (Elevator t m)
  where
  runSeldaTransaction transaction =
    liftWithIdentity $ \runT ->
      runSeldaTransaction $
        mapSeldaTransactionT runT transaction

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTransControlIdentity t1
    , MonadSeldaPool (t2 m)
    , MonadIO (t2 m)
    , MonadMask (t2 m)
    , MonadIO m
    , MonadTrans (ComposeT t1 t2)
    ) =>
    MonadSeldaPool (ComposeT t1 t2 m)

type SeldaTransactionT :: (Type -> Type) -> Type -> Type
newtype SeldaTransactionT m a = MkSeldaTransactionT {unSeldaTransactionT :: SeldaT SQLite m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans)
  deriving newtype (MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadSelda)

mapSeldaTransactionT :: (m a -> n b) -> SeldaTransactionT m a -> SeldaTransactionT n b
mapSeldaTransactionT f = MkSeldaTransactionT . S . mapReaderT f . unS . unSeldaTransactionT
