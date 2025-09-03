{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.SeldaPool.Class where

import Control.Monad.Catch
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
  runSeldaTransactionT :: SeldaTransactionT m a -> m (SeldaResult a)

instance
  ( Monad (t m)
  , MonadTransControlIdentity t
  , MonadSeldaPool m
  , MonadIO m
  , MonadMask m
  ) =>
  MonadSeldaPool (Elevator t m)
  where
  runSeldaTransactionT transaction =
    liftWithIdentity $ \runT ->
      runSeldaTransactionT $
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
type role SeldaTransactionT _ _
newtype SeldaTransactionT m a = MkSeldaTransactionT {unSeldaTransactionT :: SeldaT SQLite m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans)
  deriving newtype (MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadSelda)

mapSeldaTransactionT :: (m a -> n b) -> SeldaTransactionT m a -> SeldaTransactionT n b
mapSeldaTransactionT f = MkSeldaTransactionT . S . mapReaderT f . unS . unSeldaTransactionT

type SeldaResult :: Type -> Type
type role SeldaResult _
data SeldaResult a
  = SeldaSuccess a
  | SeldaFailure SomeException
