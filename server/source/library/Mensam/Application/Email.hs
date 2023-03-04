{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.Email where

import Mensam.Application.Email.Class

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind

type EmailT :: (Type -> Type) -> Type -> Type
newtype EmailT m a = MkEmail {unEmailT :: ReaderT () m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadEmail (EmailT m) where
  sendEmail = undefined

deriving via
  EmailT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadEmail (ComposeT EmailT t2 m)

runEmailT :: EmailT m a -> () -> m a
runEmailT = runReaderT . unEmailT
