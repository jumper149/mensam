{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application.Options where

import Mensam.Client.Application.Options.Class

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind

type OptionsT :: (Type -> Type) -> Type -> Type
newtype OptionsT m a = MkOptionsT {unOptionsT :: ReaderT Options m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadOptions (OptionsT m) where
  options = MkOptionsT ask

deriving via
  OptionsT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadOptions (ComposeT OptionsT t2 m)

runOptionsT :: OptionsT m a -> Options -> m a
runOptionsT = runReaderT . unOptionsT

runAppOptionsT ::
  (MonadIO m, MonadLogger m) =>
  OptionsT m a ->
  m a
runAppOptionsT tma = do
  logInfo "Parsing command line options."
  runOptionsT tma undefined -- TODO
