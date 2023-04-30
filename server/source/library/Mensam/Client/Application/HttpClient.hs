{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application.HttpClient where

import Mensam.Client.Application.HttpClient.Class

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Network.HTTP.Client qualified as Network
import Network.HTTP.Client.TLS qualified as Network

type HttpClientT :: (Type -> Type) -> Type -> Type
newtype HttpClientT m a = MkHttpClientT {unHttpClientT :: ReaderT Network.Manager m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadHttpClient (HttpClientT m) where
  httpManager = MkHttpClientT ask

deriving via
  HttpClientT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadHttpClient (ComposeT HttpClientT t2 m)

runHttpClientT :: HttpClientT m a -> Network.Manager -> m a
runHttpClientT = runReaderT . unHttpClientT

runAppHttpClientT ::
  (MonadIO m, MonadLogger m) =>
  HttpClientT m a ->
  m a
runAppHttpClientT tma = do
  logInfo "Creating new HTTP manager."
  manager <- liftIO $ Network.newManager Network.tlsManagerSettings
  logInfo "Created new HTTP manager."
  runHttpClientT tma manager
