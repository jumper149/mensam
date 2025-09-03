{-# LANGUAGE UndecidableInstances #-}

module Mensam.Client.Application.MensamClient where

import Mensam.Client.Application.HttpClient.Class
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.Application.Options.Class

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Servant.Client

type MensamClientT :: (Type -> Type) -> Type -> Type
type role MensamClientT _ _
newtype MensamClientT m a = MkMensamClientT {unMensamClientT :: ReaderT ClientEnv m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadMensamClient (MensamClientT m) where
  mensamCall call = MkMensamClientT $ do
    clientEnv <- ask
    liftIO $ runClientM call clientEnv

deriving via
  MensamClientT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadMensamClient (ComposeT MensamClientT t2 m)

runMensamClientT :: MensamClientT m a -> ClientEnv -> m a
runMensamClientT = runReaderT . unMensamClientT

runAppMensamClientT ::
  (MonadIO m, MonadHttpClient m, MonadLogger m, MonadOptions m) =>
  MensamClientT m a ->
  m a
runAppMensamClientT tma = do
  logInfo "Creating HTTP client environment."
  manager <- httpManager
  baseUrl <- optionBaseUrl <$> options
  let clientEnv = mkClientEnv manager baseUrl
  logInfo "Created HTTP client environment."
  runMensamClientT tma clientEnv
