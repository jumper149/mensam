{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Secret where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Secrets

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Text qualified as T

type SecretT :: (Type -> Type) -> Type -> Type
newtype SecretT m a = SecretT {unSecretT :: ReaderT Secrets m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance Monad m => MonadSecret (SecretT m) where
  secrets = SecretT ask

deriving via
  SecretT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadSecret (ComposeT SecretT t2 m)

runSecretT :: SecretT m a -> Secrets -> m a
runSecretT = runReaderT . unSecretT

runAppSecretT ::
  (MonadConfigured m, MonadLogger m, MonadSeldaPool m) =>
  SecretT m a ->
  m a
runAppSecretT tma = do
  logInfo "Acquiring secrets."

  secretsJwk <- do
    logInfo "Acquiring JWK."
    seldaResult <- runSeldaTransactionT $ do
      maybeOldJwk <- jwkGetLatest
      case maybeOldJwk of
        Just jwk -> do
          lift $ logInfo "Using existing JWK."
          pure jwk
        Nothing -> do
          lift $ logInfo "Currently there is no JWK set. Setting new JWK."
          jwk <- jwkSetLatest
          lift $ logInfo "Using newly set JWK."
          pure jwk
    case seldaResult of
      SeldaFailure err -> do
        logError $ "Failed to acquire JWK: " <> T.pack (show err)
        error "No JWK."
      SeldaSuccess jwk -> do
        logInfo "Acquired JWK successfully."
        pure jwk

  runSecretT tma $ MkSecrets {secretsJwk}
