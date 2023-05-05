{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Email where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.Email

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Text qualified as T
import Network.Mail.SMTP

type EmailT :: (Type -> Type) -> Type -> Type
newtype EmailT m a = MkEmailT {unEmailT :: ReaderT (Maybe EmailConfig) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadIO m, MonadLogger m) => MonadEmail (EmailT m) where
  sendEmail email = do
    maybeEmailConfig <- MkEmailT ask
    lift $ logDebug $ "Sending email: " <> T.pack (show email)
    case maybeEmailConfig of
      Nothing -> do
        lift $ logWarn "A requested email was not sent."
      Just MkEmailConfig {emailHostname, emailPort, emailUsername, emailPassword, emailTls} -> do
        let port = toEnum $ fromEnum emailPort
        if emailTls
          then lift . liftIO $ sendMailWithLoginTLS' emailHostname port emailUsername emailPassword email
          else lift . liftIO $ sendMailWithLogin' emailHostname port emailUsername emailPassword email
        lift $ logInfo "Sent an email."

deriving via
  EmailT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadIO (t2 m), MonadLogger (t2 m)) => MonadEmail (ComposeT EmailT t2 m)

runEmailT :: Maybe EmailConfig -> EmailT m a -> m a
runEmailT config tma = runReaderT (unEmailT tma) config

runAppEmailT :: MonadConfigured m => EmailT m a -> m a
runAppEmailT tma = do
  config <- configEmailConfig <$> configuration
  runEmailT config tma
