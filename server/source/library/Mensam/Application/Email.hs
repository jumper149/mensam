{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.Email where

import Mensam.Application.Configured.Class
import Mensam.Application.Email.Class
import Mensam.Configuration
import Mensam.Configuration.Email

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Text qualified as T
import Network.Mail.Mime

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
      Just MkEmailConfig {emailSendmailPath, emailSendmailArguments} -> do
        lift . liftIO $ renderSendMailCustom emailSendmailPath emailSendmailArguments email
        lift $ logInfo "Sent an email."

deriving via
  EmailT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadIO (t2 m), MonadLogger (t2 m)) => MonadEmail (ComposeT EmailT t2 m)

runEmailT :: EmailT m a -> Maybe EmailConfig -> m a
runEmailT = runReaderT . unEmailT

runAppEmailT :: MonadConfigured m => EmailT m a -> m a
runAppEmailT tma = do
  emailConfig <- configEmailConfig <$> configuration
  runReaderT (unEmailT tma) emailConfig
