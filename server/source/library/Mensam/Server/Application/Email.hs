{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.Email where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.Email

import Control.Exception
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Network.Mail.Mime
import Network.Mail.SMTP
import Text.Email.Text

type EmailT :: (Type -> Type) -> Type -> Type
newtype EmailT m a = MkEmailT {unEmailT :: ReaderT (Maybe EmailConfig) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadIO m, MonadLogger m) => MonadEmail (EmailT m) where
  sendEmail email = do
    maybeEmailConfig <- MkEmailT ask
    lift $ logDebug $ "Sending email: " <> T.pack (show email)
    let
      mimeMailFromEmailConfig :: EmailConfig -> Mail
      mimeMailFromEmailConfig MkEmailConfig {emailUsername} =
        Mail
          { mailFrom =
              Address
                { addressName = Just "Mensam"
                , addressEmail = T.pack emailUsername
                }
          , mailTo =
              [ Address
                  { addressName = Nothing
                  , addressEmail = toText $ emailRecipient email
                  }
              ]
          , mailCc = []
          , mailBcc = []
          , mailHeaders = [("Subject", emailTitle email)]
          , mailParts = [[Network.Mail.Mime.htmlPart $ TL.fromStrict $ emailBodyHtml email]]
          }
    case maybeEmailConfig of
      Nothing -> do
        lift $ logWarn "A requested email was not sent."
        let
          placeholderEmailConfig =
            MkEmailConfig
              { emailHostname = undefined
              , emailPort = undefined
              , emailUsername = "nousernamegiven@example.com"
              , emailPassword = undefined
              , emailTls = undefined
              }
          mimeMail = mimeMailFromEmailConfig placeholderEmailConfig
        renderedMail <- lift $ liftIO $ renderMail' mimeMail
        lift $ logDebug $ "Tried to send email: " <> T.pack (show renderedMail)
        pure EmailFailedToSend
      Just emailConfig -> do
        let
          port = toEnum $ fromEnum $ emailPort emailConfig
          mimeMail = mimeMailFromEmailConfig emailConfig
        sendMailResult <-
          lift . liftIO . try $
            if emailTls emailConfig
              then sendMailWithLoginTLS' (emailHostname emailConfig) port (emailUsername emailConfig) (emailPassword emailConfig) mimeMail
              else sendMailWithLogin' (emailHostname emailConfig) port (emailUsername emailConfig) (emailPassword emailConfig) mimeMail
        case sendMailResult of
          Left (err :: SomeException) -> do
            lift $ logWarn $ "Failed to actually send an email: " <> T.pack (show err)
            pure EmailFailedToSend
          Right () -> do
            lift $ logInfo "Sent an email."
            pure EmailSent

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
