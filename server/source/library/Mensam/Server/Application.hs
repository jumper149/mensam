{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application where

import Mensam.Server.Application.Configured
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Environment
import Mensam.Server.Application.Environment.Acquisition
import Mensam.Server.Application.Logging
import Mensam.Server.Application.Secret
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool
import Mensam.Server.Application.SeldaPool.Class

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Catch.OrphanInstances ()
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Compose.Stack
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.Foldable
import Data.Kind

type Transformers :: Stack
type Transformers =
  NilT
    :.|> EnvironmentT
    :.|> TimedLoggingT
    :.|> ConfiguredT
    :.|> SecretT
    :.|> SeldaPoolT
    :.|> EmailT

type ApplicationT :: (Type -> Type) -> Type -> Type
newtype ApplicationT m a = ApplicationT {unApplicationT :: StackT Transformers m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadIO, MonadUnliftIO)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadSecret)
  deriving newtype (MonadSeldaPool)
  deriving newtype (MonadEmail)

runApplicationT ::
  (MonadBaseControlIdentity IO m, MonadUnliftIO m) =>
  ApplicationT m a ->
  m a
runApplicationT app = do
  (env, preLog) <- runWriterLoggingT $ do
    logInfo "Startup."
    acquireEnvironment

  let runTransformers =
        RunNilT
          :..> runEnvironmentT env
          :..> runAppTimedLoggingT
          . (traverse_ logLine preLog >>)
          :..> runAppConfiguredT
          :..> runAppSecretT
          :..> runSeldaPoolT
          :..> runAppEmailT

  runStackT runTransformers $ unApplicationT app
