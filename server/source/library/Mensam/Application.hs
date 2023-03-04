{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application where

import Mensam.Application.Configured
import Mensam.Application.Configured.Class
import Mensam.Application.Email
import Mensam.Application.Email.Class
import Mensam.Application.Environment
import Mensam.Application.Environment.Acquisition
import Mensam.Application.Logging
import Mensam.Application.SeldaConnection
import Mensam.Application.SeldaConnection.Class

import Control.Monad.Base
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
    :.|> SeldaConnectionT
    :.|> EmailT

type ApplicationT :: (Type -> Type) -> Type -> Type
newtype ApplicationT m a = ApplicationT {unApplicationT :: StackT Transformers m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadBase b, MonadBaseControl b, MonadBaseControlIdentity b)
  deriving newtype (MonadIO, MonadUnliftIO)
  deriving newtype (MonadLogger)
  deriving newtype (MonadConfigured)
  deriving newtype (MonadSeldaConnection)
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
          :..> runSeldaConnectionT
          :..> runAppEmailT

  runStackT runTransformers $ unApplicationT app
