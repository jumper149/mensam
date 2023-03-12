{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.Logging where

import Mensam.Application.Environment.Class

import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Data.ByteString.Char8 qualified as B
import Data.Kind
import Data.Time qualified as T
import Data.Time.Format.ISO8601 qualified as T

type TimedLoggingT :: (Type -> Type) -> Type -> Type
newtype TimedLoggingT m a = TimedLoggingT {unTimedLoggingT :: LoggingT m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadLogger (TimedLoggingT m) where
  monadLoggerLog loc logSource logLevel logStr = do
    time <- lift $ liftIO T.getCurrentTime
    let timeInfo = T.iso8601Show time
    TimedLoggingT . monadLoggerLog loc logSource logLevel . toLogStr $
      "@{" <> B.pack timeInfo <> "} " <> fromLogStr (toLogStr logStr)

deriving via
  TimedLoggingT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadLogger (ComposeT TimedLoggingT t2 m)

runTimedLoggingT ::
  (MonadBaseControl IO m, MonadIO m) =>
  Maybe FilePath ->
  LogLevel ->
  TimedLoggingT m a ->
  m a
runTimedLoggingT maybeFilePath configuredLogLevel = run . withFilter . unTimedLoggingT
 where
  run = maybe runStdoutLoggingT runFileLoggingT maybeFilePath
  withFilter = filterLogger $ \_src lvl -> lvl >= configuredLogLevel

runAppTimedLoggingT :: (MonadBaseControl IO m, MonadEnvironment m, MonadIO m) => TimedLoggingT m a -> m a
runAppTimedLoggingT tma = do
  maybeLogFile <- environmentVariable $ EnvVar @"MENSAM_LOG_FILE"
  logLevel <- environmentVariable $ EnvVar @"MENSAM_LOG_LEVEL"
  runTimedLoggingT maybeLogFile logLevel tma

logLine ::
  MonadLogger m =>
  LogLine ->
  m ()
logLine (loc, logSource, logLevel, logStr) = monadLoggerLog loc logSource logLevel logStr
