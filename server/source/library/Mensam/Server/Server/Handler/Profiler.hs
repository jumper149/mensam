{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Server.Handler.Profiler where

import Mensam.Server.Server.Handler.Profiler.Class

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as Clock

type ProfilerT :: (Type -> Type) -> Type -> Type
newtype ProfilerT m a = ProfilerT {unProfilerT :: ReaderT Clock.POSIXTime m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadIO m, MonadLogger m) => MonadProfiler (ProfilerT m) where
  profilerDuration = do
    referenceTime <- ProfilerT ask
    currentTime <- lift $ liftIO Clock.getPOSIXTime
    let duration = currentTime - referenceTime
    lift $ logOther logLevel $ T.pack $ show duration

deriving via
  ProfilerT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadIO (t2 m), MonadLogger (t2 m)) => MonadProfiler (ComposeT ProfilerT t2 m)

runProfilerT :: (MonadIO m, MonadLogger m) => ProfilerT m a -> m a
runProfilerT tma = do
  referenceTime <- liftIO Clock.getPOSIXTime
  runReaderT (unProfilerT (tma <* profilerDuration)) referenceTime

logLevel :: LogLevel
logLevel = LevelOther "Profiler"
