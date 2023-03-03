{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.SeldaConnection where

import Mensam.Application.Configured.Class
import Mensam.Application.SeldaConnection.Class
import Mensam.Configuration

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Pool qualified as P
import Data.Time qualified as T
import Database.Selda.Backend
import Database.Selda.SQLite

type SeldaConnectionT :: (Type -> Type) -> Type -> Type
newtype SeldaConnectionT m a = SeldaConnectionT {unSeldaConnectionT :: ReaderT (P.Pool (SeldaConnection SQLite)) m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance MonadIO m => MonadSeldaConnection (SeldaConnectionT m) where
  runSeldaTransaction tma = do
    pool <- SeldaConnectionT ask
    lift $ liftIO $ P.withResource pool $ \connection ->
      runSeldaT tma connection

deriving via
  SeldaConnectionT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadSeldaConnection (ComposeT SeldaConnectionT t2 m)

runSeldaConnectionT :: (MonadConfigured m, MonadLogger m, MonadUnliftIO m) => SeldaConnectionT m a -> m a
runSeldaConnectionT tma = do
  logDebug "Initializing SQLite connection pool for Selda."
  filepath <- configSqlitePath <$> configuration
  pool <- withRunInIO $ \runInIO -> do
    let
      openConnection :: IO (SeldaConnection SQLite)
      openConnection = runInIO $ do
        logDebug "Opening SQLite connection."
        connection <- liftIO $ sqliteOpen filepath
        logInfo "Opened SQLite connection successfully."
        pure connection
      closeConnection :: SeldaConnection SQLite -> IO ()
      closeConnection connection = runInIO $ do
        logDebug "Closing SQLite connection."
        liftIO $ seldaClose connection
        logInfo "Closed SQLite connection successfully."
    liftIO $ P.createPool openConnection closeConnection 5 (T.secondsToNominalDiffTime 5) 5
  logInfo "Initialized SQLite connection pool for Selda successfully."
  runReaderT (unSeldaConnectionT tma) pool
