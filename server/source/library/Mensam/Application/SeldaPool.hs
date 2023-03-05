{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.SeldaPool where

import Mensam.Application.Configured.Class
import Mensam.Application.SeldaPool.Class
import Mensam.Configuration
import Mensam.Configuration.SQLite

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Reader
import Data.Kind
import Data.Pool qualified as P
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite

type SeldaPoolT :: (Type -> Type) -> Type -> Type
newtype SeldaPoolT m a = SeldaPoolT {unSeldaPoolT :: ReaderT SeldaPoolContext m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadIO)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)

instance (MonadLogger m, MonadMask m, MonadUnliftIO m) => MonadSeldaPool (SeldaPoolT m) where
  runSeldaTransactionT tma = do
    pool <- seldaConnectionPool <$> SeldaPoolT ask
    lift $ logDebug "Starting SQLite transaction."
    let transactionComputation =
          liftWithIdentity $ \runT ->
            withRunInIO $ \runInIO ->
              P.withResource pool $ \connection ->
                (`runSeldaT` connection) $
                  transaction $
                    unSeldaTransactionT $
                      mapSeldaTransactionT (runInIO . runT) tma
    result <- catch transactionComputation $ \case
      (err :: SomeException) -> do
        lift $ logError "SQLite transaction failed and the database was rolled back."
        throwM err
    lift $ logInfo "Committed SQLite transaction."
    pure result

deriving via
  SeldaPoolT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    (MonadLogger (t2 m), MonadMask (t2 m), MonadUnliftIO (t2 m), MonadIO (ComposeT SeldaPoolT t2 m)) => MonadSeldaPool (ComposeT SeldaPoolT t2 m)

runSeldaPoolT :: (MonadConfigured m, MonadLogger m, MonadUnliftIO m) => SeldaPoolT m a -> m a
runSeldaPoolT tma = do
  logDebug "Initializing SQLite connection pool for Selda."
  config <- configSqlite <$> configuration
  pool <- withRunInIO $ \runInIO -> do
    let
      openConnection :: IO (SeldaConnection SQLite)
      openConnection = runInIO $ do
        logDebug "Opening SQLite connection."
        connection <- liftIO $ sqliteOpen $ sqliteFilepath config
        logInfo "Opened SQLite connection successfully."
        pure connection
      closeConnection :: SeldaConnection SQLite -> IO ()
      closeConnection connection = runInIO $ do
        logDebug "Closing SQLite connection."
        liftIO $ seldaClose connection
        logInfo "Closed SQLite connection successfully."
      poolConfig =
        P.defaultPoolConfig
          openConnection
          closeConnection
          (sqliteConnectionPoolTimeoutSeconds config)
          (sqliteConnectionPoolMaxNumberOfConnections config)
    P.newPool poolConfig
  logInfo "Initialized SQLite connection pool for Selda successfully."
  let context =
        MkSeldaPoolContext
          { seldaConnectionPool = pool
          }
  runReaderT (unSeldaPoolT tma) context

type SeldaPoolContext :: Type
newtype SeldaPoolContext = MkSeldaPoolContext
  { seldaConnectionPool :: P.Pool (SeldaConnection SQLite)
  }
