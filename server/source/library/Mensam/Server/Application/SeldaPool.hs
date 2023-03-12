{-# LANGUAGE UndecidableInstances #-}

module Mensam.Server.Application.SeldaPool where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.SQLite

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
import Data.Text qualified as T
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
    alreadyInTransaction <- seldaAlreadyInTransaction <$> SeldaPoolT ask
    if alreadyInTransaction
      then do
        let msg :: Text = "Tried to start nested SQLite transaction. Nested transactions are not allowed to prevent deadlocks."
        lift $ logError msg
        throwM $ SqlError $ show msg
      else do
        pool <- seldaConnectionPool <$> SeldaPoolT ask
        let transactionComputation =
              liftWithIdentity $ \runT ->
                withRunInIO $ \runInIO ->
                  P.withResource pool $ \connection ->
                    (`runSeldaT` connection) $ do
                      lift $ runInIO $ logDebug "Starting SQLite transaction."
                      result <-
                        transaction $
                          unSeldaTransactionT $
                            mapSeldaTransactionT (runInIO . runT . localSetAlreadyInTransaction) tma
                      lift $ runInIO $ logInfo "Committed SQLite transaction."
                      pure $ SeldaSuccess result
        catch transactionComputation $ \case
          (err :: SomeException) -> do
            lift $ logWarn $ "SQLite transaction failed and the database was rolled back: " <> T.pack (show err)
            pure $ SeldaFailure err
   where
    localSetAlreadyInTransaction :: SeldaPoolT m a -> SeldaPoolT m a
    localSetAlreadyInTransaction = SeldaPoolT . local (\context -> context {seldaAlreadyInTransaction = True}) . unSeldaPoolT

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
          , seldaAlreadyInTransaction = False
          }
  runReaderT (unSeldaPoolT tma) context

type SeldaPoolContext :: Type
data SeldaPoolContext = MkSeldaPoolContext
  { seldaConnectionPool :: P.Pool (SeldaConnection SQLite)
  , seldaAlreadyInTransaction :: Bool
  }
