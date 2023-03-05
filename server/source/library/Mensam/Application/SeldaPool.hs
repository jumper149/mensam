{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Mensam.Application.SeldaPool where

import Mensam.Application.Configured.Class
import Mensam.Application.SeldaPool.Class
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
import Database.Selda
import Database.Selda.Backend
import Database.Selda.SQLite

type SeldaPoolT :: (Type -> Type) -> Type -> Type
newtype SeldaPoolT m a = SeldaPoolT {unSeldaPoolT :: ReaderT SeldaPoolContext m a}
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)
  deriving newtype (MonadIO)

instance MonadUnliftIO m => MonadSelda (SeldaPoolT m) where
  type Backend (SeldaPoolT m) = SQLite
  withConnection computation = do
    maybeTransactionConnection <- seldaTransactionConnection <$> SeldaPoolT ask
    case maybeTransactionConnection of
      Just connection -> computation connection
      Nothing -> do
        pool <- seldaConnectionPool <$> SeldaPoolT ask
        liftWithIdentity $ \runT ->
          withRunInIO $ \runInIO ->
            P.withResource pool $ \connection ->
              runInIO $ runT $ computation connection
  transact computation = do
    maybeTransactionConnection <- seldaTransactionConnection <$> SeldaPoolT ask
    case maybeTransactionConnection of
      Just _connection -> computation
      Nothing -> do
        pool <- seldaConnectionPool <$> SeldaPoolT ask
        liftWithIdentity $ \runT ->
          withRunInIO $ \runInIO ->
            P.withResource pool $ \connection ->
              runInIO $ runT $ SeldaPoolT $ do
                let
                  setTransactionConnection :: SeldaPoolContext -> SeldaPoolContext
                  setTransactionConnection context = context {seldaTransactionConnection = Just connection}
                local setTransactionConnection $ unSeldaPoolT computation

instance MonadIO m => MonadSeldaPool (SeldaPoolT m) where
  runSeldaTransaction tma = do
    pool <- seldaConnectionPool <$> SeldaPoolT ask
    lift $ liftIO $ P.withResource pool $ \connection ->
      runSeldaT (transaction tma) connection

deriving via
  SeldaPoolT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadSeldaPool (ComposeT SeldaPoolT t2 m)

runSeldaPoolT :: (MonadConfigured m, MonadLogger m, MonadUnliftIO m) => SeldaPoolT m a -> m a
runSeldaPoolT tma = do
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
  let context =
        MkSeldaPoolContext
          { seldaConnectionPool = pool
          , seldaTransactionConnection = Nothing
          }
  runReaderT (unSeldaPoolT tma) context

type SeldaPoolContext :: Type
data SeldaPoolContext = MkSeldaPoolContext
  { seldaConnectionPool :: P.Pool (SeldaConnection SQLite)
  , seldaTransactionConnection :: Maybe (SeldaConnection SQLite)
  }
