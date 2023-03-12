{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Logger.OrphanInstances () where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Reader
import Data.Kind

deriving via
  ReaderT (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  instance
    MonadTransControlIdentity LoggingT

instance (Monad (t m), MonadTrans t, MonadLogger m) => MonadLogger (Elevator t m) where
  monadLoggerLog loc logSource logLevel = lift . monadLoggerLog loc logSource logLevel

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadLogger (t2 m)
    ) =>
    MonadLogger (ComposeT t1 t2 m)

deriving via
  LoggingT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadLogger (ComposeT LoggingT t2 m)

deriving via
  NoLoggingT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    MonadIO (t2 m) => MonadLogger (ComposeT NoLoggingT t2 m)

deriving via
  WriterLoggingT ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
    Monad (t2 m) => MonadLogger (ComposeT WriterLoggingT t2 m)

instance (Monad (t m), MonadTrans t, MonadLoggerIO m) => MonadLoggerIO (Elevator t m) where
  askLoggerIO = lift askLoggerIO

deriving via
  Elevator t1 (t2 (m :: Type -> Type))
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadLoggerIO (t2 m)
    , MonadIO (ComposeT t1 t2 m)
    ) =>
    MonadLoggerIO (ComposeT t1 t2 m)

deriving via
  LoggingT (t2 (m :: Type -> Type))
  instance
    ( MonadIO (t2 m)
    , MonadIO (ComposeT LoggingT t2 m)
    ) =>
    MonadLoggerIO (ComposeT LoggingT t2 m)
