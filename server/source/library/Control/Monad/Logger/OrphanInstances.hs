{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Logger.OrphanInstances () where

import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

deriving via
  ReaderT (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  instance
    MonadTransControlIdentity LoggingT

deriving via
  IdentityT
  instance
    MonadTransControlIdentity NoLoggingT
