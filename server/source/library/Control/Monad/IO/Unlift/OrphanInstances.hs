{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.IO.Unlift.OrphanInstances () where

import Control.Monad.IO.Unlift
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator

instance
  ( Monad (t m)
  , MonadTransControlIdentity t
  , MonadUnliftIO m
  ) =>
  MonadUnliftIO (Elevator t m)
  where
  withRunInIO f = liftWithIdentity $ \runT -> withRunInIO $ \runInIO -> f $ runInIO . runT

deriving via
  Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControlIdentity (ComposeT t1 t2)
    , MonadUnliftIO m
    ) =>
    MonadUnliftIO (ComposeT t1 t2 m)
