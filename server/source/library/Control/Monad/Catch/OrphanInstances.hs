{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Catch.OrphanInstances () where

import Control.Monad.Catch qualified as Exceptions
import Control.Monad.Catch.Pure qualified as Exceptions.T
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Data.Kind

instance (Monad (t m), Exceptions.MonadMask m, MonadTransControl t) => Exceptions.MonadMask (Elevator t m) where
  mask f = (restoreT . pure =<<) $ liftWith $ \runT ->
    Exceptions.mask $ \u -> runT $ f $ restoreT . u . runT
  uninterruptibleMask f = (restoreT . pure =<<) $ liftWith $ \runT ->
    Exceptions.uninterruptibleMask $ \u -> runT $ f $ restoreT . u . runT
  generalBracket acquire release use = do
    (usageResult', releaseResult') <- liftWith $ \runT -> do
      Exceptions.generalBracket
        (runT acquire)
        ( \resource' exitCase' -> runT $ do
            resource <- restoreT $ pure resource'
            case exitCase' of
              Exceptions.ExitCaseSuccess x' ->
                release resource . Exceptions.ExitCaseSuccess =<< restoreT (pure x')
              Exceptions.ExitCaseException e ->
                release resource (Exceptions.ExitCaseException e)
              Exceptions.ExitCaseAbort ->
                release resource Exceptions.ExitCaseAbort
        )
        (\resource -> runT $ use =<< restoreT (pure resource))
    usageResult <- restoreT $ pure usageResult'
    releaseResult <- restoreT $ pure releaseResult'
    pure (usageResult, releaseResult)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via
  Elevator t1 (t2 (m :: Type -> Type))
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , Exceptions.MonadMask (t2 m)
    , MonadTransControl t1
    ) =>
    Exceptions.MonadMask (ComposeT t1 t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via
  Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) =>
    Exceptions.MonadMask (ComposeT Exceptions.T.CatchT t2 m)
