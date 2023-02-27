module Homepage.Application.Environment.Class where

import Homepage.Environment

import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Elevator
import Data.Kind
import Data.Singletons
import GHC.TypeLits

type MonadEnvironment :: (Type -> Type) -> Constraint
class Monad m => MonadEnvironment m where
  environmentVariable ::
    forall envVar.
    SingI envVar =>
    ProxyEnvVarName (EnvVarName envVar) ->
    m (EnvVarValue envVar)

instance
  ( Monad (t m)
  , MonadTrans t
  , MonadEnvironment m
  ) =>
  MonadEnvironment (Elevator t m)
  where
  environmentVariable = lift . environmentVariable

deriving via
  Elevator t1 ((t2 :: (Type -> Type) -> Type -> Type) m)
  instance
  {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadEnvironment (t2 m)
    ) =>
    MonadEnvironment (ComposeT t1 t2 m)

type ProxyEnvVarName :: Symbol -> Type
data ProxyEnvVarName name = EnvVar
