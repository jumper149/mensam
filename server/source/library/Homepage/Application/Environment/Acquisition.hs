{-# LANGUAGE TypeFamilies #-}

module Homepage.Application.Environment.Acquisition where

import Homepage.Environment

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Logger.OrphanInstances ()
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.State
import Data.List qualified as L
import Data.Singletons
import Data.Text qualified as T
import GHC.TypeLits
import System.Posix.Env qualified as System

acquireEnvironment ::
  (MonadIO m, MonadLogger m) =>
  m Environment
acquireEnvironment = do
  logInfo "Looking up environment variables."
  env <- liftIO System.getEnvironment
  logDebug $ "Looked up environment variables: " <> T.pack (show env)

  (environment, unconsumedEnv) <- flip runStateT env . descend $ do
    configFile <- lookupEnvironmentVariable SEnvVarConfigFile
    logFile <- lookupEnvironmentVariable SEnvVarLogFile
    logLevel <- lookupEnvironmentVariable SEnvVarLogLevel

    let
      environment :: SEnvVar envVar -> Const (EnvVarValue envVar) envVar
      environment = \case
        SEnvVarConfigFile -> configFile
        SEnvVarLogFile -> logFile
        SEnvVarLogLevel -> logLevel

    pure $ MkEnvironment $ getConst . environment

  checkUnconsumedEnvironment unconsumedEnv
  pure environment

lookupEnvironmentVariable ::
  forall envVar m.
  (KnownSymbol (EnvVarName envVar), MonadLogger m, Show (EnvVarValue envVar)) =>
  SEnvVar envVar ->
  Elevator (StateT [(String, String)]) m (Const (EnvVarValue envVar) envVar)
lookupEnvironmentVariable singEnvVar = do
  logInfo $ "Inspecting environment variable: " <> T.pack (show envVarName)
  env <- Ascend get
  case lookup envVarName env of
    Nothing -> do
      logInfo $ "Environment variable '" <> T.pack (show envVarName) <> "' is not set."
      logInfo $ "Using default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefaultValue)
      pure $ Const envVarDefaultValue
    Just str -> do
      logInfo $ "Spotted environment variable: " <> T.pack (show envVarName)
      Ascend $ modify $ L.deleteBy (\x y -> fst x == fst y) (envVarName, undefined)
      logDebug $ "Parsing environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show str)
      case envVarParse singEnvVar str of
        Nothing -> do
          logError $ "Failed to parse environment variable: " <> T.pack (show envVarName)
          logWarn $ "Falling back to default value for environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show envVarDefaultValue)
          pure $ Const envVarDefaultValue
        Just val -> do
          logInfo $ "Parsed environment variable '" <> T.pack (show envVarName) <> "': " <> T.pack (show val)
          pure $ Const val
 where
  envVarName = symbolVal $ Proxy @(EnvVarName envVar)
  envVarDefaultValue = envVarDefault singEnvVar

checkUnconsumedEnvironment ::
  MonadLogger m =>
  [(String, String)] ->
  m ()
checkUnconsumedEnvironment env = do
  logDebug $ "Checking unconsumed environment for left-over environment variables: " <> T.pack (show env)
  case filter isSuspicious env of
    [] -> logInfo "Unconsumed environment doesn't contain any anomalies."
    anomalies -> logWarn $ "Unconsumed environment contains anomalies: " <> T.pack (show anomalies)
 where
  isSuspicious :: (String, String) -> Bool
  isSuspicious (identifier, _value) = "HOMEPAGE" `L.isPrefixOf` identifier
