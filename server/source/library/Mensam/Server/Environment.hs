{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Mensam.Server.Environment where

import Control.Monad.Logger.CallStack
import Data.Kind
import Data.Singletons.TH
import GHC.Generics
import GHC.TypeLits
import Text.Read

type EnvVar :: Type
data EnvVar
  = EnvVarConfigFile
  | EnvVarLogFile
  | EnvVarLogLevel
  | EnvVarLogColor
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

genSingletons [''EnvVar]

type EnvVarName :: EnvVar -> Symbol
type family EnvVarName envVar = name | name -> envVar where
  EnvVarName EnvVarConfigFile = "MENSAM_CONFIG_FILE"
  EnvVarName EnvVarLogFile = "MENSAM_LOG_FILE"
  EnvVarName EnvVarLogLevel = "MENSAM_LOG_LEVEL"
  EnvVarName EnvVarLogColor = "MENSAM_LOG_COLOR"

type EnvVarValue :: EnvVar -> Type
type family EnvVarValue envVar = value where
  EnvVarValue EnvVarConfigFile = FilePath
  EnvVarValue EnvVarLogFile = Maybe FilePath
  EnvVarValue EnvVarLogLevel = LogLevel
  EnvVarValue EnvVarLogColor = Bool

envVarParse :: SEnvVar envVar -> String -> Maybe (EnvVarValue envVar)
envVarParse = \case
  SEnvVarConfigFile -> Just
  SEnvVarLogFile -> Just . Just
  SEnvVarLogLevel -> readMaybe
  SEnvVarLogColor -> readMaybe

envVarDefault :: SEnvVar envVar -> EnvVarValue envVar
envVarDefault = \case
  SEnvVarConfigFile -> "./mensam.json"
  SEnvVarLogFile -> Nothing
  SEnvVarLogLevel -> LevelDebug
  SEnvVarLogColor -> False

type Environment :: Type
newtype Environment = MkEnvironment {getEnvironment :: forall envVar. SEnvVar envVar -> EnvVarValue envVar}
