{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Mensam.Environment where

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
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

genSingletons [''EnvVar]

type EnvVarName :: EnvVar -> Symbol
type family EnvVarName envVar = name | name -> envVar where
  EnvVarName EnvVarConfigFile = "HOMEPAGE_CONFIG_FILE"
  EnvVarName EnvVarLogFile = "HOMEPAGE_LOG_FILE"
  EnvVarName EnvVarLogLevel = "HOMEPAGE_LOG_LEVEL"

type EnvVarValue :: EnvVar -> Type
type family EnvVarValue envVar = value where
  EnvVarValue EnvVarConfigFile = FilePath
  EnvVarValue EnvVarLogFile = Maybe FilePath
  EnvVarValue EnvVarLogLevel = LogLevel

envVarParse :: SEnvVar envVar -> String -> Maybe (EnvVarValue envVar)
envVarParse = \case
  SEnvVarConfigFile -> Just
  SEnvVarLogFile -> Just . Just
  SEnvVarLogLevel -> readMaybe

envVarDefault :: SEnvVar envVar -> EnvVarValue envVar
envVarDefault = \case
  SEnvVarConfigFile -> "./homepage.json"
  SEnvVarLogFile -> Nothing
  SEnvVarLogLevel -> LevelDebug

type Environment :: Type
newtype Environment = MkEnvironment {getEnvironment :: forall envVar. SEnvVar envVar -> EnvVarValue envVar}
