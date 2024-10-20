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
  | EnvVarLogColor
  | EnvVarLogFile
  | EnvVarLogLevel
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)

genSingletons [''EnvVar]

type EnvVarName :: EnvVar -> Symbol
type family EnvVarName envVar = name | name -> envVar where
  EnvVarName EnvVarConfigFile = "MENSAM_CONFIG_FILE"
  EnvVarName EnvVarLogColor = "MENSAM_LOG_COLOR"
  EnvVarName EnvVarLogFile = "MENSAM_LOG_FILE"
  EnvVarName EnvVarLogLevel = "MENSAM_LOG_LEVEL"

type EnvVarValue :: EnvVar -> Type
type family EnvVarValue envVar = value where
  EnvVarValue EnvVarConfigFile = FilePath
  EnvVarValue EnvVarLogFile = Maybe FilePath
  EnvVarValue EnvVarLogLevel = LogLevel
  EnvVarValue EnvVarLogColor = Bool

envVarParse :: SEnvVar envVar -> String -> Maybe (EnvVarValue envVar)
envVarParse = \case
  SEnvVarConfigFile -> Just
  SEnvVarLogColor -> readMaybe
  SEnvVarLogFile -> Just . Just
  SEnvVarLogLevel -> readMaybe

envVarDefault :: SEnvVar envVar -> EnvVarValue envVar
envVarDefault = \case
  SEnvVarConfigFile -> "./mensam.json"
  SEnvVarLogColor -> True
  SEnvVarLogFile -> Nothing
  SEnvVarLogLevel -> LevelDebug

envVarHelp :: [String]
envVarHelp = singleEnvVarHelp <$> [minBound .. maxBound]
 where
  singleEnvVarHelp :: EnvVar -> String
  singleEnvVarHelp = \case
    EnvVarConfigFile -> "MENSAM_CONFIG_FILE=[FILEPATH] (filepath to a JSON configuration file)"
    EnvVarLogColor -> "MENSAM_LOG_COLOR=[True|False] (toggle the color of the log)"
    EnvVarLogFile -> "MENSAM_LOG_FILE=[FILEPATH] (filepath where the log will get dumped)"
    EnvVarLogLevel -> "MENSAM_LOG_LEVEL=[LevelDebug|LevelInfo|LevelWarn|LevelError] (set log verbosity)"

type Environment :: Type
newtype Environment = MkEnvironment {getEnvironment :: forall envVar. SEnvVar envVar -> EnvVarValue envVar}
