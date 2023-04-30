module Mensam.Server.Secrets where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Crypto.JOSE.JWK qualified as JOSE
import Data.Kind
import Servant.Auth.Server
import System.Posix.Files

type Secrets :: Type
newtype Secrets = MkSecrets
  { secretsJwk :: JOSE.JWK
  }

initSecrets ::
  ( MonadConfigured m
  , MonadIO m
  , MonadLogger m
  ) =>
  m ()
initSecrets = do
  config <- configuration
  logDebug "Checking JWK."
  jwkExists <- liftIO $ fileExist $ authJwkFilepath $ configAuth config
  if jwkExists
    then logWarn "JWK already exists. Skipping."
    else do
      logDebug "Initializing JWK."
      liftIO $ writeKey $ authJwkFilepath $ configAuth config
      logDebug "Initialized JWK."
