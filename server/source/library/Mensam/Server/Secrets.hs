module Mensam.Server.Secrets where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration

import Control.Monad.IO.Class
import Crypto.JOSE.JWK qualified as JOSE
import Data.Kind
import Servant.Auth.Server

type Secrets :: Type
newtype Secrets = MkSecrets
  { secretsJwk :: JOSE.JWK
  }

initSecrets ::
  ( MonadConfigured m
  , MonadIO m
  ) =>
  m ()
initSecrets = do
  config <- configuration
  liftIO $ writeKey $ authJwkFilepath $ configAuth config
