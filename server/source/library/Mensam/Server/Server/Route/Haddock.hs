module Mensam.Server.Server.Route.Haddock where

import Mensam.API.Route.Static
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Server.Err404
import Mensam.Server.Server.FileServer

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Network.Wai.Application.Static
import Network.Wai.Trans
import Servant
import Servant.RawM.Server qualified as RawM

handler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT API m
handler = do
  maybeDirectory <- configDirectoryHaddock <$> configuration
  fallbackApplication <- runApplicationT application404
  case maybeDirectory of
    Nothing -> do
      logWarn "No haddock files configured. Serving fallback application."
      pure fallbackApplication
    Just directory -> do
      logInfo "Serve haddock file download."
      settings <- fileServerSettings directory
      RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
