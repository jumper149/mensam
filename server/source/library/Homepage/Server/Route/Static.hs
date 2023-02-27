module Homepage.Server.Route.Static where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Err404
import Homepage.Server.FileServer
import Homepage.Server.Route.Static.Type

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Network.Wai.Trans
import Servant
import Servant.RawM.Server qualified as RawM
import WaiAppStatic.Types

handler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT API m
handler = do
  directory <- configDirectoryStatic <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve static file download."
  settings <- fileServerSettings directory
  RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
