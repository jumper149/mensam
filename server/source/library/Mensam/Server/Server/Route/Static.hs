module Mensam.Server.Server.Route.Static where

import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Server.Err404
import Mensam.Server.Server.FileServer
import Mensam.Server.Server.Route.Static.Type

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
