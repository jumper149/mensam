module Mensam.Server.Route.Static where

import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Server.Err404
import Mensam.Server.FileServer
import Mensam.Server.Route.Static.Type

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
