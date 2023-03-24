module Mensam.Server.Server.Route.Api.OpenApi where

import Mensam.API.Route.Api.OpenApi
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.OpenApi qualified

import Control.Lens
import Data.OpenApi
import Servant.Server.Generic

handler ::
  MonadConfigured m =>
  Routes (AsServerT m)
handler =
  Routes
    { routeJson = specification
    }

specification :: MonadConfigured m => m OpenApi
specification = do
  config <- configuration
  let
    addVersion :: OpenApi -> OpenApi
    addVersion =
      case configRevision config of
        Just revision -> info . version .~ revision
        Nothing -> id
    addServer :: OpenApi -> OpenApi
    addServer = servers .~ [relativeServer]
     where
      relativeServer =
        Server
          { _serverUrl = ""
          , _serverDescription = Nothing
          , _serverVariables = mempty
          }
  pure $
    Mensam.Server.OpenApi.openapi
      & addVersion
      & addServer
