module Mensam.Server.Server.Route.Api.OpenApi where

import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.OpenApi
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.OpenApi qualified

import Control.Lens
import Data.List qualified as L
import Data.OpenApi
import Data.Text qualified as T
import Servant.Links
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
          { _serverUrl = T.pack relativePath
          , _serverDescription = Nothing
          , _serverVariables = mempty
          }
      linkCurrent = routeJson . Route.Api.routeOpenApi $ allFieldLinks
      relativePath = relativePathGoBack linkCurrent
    addDescription :: OpenApi -> OpenApi
    addDescription =
      info . description
        ?~ T.concat
          [ "This is the API for Mensam Desk-Booking.\n\
            \\n\
            \- [User Interface](..)\n"
          , case configSourceUrl config of
              Nothing -> ""
              Just sourceUrl -> "- [GitHub](" <> sourceUrl <> ")\n"
          , "- [OpenAPI]()\n\
            \- [Haddock (server source)](./haddock/index.html)\n\
            \\n"
          ]
  pure $
    Mensam.Server.OpenApi.openapi
      & info . title .~ "Mensam API"
      & addVersion
      & info . license ?~ "GNU Affero General Public License v3.0"
      & addDescription
      & addServer

-- | Assuming that the origin link doesn't have a trailing slash.
relativePathGoBack :: Servant.Links.Link -> String
relativePathGoBack origin =
  case linkSegments origin of
    [] -> ""
    [_] -> ""
    _ : baseSegments -> L.intercalate "/" (".." <$ baseSegments)
