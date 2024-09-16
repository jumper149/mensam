module Mensam.Server.Server.Route where

import Mensam.API.Route
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Server.Route.Api qualified
import Mensam.Server.Server.Route.Frontend qualified
import Mensam.Server.Server.Route.Haddock qualified
import Mensam.Server.Server.Route.OpenApi qualified
import Mensam.Server.Server.Route.Static qualified

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadConfigured m, MonadEmail m, MonadLogger m, MonadSecret m, MonadSeldaPool m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeApi = Mensam.Server.Server.Route.Api.handler
    , routeOpenApi = Mensam.Server.Server.Route.OpenApi.handler
    , routeStatic = Mensam.Server.Server.Route.Static.handler
    , routeHaddock = Mensam.Server.Server.Route.Haddock.handler
    , routeFrontend = Mensam.Server.Server.Route.Frontend.handler
    }
