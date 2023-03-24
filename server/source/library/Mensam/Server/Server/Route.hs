module Mensam.Server.Server.Route where

import Mensam.API.Route
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Server.Route.Booking qualified
import Mensam.Server.Server.Route.OpenApi qualified
import Mensam.Server.Server.Route.Static qualified
import Mensam.Server.Server.Route.User qualified

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadConfigured m, MonadLogger m, MonadSecret m, MonadSeldaPool m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeOpenApi = Mensam.Server.Server.Route.OpenApi.handler
    , routeUser = Mensam.Server.Server.Route.User.handler
    , routeBooking = Mensam.Server.Server.Route.Booking.handler
    , routeStatic = Mensam.Server.Server.Route.Static.handler
    }
