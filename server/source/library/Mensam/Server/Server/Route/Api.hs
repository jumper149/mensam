module Mensam.Server.Server.Route.Api where

import Mensam.API.Route.Api
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Server.Route.Api.OpenApi qualified
import Mensam.Server.Server.Route.Api.Reservation qualified
import Mensam.Server.Server.Route.Api.Space qualified
import Mensam.Server.Server.Route.Api.User qualified

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

handler ::
  (MonadConfigured m, MonadEmail m, MonadLogger m, MonadSecret m, MonadSeldaPool m, MonadUnliftIO m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeOpenApi = Mensam.Server.Server.Route.Api.OpenApi.handler
    , routeUser = Mensam.Server.Server.Route.Api.User.handler
    , routeSpace = Mensam.Server.Server.Route.Api.Space.handler
    , routeReservation = Mensam.Server.Server.Route.Api.Reservation.handler
    }
