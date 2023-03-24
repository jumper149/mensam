module Mensam.Server.Server.Route.Api where

import Mensam.API.Route.Api
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Server.Route.Api.Booking qualified
import Mensam.Server.Server.Route.Api.OpenApi qualified
import Mensam.Server.Server.Route.Api.User qualified

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

handler ::
  (MonadConfigured m, MonadLogger m, MonadSecret m, MonadSeldaPool m, MonadUnliftIO m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeOpenApi = Mensam.Server.Server.Route.Api.OpenApi.handler
    , routeUser = Mensam.Server.Server.Route.Api.User.handler
    , routeBooking = Mensam.Server.Server.Route.Api.Booking.handler
    }
