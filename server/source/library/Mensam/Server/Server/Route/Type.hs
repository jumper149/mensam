module Mensam.Server.Server.Route.Type where

import Mensam.Server.Server.Route.Booking.Type qualified
import Mensam.Server.Server.Route.OpenApi.Type qualified
import Mensam.Server.Server.Route.Static.Type qualified
import Mensam.Server.Server.Route.User.Type qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeOpenApi :: route :- NamedRoutes Mensam.Server.Server.Route.OpenApi.Type.Routes
  , routeUser :: route :- NamedRoutes Mensam.Server.Server.Route.User.Type.Routes
  , routeBooking :: route :- NamedRoutes Mensam.Server.Server.Route.Booking.Type.Routes
  , routeStatic :: route :- Mensam.Server.Server.Route.Static.Type.API
  }
  deriving stock (Generic)
