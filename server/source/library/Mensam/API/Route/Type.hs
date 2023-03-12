module Mensam.API.Route.Type where

import Mensam.API.Route.Booking.Type qualified
import Mensam.API.Route.OpenApi.Type qualified
import Mensam.API.Route.Static.Type qualified
import Mensam.API.Route.User.Type qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeOpenApi :: route :- NamedRoutes Mensam.API.Route.OpenApi.Type.Routes
  , routeUser :: route :- NamedRoutes Mensam.API.Route.User.Type.Routes
  , routeBooking :: route :- NamedRoutes Mensam.API.Route.Booking.Type.Routes
  , routeStatic :: route :- Mensam.API.Route.Static.Type.API
  }
  deriving stock (Generic)
