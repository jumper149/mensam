module Mensam.API.Route where

import Mensam.API.Route.Booking qualified
import Mensam.API.Route.OpenApi qualified
import Mensam.API.Route.Static qualified
import Mensam.API.Route.User qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeOpenApi :: route :- NamedRoutes Mensam.API.Route.OpenApi.Routes
  , routeUser :: route :- NamedRoutes Mensam.API.Route.User.Routes
  , routeBooking :: route :- NamedRoutes Mensam.API.Route.Booking.Routes
  , routeStatic :: route :- Mensam.API.Route.Static.API
  }
  deriving stock (Generic)
