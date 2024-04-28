module Mensam.API.Route.Api where

import Mensam.API.Route.Api.OpenApi qualified
import Mensam.API.Route.Api.Reservation qualified
import Mensam.API.Route.Api.Space qualified
import Mensam.API.Route.Api.User qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeOpenApi :: route :- NamedRoutes Mensam.API.Route.Api.OpenApi.Routes
  , routeUser :: route :- NamedRoutes Mensam.API.Route.Api.User.Routes
  , routeSpace :: route :- NamedRoutes Mensam.API.Route.Api.Space.Routes
  , routeReservation :: route :- "reservation" :> NamedRoutes Mensam.API.Route.Api.Reservation.Routes
  }
  deriving stock (Generic)
