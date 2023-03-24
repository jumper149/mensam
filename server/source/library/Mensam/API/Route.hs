module Mensam.API.Route where

import Mensam.API.Route.Api qualified
import Mensam.API.Route.Api.OpenApi qualified
import Mensam.API.Route.Static qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeApi :: route :- NamedRoutes Mensam.API.Route.Api.Routes
  , routeOpenApi :: route :- NamedRoutes Mensam.API.Route.Api.OpenApi.Routes
  , routeStatic :: route :- Mensam.API.Route.Static.API
  }
  deriving stock (Generic)
