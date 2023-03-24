module Mensam.API.Route where

import Mensam.API.Route.Api qualified
import Mensam.API.Route.OpenApi qualified
import Mensam.API.Route.Static qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeApi :: route :- "api" :> NamedRoutes Mensam.API.Route.Api.Routes
  , routeOpenApi :: route :- "openapi" :> NamedRoutes Mensam.API.Route.OpenApi.Routes
  , routeStatic :: route :- Mensam.API.Route.Static.API
  }
  deriving stock (Generic)
