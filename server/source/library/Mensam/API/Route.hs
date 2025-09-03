module Mensam.API.Route where

import Mensam.API.Route.Api qualified
import Mensam.API.Route.Frontend qualified
import Mensam.API.Route.Haddock qualified
import Mensam.API.Route.OpenApi qualified
import Mensam.API.Route.Static qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
type role Routes _
data Routes route = Routes
  { routeApi :: route :- "api" :> NamedRoutes Mensam.API.Route.Api.Routes
  , routeOpenApi :: route :- "openapi" :> NamedRoutes Mensam.API.Route.OpenApi.Routes
  , routeStatic :: route :- "static" :> Mensam.API.Route.Static.API
  , routeHaddock :: route :- "haddock" :> Mensam.API.Route.Haddock.API
  , routeFrontend :: route :- Mensam.API.Route.Frontend.API
  }
  deriving stock (Generic)
