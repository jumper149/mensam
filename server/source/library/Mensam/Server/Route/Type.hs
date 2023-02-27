module Mensam.Server.Route.Type where

import Mensam.Server.Route.Home.Type qualified
import Mensam.Server.Route.Static.Type qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeHome :: route :- Mensam.Server.Route.Home.Type.API
  , routeStatic :: route :- Mensam.Server.Route.Static.Type.API
  }
  deriving stock (Generic)
