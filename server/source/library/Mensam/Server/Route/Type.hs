module Mensam.Server.Route.Type where

import Mensam.Server.Route.Blog.Type qualified
import Mensam.Server.Route.Donate.Type qualified
import Mensam.Server.Route.Files.Type qualified
import Mensam.Server.Route.Home.Type qualified
import Mensam.Server.Route.Redirect.Type qualified
import Mensam.Server.Route.Static.Type qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeHome :: route :- Mensam.Server.Route.Home.Type.API
  , routeBlog :: route :- "blog" :> NamedRoutes Mensam.Server.Route.Blog.Type.Routes
  , routeDonate :: route :- "donate" :> NamedRoutes Mensam.Server.Route.Donate.Type.Routes
  , routeFiles :: route :- "files" :> NamedRoutes Mensam.Server.Route.Files.Type.Routes
  , routeRedirect :: route :- NamedRoutes Mensam.Server.Route.Redirect.Type.Routes
  , routeStatic :: route :- Mensam.Server.Route.Static.Type.API
  }
  deriving stock (Generic)
