module Homepage.Server.Route.Type where

import Homepage.Server.Route.Blog.Type qualified
import Homepage.Server.Route.Donate.Type qualified
import Homepage.Server.Route.Files.Type qualified
import Homepage.Server.Route.Home.Type qualified
import Homepage.Server.Route.Redirect.Type qualified
import Homepage.Server.Route.Static.Type qualified

import Data.Kind
import Servant
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeHome :: route :- Homepage.Server.Route.Home.Type.API
  , routeBlog :: route :- "blog" :> NamedRoutes Homepage.Server.Route.Blog.Type.Routes
  , routeDonate :: route :- "donate" :> NamedRoutes Homepage.Server.Route.Donate.Type.Routes
  , routeFiles :: route :- "files" :> NamedRoutes Homepage.Server.Route.Files.Type.Routes
  , routeRedirect :: route :- NamedRoutes Homepage.Server.Route.Redirect.Type.Routes
  , routeStatic :: route :- Homepage.Server.Route.Static.Type.API
  }
  deriving stock (Generic)
