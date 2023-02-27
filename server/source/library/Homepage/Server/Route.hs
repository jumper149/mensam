module Homepage.Server.Route where

import Homepage.Application.Blog.Class
import Homepage.Application.Configured.Class
import Homepage.Server.Route.Blog qualified
import Homepage.Server.Route.Donate qualified
import Homepage.Server.Route.Files qualified
import Homepage.Server.Route.Home qualified
import Homepage.Server.Route.Redirect qualified
import Homepage.Server.Route.Static qualified
import Homepage.Server.Route.Type

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadBlog m, MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeHome = Homepage.Server.Route.Home.handler
    , routeBlog = Homepage.Server.Route.Blog.routes
    , routeDonate = Homepage.Server.Route.Donate.routes
    , routeFiles = Homepage.Server.Route.Files.routes
    , routeRedirect = Homepage.Server.Route.Redirect.routes
    , routeStatic = Homepage.Server.Route.Static.handler
    }
