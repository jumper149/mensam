module Mensam.Server.Route where

import Mensam.Application.Blog.Class
import Mensam.Application.Configured.Class
import Mensam.Server.Route.Blog qualified
import Mensam.Server.Route.Donate qualified
import Mensam.Server.Route.Files qualified
import Mensam.Server.Route.Home qualified
import Mensam.Server.Route.Redirect qualified
import Mensam.Server.Route.Static qualified
import Mensam.Server.Route.Type

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadBlog m, MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeHome = Mensam.Server.Route.Home.handler
    , routeBlog = Mensam.Server.Route.Blog.routes
    , routeDonate = Mensam.Server.Route.Donate.routes
    , routeFiles = Mensam.Server.Route.Files.routes
    , routeRedirect = Mensam.Server.Route.Redirect.routes
    , routeStatic = Mensam.Server.Route.Static.handler
    }
