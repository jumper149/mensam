module Mensam.Server.Route where

import Mensam.Application.Configured.Class
import Mensam.Application.SeldaPool.Class
import Mensam.Server.Route.Home qualified
import Mensam.Server.Route.Static qualified
import Mensam.Server.Route.Type
import Mensam.Server.Route.User qualified

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadConfigured m, MonadLogger m, MonadSeldaPool m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeHome = Mensam.Server.Route.Home.handler
    , routeUser = Mensam.Server.Route.User.handler
    , routeStatic = Mensam.Server.Route.Static.handler
    }
