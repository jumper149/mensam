module Mensam.Server.Route where

import Mensam.Application.Configured.Class
import Mensam.Server.Route.Home qualified
import Mensam.Server.Route.Static qualified
import Mensam.Server.Route.Type

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant.Server.Generic

routes ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeHome = Mensam.Server.Route.Home.handler
    , routeStatic = Mensam.Server.Route.Static.handler
    }
