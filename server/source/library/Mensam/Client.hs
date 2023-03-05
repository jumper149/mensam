module Mensam.Client where

import Mensam.Client.OrphanInstances ()
import Mensam.Server.Route.Type qualified as Route

import Data.Proxy
import Servant
import Servant.Client
import Servant.RawM.Client ()

routes :: Route.Routes (AsClientT ClientM)
routes = client $ Proxy @(NamedRoutes Route.Routes)
