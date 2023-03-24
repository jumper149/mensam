module Mensam.API.Route.Api.OpenApi where

import Data.Kind
import Data.OpenApi
import GHC.Generics
import Servant.API

type Routes :: Type -> Type
newtype Routes route = Routes
  { routeJson ::
      route
        :- Summary "OpenAPI definition"
          :> Description
              "This OpenAPI definition is automatically generated.\n"
          :> "openapi"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
