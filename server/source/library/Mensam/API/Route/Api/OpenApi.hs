module Mensam.API.Route.Api.OpenApi where

import Data.Kind
import Data.OpenApi
import GHC.Generics
import Servant.API

type Routes :: Type -> Type
newtype Routes route = Routes
  { routeJson ::
      route
        :- Summary "OpenAPI"
          :> Description
              "This OpenAPI specification is automatically generated from a servant API.\n"
          :> "openapi"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
