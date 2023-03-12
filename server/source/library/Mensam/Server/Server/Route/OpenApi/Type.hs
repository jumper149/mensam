module Mensam.Server.Server.Route.OpenApi.Type where

import Data.Kind
import Data.OpenApi
import GHC.Generics
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html

type Routes :: Type -> Type
data Routes route = Routes
  { routeRender ::
      route
        :- Summary "View API documentation"
          :> Description
              "View the OpenAPI documentation in a human-readabable format."
          :> "openapi"
          :> Get '[HTML] Html
  , routeJson ::
      route
        :- Summary "OpenAPI definition"
          :> Description
              "This OpenAPI definition is automatically generated."
          :> "openapi"
          :> "json"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
