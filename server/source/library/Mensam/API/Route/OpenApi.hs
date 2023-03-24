module Mensam.API.Route.OpenApi where

import Data.Kind
import GHC.Generics
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html

type Routes :: Type -> Type
newtype Routes route = Routes
  { routeRender ::
      route
        :- Summary "View API documentation"
          :> Description
              "View the OpenAPI documentation in a human-readabable format.\n"
          :> "openapi"
          :> Get '[HTML] Html
  }
  deriving stock (Generic)
