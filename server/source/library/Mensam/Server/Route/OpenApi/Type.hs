module Mensam.Server.Route.OpenApi.Type where

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
        :- "openapi"
          :> Get '[HTML] Html
  , routeJson ::
      route
        :- "openapi"
          :> "json"
          :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)
