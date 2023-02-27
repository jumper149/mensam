module Homepage.Server.Route.Donate.Type where

import Data.Kind
import Servant.API
import Servant.API.Generic
import Servant.HTML.Blaze
import Text.Blaze.Html5

type Routes :: Type -> Type
data Routes route = Routes
  { routeDonate :: route :- Get '[HTML] Html
  , routeThankYou :: route :- "thankYou" :> Get '[HTML] Html
  }
  deriving stock (Generic)
