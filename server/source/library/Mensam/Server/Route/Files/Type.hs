module Mensam.Server.Route.Files.Type where

import Data.Kind
import Servant.API
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM qualified as RawM
import Text.Blaze.Html5

type Routes :: Type -> Type
data Routes route = Routes
  { routeOverview :: route :- Get '[HTML] Html
  , routeFiles :: route :- RawM.RawM
  }
  deriving stock (Generic)
