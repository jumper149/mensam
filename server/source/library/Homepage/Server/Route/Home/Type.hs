module Homepage.Server.Route.Home.Type where

import Data.Kind
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html5

type API :: Type
type API = Get '[HTML] Html
