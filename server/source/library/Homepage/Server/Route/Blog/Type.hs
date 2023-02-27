module Homepage.Server.Route.Blog.Type where

import Homepage.Configuration.Blog
import Homepage.Server.Route.Blog.Atom.Type qualified as Atom

import Data.Kind
import Servant.API
import Servant.API.Generic
import Servant.HTML.Blaze
import Servant.RawM qualified as RawM
import Text.Blaze.Html5

type Routes :: Type -> Type
data Routes route = Routes
  { routeRaw :: route :- "raw" :> RawM.RawM
  , routeFeed :: route :- "atom.xml" :> Atom.API
  , routeArticle ::
      route
        :- Capture "article" BlogId
          :> UVerb GET '[HTML] [WithStatus 200 Html, WithStatus 404 Html]
  , routeOverview :: route :- Get '[HTML] Html
  }
  deriving stock (Generic)
