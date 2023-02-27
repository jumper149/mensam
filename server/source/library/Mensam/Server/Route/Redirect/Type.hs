module Mensam.Server.Route.Redirect.Type where

import Data.Kind
import Servant.API
import Servant.API.Generic

type Routes :: Type -> Type
data Routes route = Routes
  { routeFeed :: route :- "feed" :> Found302
  , routeFeedXml :: route :- "feed.xml" :> Found302
  , routeRss :: route :- "rss" :> Found302
  , routeRssXml :: route :- "rss.xml" :> Found302
  , routeAtom :: route :- "atom" :> Found302
  , routeAtomXml :: route :- "atom.xml" :> Found302
  , routeBlogFeed :: route :- "blog" :> "feed" :> Found302
  , routeBlogFeedXml :: route :- "blog" :> "feed.xml" :> Found302
  , routeBlogRss :: route :- "blog" :> "rss" :> Found302
  , routeBlogRssXml :: route :- "blog" :> "rss.xml" :> Found302
  , routeBlogAtom :: route :- "blog" :> "atom" :> Found302
  }
  deriving stock (Generic)

type Found302Content :: Type
type Found302Content = Headers '[Header "Location" String] NoContent
type Found302 :: Type
type Found302 = Verb GET 302 '[PlainText] Found302Content
