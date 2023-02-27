module Homepage.Server.Route.Redirect where

import Homepage.Server.Route.Blog.Type qualified
import Homepage.Server.Route.Redirect.Type
import Homepage.Server.Route.Type qualified

import Servant
import Servant.Server.Generic

routes :: Applicative m => Routes (AsServerT m)
routes =
  Routes
    { routeFeed = pure $ headers blogFeedLink
    , routeFeedXml = pure $ headers blogFeedLink
    , routeRss = pure $ headers blogFeedLink
    , routeRssXml = pure $ headers blogFeedLink
    , routeAtom = pure $ headers blogFeedLink
    , routeAtomXml = pure $ headers blogFeedLink
    , routeBlogFeed = pure $ headers feedLink
    , routeBlogFeedXml = pure $ headers feedLink
    , routeBlogRss = pure $ headers feedLink
    , routeBlogRssXml = pure $ headers feedLink
    , routeBlogAtom = pure $ headers feedLink
    }
 where
  headers :: Link -> Found302Content
  headers link =
    Headers
      { getResponse = NoContent
      , getHeadersHList = HCons (Header $ show $ linkURI link) HNil
      }
  blogFeedLink = Homepage.Server.Route.Blog.Type.routeFeed . Homepage.Server.Route.Type.routeBlog $ allFieldLinks
  feedLink = Homepage.Server.Route.Blog.Type.routeFeed allFieldLinks
