module Mensam.Server.Route.Blog where

import Mensam.Application.Blog.Class
import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.Blog
import Mensam.Server.Err404
import Mensam.Server.FileServer
import Mensam.Server.Html.Blog
import Mensam.Server.Html.Depth
import Mensam.Server.Html.Document
import Mensam.Server.Route.Blog.Atom qualified as Atom
import Mensam.Server.Route.Blog.Type
import Mensam.Server.Route.Type qualified
import Mensam.Server.Tab

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Network.Wai.Trans
import Servant
import Servant.RawM.Server qualified as RawM
import Servant.Server.Generic
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes qualified as HA
import Text.Blaze.Html5.Extra
import WaiAppStatic.Types

routes ::
  (MonadBlog m, MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  Routes (AsServerT m)
routes =
  Routes
    { routeRaw = rawHandler
    , routeFeed = Atom.handler
    , routeArticle = articleHandler
    , routeOverview = overviewHandler
    }

overviewHandler ::
  (MonadConfigured m, MonadLogger m) =>
  m Html
overviewHandler = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  blogs <- configBlogEntries <$> configuration
  logInfo "Serve blog overview."
  let description = (describeDocument contactInformation $ Just TabBlog) {documentDepth = Just 0}
  pure . document baseUrl contactInformation revision description $ do
    h2 "my Blog"
    p $ do
      "My blog is available as an "
      a ! hrefWithDepth baseUrl (Just 0) (stringValue $ show $ linkURI blogFeedLink) $ s "RSS" <> "/Atom Feed"
      "."
    blogList baseUrl (Just 0) blogs
 where
  blogFeedLink = routeFeed . Mensam.Server.Route.Type.routeBlog $ allFieldLinks

articleHandler ::
  (MonadConfigured m, MonadLogger m) =>
  BlogId ->
  m (Union [WithStatus 200 Html, WithStatus 404 Html])
articleHandler blogId = do
  blogs <- configBlogEntries <$> configuration
  case lookupBlog blogId blogs of
    Nothing -> do
      logInfo $ "Failed to serve blog article: " <> T.pack (show blogId)
      respond . WithStatus @404 =<< html404
    Just blog -> do
      baseUrl <- configBaseUrl <$> configuration
      contactInformation <- configContactInformation <$> configuration
      revision <- configRevision <$> configuration
      logInfo $ "Serve blog article: " <> T.pack (show blogId)
      let shortenText len txt = if T.length txt <= len + 3 then txt else T.take len txt <> "..."
      let description =
            let x = (describeDocument contactInformation $ Just TabBlog) {documentDepth = Just 1}
             in x {documentTitle = ((shortenText 29 (blogTitle blog) <> " - ") <>) <$> documentTitle x}
      respond . WithStatus @200 $
        document baseUrl contactInformation revision description $ do
          h2 $ text $ blogTitle blog
          p $ do
            "View blog entry: "
            a ! hrefWithDepth baseUrl (Just 1) (textValue $ T.pack (show $ linkURI blogRawLink) <> "/" <> unBlogId blogId <> ".html") $
              "HTML"
            " | "
            a
              ! hrefWithDepth baseUrl (Just 1) (textValue $ T.pack (show $ linkURI blogRawLink) <> "/" <> unBlogId blogId <> ".pdf")
              $ "PDF"
          hr
          script ! HA.type_ "text/javascript" $
            "function resizeIframe(iframe) {\
            \  iframe.height = `${iframe.contentWindow.document.body.scrollHeight + 30}` + \"px\";\
            \}"
          iframe
            ! HA.src (withDepth baseUrl (Just 1) (textValue $ T.pack (show $ linkURI blogRawLink) <> "/" <> unBlogId blogId <> ".html"))
            ! HA.name "blog article (HTML)"
            ! HA.width "100%"
            ! HA.onload "resizeIframe(this)"
            ! HA.target "_parent"
            ! HA.style "border: none;"
            $ mempty
          case blogDiscussion blog of
            [] -> mempty
            links -> do
              hr
              p $ do
                "Follow the discussion about this article:"
                ul . toMarkup $ markupLink <$> links
 where
  blogRawLink = routeRaw . Mensam.Server.Route.Type.routeBlog $ allFieldLinks
  markupLink BlogLink {blogLinkDescription, blogLinkUrl} =
    li $ a ! HA.href (toValue blogLinkUrl) $ text blogLinkDescription

rawHandler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT RawM.RawM m
rawHandler = do
  directory <- configDirectoryBlog <$> configuration
  fallbackApplication <- runApplicationT application404
  logInfo "Serve blog download."
  settings <- fileServerSettings directory
  RawM.serveDirectoryWith settings {ss404Handler = Just fallbackApplication}
