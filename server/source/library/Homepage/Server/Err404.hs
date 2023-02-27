module Homepage.Server.Err404 where

import Homepage.Application.Configured.Class
import Homepage.Configuration
import Homepage.Server.Html.Depth
import Homepage.Server.Html.Document

import Control.Monad.Logger.CallStack
import Data.ByteString.Builder
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Trans
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H

application404 ::
  (MonadConfigured m, MonadLogger m) =>
  ApplicationT m
application404 _req rsp = do
  html404' <- html404
  logInfo "Serve generic 404 page."
  rsp . responseBuilder status404 [(,) "Content-Type" "text/html"] $
    lazyByteString $
      renderHtml html404'

html404 ::
  MonadConfigured m =>
  m Html
html404 = do
  baseUrl <- configBaseUrl <$> configuration
  contactInformation <- configContactInformation <$> configuration
  revision <- configRevision <$> configuration
  let description = describeDocument contactInformation Nothing
  pure . document baseUrl contactInformation revision description $ do
    h1 "404"
    h2 "You got lost?"
    p $ "My homepage is " <> (a ! hrefWithDepth baseUrl Nothing "" $ "here") <> "."
