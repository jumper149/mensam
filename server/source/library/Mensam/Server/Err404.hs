module Mensam.Server.Err404 where

import Mensam.Application.Configured.Class
import Mensam.Configuration
import Mensam.Configuration.BaseUrl

import Control.Monad.Logger.CallStack
import Data.ByteString.Builder
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Trans
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes as HA

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
  m H.Html
html404 = do
  baseUrl <- configBaseUrl <$> configuration
  maybeRevision <- configRevision <$> configuration
  pure $ H.docTypeHtml H.! HA.lang "en" $ do
    H.head $ do
      H.meta H.! HA.charset "UTF-8"
      H.meta H.! HA.name "description" H.! content "Mensam did not find, what you were looking for."
      H.meta H.! HA.name "viewport" H.! content "width=500"
      H.title "Mensam 404"
      H.link
        H.! HA.rel "icon"
        H.! HA.type_ "image/png"
        H.! HA.sizes "32x32"
        H.! HA.href (H.textValue $ displayBaseUrl baseUrl <> "/favicon.png")
      H.link
        H.! HA.rel "icon"
        H.! HA.type_ "image/png"
        H.! HA.sizes "192x192"
        H.! HA.href (H.textValue $ displayBaseUrl baseUrl <> "/favicon-192x192.png")
      H.link
        H.! HA.rel "icon"
        H.! HA.type_ "image/png"
        H.! HA.sizes "512x512"
        H.! HA.href (H.textValue $ displayBaseUrl baseUrl <> "/favicon-512x512.png")
      H.link
        H.! HA.rel "apple-touch-icon"
        H.! HA.type_ "image/png"
        H.! HA.sizes "512x512"
        H.! HA.href (H.textValue $ displayBaseUrl baseUrl <> "/favicon-512x512.png")
      H.link
        H.! HA.rel "stylesheet"
        H.! HA.type_ "text/css"
        H.! HA.href (H.textValue $ displayBaseUrl baseUrl <> "/stylesheet.css")
    H.body $ do
      H.h1 "404"
      H.h2 "You got lost?"
      H.p $ "Try starting " <> (H.a H.! HA.href (H.textValue (displayBaseUrl baseUrl)) $ "here") <> "."
      case maybeRevision of
        Nothing -> pure ()
        Just revision ->
          H.p $ do
            "This happened on revision "
            H.code $ H.text revision
            "."
