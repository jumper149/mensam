module Mensam.Server.Server.Route.Frontend where

import Mensam.API.Route.Frontend
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration

import Control.Monad.IO.Unlift
import Control.Monad.Logger.CallStack
import Servant
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as H.A
import Mensam.Server.Configuration.BaseUrl
import Numeric.Natural

handler ::
  (MonadConfigured m, MonadLogger m, MonadUnliftIO m) =>
  ServerT API m
handler _ = do
    baseUrl <- configBaseUrl <$> configuration
    let depth = Just 1
    pure $
        docTypeHtml $ do
            H.head $ do
                meta ! charset "UTF-8"
                H.title "Mensam"
                link
                  ! rel "icon"
                  ! type_ "image/png"
                  ! sizes "32x32"
                  ! hrefWithDepth baseUrl depth "static/favicon.png"
                link
                  ! rel "icon"
                  ! type_ "image/png"
                  ! sizes "192x192"
                  ! hrefWithDepth baseUrl depth "static/favicon-192x192.png"
                link
                  ! rel "icon"
                  ! type_ "image/png"
                  ! sizes "512x512"
                  ! hrefWithDepth baseUrl depth "static/favicon-512x512.png"
                link
                  ! rel "apple-touch-icon"
                  ! type_ "image/png"
                  ! sizes "512x512"
                  ! hrefWithDepth baseUrl depth "static/favicon-512x512.png"
                link
                  ! rel "stylesheet"
                  ! type_ "text/css"
                  ! hrefWithDepth baseUrl depth "static/stylesheet.css"
                script ! src (withDepth baseUrl depth "static/spa.js") $ ""
            body $ do
                H.div ! H.A.id "myapp" $ ""
                script
                    "var app = Elm.Main.init({\
                    \  node: document.getElementById('myapp')\
                    \});"

hrefWithDepth ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  AttributeValue ->
  Attribute
hrefWithDepth baseUrl depth ref = href $ withDepth baseUrl depth ref

withDepth ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  AttributeValue ->
  AttributeValue
withDepth baseUrl Nothing ref = textValue (displayBaseUrl baseUrl) <> ref
withDepth _ (Just 0) ref = "./" <> ref
withDepth _ (Just 1) ref = "../" <> ref
withDepth baseUrl (Just n) ref = withDepth baseUrl (Just $ pred n) $ "../" <> ref
