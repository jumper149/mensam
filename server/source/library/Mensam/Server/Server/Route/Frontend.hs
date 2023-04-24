module Mensam.Server.Server.Route.Frontend where

import Mensam.API.Route.Frontend
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.BaseUrl

import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Numeric.Natural
import Servant
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as H.A

handler ::
  (MonadConfigured m, MonadLogger m) =>
  ServerT API m
handler segments = do
  baseUrl <- configBaseUrl <$> configuration
  let depth =
        case segments of
          [] -> Just 0
          _ -> Just $ toEnum $ length segments - 1
  logInfo "Serve frontend."
  logDebug $ "Frontend will be using path: " <> T.pack (show segments)
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
        script ! src (withDepth baseUrl depth "static/spa.js") $ ""
        link
          ! rel "stylesheet"
          ! type_ "text/css"
          ! hrefWithDepth baseUrl depth "static/fonts.css"
      body $ do
        H.div ! H.A.id "myapp" $ ""
        script
          "var storedData = localStorage.getItem('myapp-model');\n\
          \var flags = storedData ? JSON.parse(storedData) : null;\n\
          \\n\
          \var app = Elm.Main.init({\n\
          \  node: document.getElementById('myapp'),\n\
          \  flags: flags\n\
          \});\n\
          \\n\
          \app.ports.setStorageJson.subscribe(function(state) {\n\
          \  localStorage.setItem('myapp-model', JSON.stringify(state));\n\
          \});\n"

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
