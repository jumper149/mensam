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
        meta ! name "application-name" ! content "Mensam"
        meta ! name "color-scheme" ! content "dark"
        meta ! name "mobile-web-app-capable" ! content "yes"
        meta ! name "theme-color" ! content "#f0c674"
        meta ! name "viewport" ! content "width=device-width, height=device-height, initial-scale=1"
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
          ! hrefWithDepth baseUrl depth "static/fonts.css"
        script ! src (withDepth baseUrl depth "static/frontend.js") $ ""
      body $ do
        H.div ! H.A.id "mensam-frontend" $ ""
        script
          "var storageName = 'mensam-frontend-storage';\
          \var storageUnsafe = localStorage.getItem(storageName);\
          \\
          \var flags = \
          \  { storage: storageUnsafe ? JSON.parse(storageUnsafe) : null\
          \  , time: \
          \    { now: Date.now()\
          \    , zone: Intl.DateTimeFormat().resolvedOptions().timeZone\
          \    }\
          \  };\
          \\
          \var app = Elm.Main.init(\
          \  { node: document.getElementById('mensam-frontend')\
          \  , flags: flags\
          \  }\
          \);\
          \\
          \app.ports.setStorageJson.subscribe(function(state) {\
          \  localStorage.setItem(storageName, JSON.stringify(state));\
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
