module Mensam.Server.Server.Route.Frontend where

import Mensam.API.Route.Frontend
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Configuration
import Mensam.Server.Configuration.BaseUrl

import Control.Monad.Logger.CallStack
import Data.Aeson.Text qualified as A
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as M
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Numeric.Natural
import Servant
import Text.Blaze qualified as B
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as H.A

handler ::
  (MonadConfigured m, MonadLogger m) =>
  ServerT API m
handler segments = do
  baseUrl <- configBaseUrl <$> configuration
  fontConfigs <- configFonts <$> configuration
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
        meta ! name "theme-color" ! content "#282a2e"
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
        meta ! name "apple-mobile-web-app-capable" ! content "yes"
        meta ! name "apple-mobile-web-app-status-bar-style" ! content "black"
        link
          ! rel "apple-touch-icon"
          ! type_ "image/png"
          ! sizes "512x512"
          ! hrefWithDepth baseUrl depth "static/favicon-512x512.png"
        link
          ! rel "stylesheet"
          ! type_ "text/css"
          ! hrefWithDepth baseUrl depth "static/fonts.css"
        sequence_ $ M.mapMaybe (fontPreloadLinkMaybe baseUrl depth) fontConfigs
        script ! src (withDepth baseUrl depth "static/frontend.js") $ ""
      body $ do
        H.div ! H.A.id "mensam-frontend" $ ""
        script $
          fold
            [ "\
              \var storageName = 'mensam-frontend-storage';\
              \var storageUnsafe = localStorage.getItem(storageName);\
              \\
              \var flags = \
              \  { \"storage\": storageUnsafe ? JSON.parse(storageUnsafe) : null\
              \  , \"time\": \
              \    { \"now\": Date.now()\
              \    , \"zone\": Intl.DateTimeFormat().resolvedOptions().timeZone\
              \    }\
              \  , \"base-url\": \
              \"
            , preEscapedText $ TL.toStrict $ A.encodeToLazyText baseUrl
            , "\
              \  };\
              \\
              \var app = Elm.Main.init(\
              \  { \"node\": document.getElementById('mensam-frontend')\
              \  , \"flags\": flags\
              \  }\
              \);\
              \\
              \app.ports.setStorageJson.subscribe(function(state) {\
              \  localStorage.setItem(storageName, JSON.stringify(state));\
              \});\
              \\
              \app.ports.copyTextToClipboard.subscribe(function(content) {\
              \  navigator.clipboard.writeText(content);\
              \});\
              \"
            ]

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

fontPreloadLinkMaybe ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  FontConfig ->
  Maybe Html
fontPreloadLinkMaybe baseUrl depth fontConfig =
  if fontPreload fontConfig
    then Just $ link ! rel "preload" ! href (fontUrl baseUrl depth $ fontPathPieces fontConfig) ! B.customAttribute "as" "font" ! type_ "font/woff2" ! B.customAttribute "crossorigin" "anonymous"
    else Nothing

fontUrl ::
  BaseUrl ->
  -- | depth
  Maybe Natural ->
  NE.NonEmpty T.Text ->
  AttributeValue
fontUrl baseUrl depth pathPieces = withDepth baseUrl depth fontPathSerialized
 where
  fontPathSerialized =
    case B.textValue <$> pathPieces of
      x NE.:| xs -> fold $ L.intersperse "/" (x : xs)
