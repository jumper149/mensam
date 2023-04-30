module Mensam.Server.Server.Route.OpenApi where

import Mensam.API.Route qualified as Route
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.OpenApi qualified as Route.Api.OpenApi
import Mensam.API.Route.OpenApi
import Mensam.API.Route.OpenApi qualified as Route.OpenApi
import Mensam.Server.Application.Configured.Class

import Data.List qualified as L
import Data.Text qualified as T
import Servant.Links
import Servant.Server.Generic
import Text.Blaze.Html qualified as Blaze
import Text.Blaze.Html5 qualified as Blaze
import Text.Blaze.Html5.Attributes qualified as Blaze.Attributes
import Text.Blaze.Internal qualified as Blaze.Internal

handler ::
  MonadConfigured m =>
  Routes (AsServerT m)
handler =
  Routes
    { routeRender = render
    }

render ::
  (MonadConfigured m) =>
  m Blaze.Html
render = do
  pure $
    Blaze.docTypeHtml $ do
      Blaze.head $ do
        Blaze.title $ Blaze.toMarkup @T.Text "Mensam API"
        Blaze.link
          Blaze.! Blaze.Attributes.rel "icon"
          Blaze.! Blaze.Attributes.type_ "image/png"
          Blaze.! Blaze.Attributes.sizes "32x32"
          Blaze.! Blaze.Attributes.href "static/favicon.png"
        Blaze.link
          Blaze.! Blaze.Attributes.rel "icon"
          Blaze.! Blaze.Attributes.type_ "image/png"
          Blaze.! Blaze.Attributes.sizes "192x192"
          Blaze.! Blaze.Attributes.href "static/favicon-192x192.png"
        Blaze.link
          Blaze.! Blaze.Attributes.rel "icon"
          Blaze.! Blaze.Attributes.type_ "image/png"
          Blaze.! Blaze.Attributes.sizes "512x512"
          Blaze.! Blaze.Attributes.href "static/favicon-512x512.png"
        Blaze.link
          Blaze.! Blaze.Attributes.rel "apple-touch-icon"
          Blaze.! Blaze.Attributes.type_ "image/png"
          Blaze.! Blaze.Attributes.sizes "512x512"
          Blaze.! Blaze.Attributes.href "static/favicon-512x512.png"

        --  needed for adaptive design
        Blaze.meta
          Blaze.! Blaze.Attributes.charset "utf-8"
        Blaze.meta
          Blaze.! Blaze.Attributes.name "viewport"
          Blaze.! Blaze.Attributes.content "width=device-width, initial-scale=1"
        Blaze.link
          Blaze.! Blaze.Attributes.rel "stylesheet"
          Blaze.! Blaze.Attributes.type_ "text/css"
          Blaze.! Blaze.Attributes.href "static/redoc.css"

        -- Redoc doesn't change outer page styles
        Blaze.style
          "body {\n\
          \  margin: 0;\n\
          \  padding: 0;\n\
          \}"

      Blaze.body $ do
        let
          redoc = Blaze.Internal.customParent $ Blaze.textTag "redoc"
          specUrl = Blaze.customAttribute "spec-url"
          linkCurrent = Route.OpenApi.routeRender . Route.routeOpenApi $ allFieldLinks
          linkOpenApiJson = Route.Api.OpenApi.routeJson . Route.Api.routeOpenApi . Route.routeApi $ allFieldLinks
        redoc Blaze.! specUrl (Blaze.stringValue $ relativePath linkCurrent linkOpenApiJson) $ ""
        Blaze.script Blaze.! Blaze.Attributes.src "static/redoc.standalone.js" $ ""

-- | Assuming that links don't have trailing slashes.
relativePath :: Link -> Link -> String
relativePath origin destination = goBack ++ "/" ++ goForward
 where
  goBack :: String
  goBack = case linkSegments origin of
    [] -> "."
    [_] -> "."
    _ : baseSegments -> L.intercalate "/" (".." <$ baseSegments)
  goForward :: String
  goForward = show $ linkURI destination
