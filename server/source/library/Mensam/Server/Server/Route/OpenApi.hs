module Mensam.Server.Server.Route.OpenApi where

import Mensam.API.Route.OpenApi
import Mensam.Server.Application.Configured.Class

import Data.Text qualified as T
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

render :: Monad m => m Blaze.Html
render =
  pure $
    Blaze.docTypeHtml $ do
      Blaze.head $ do
        Blaze.title $ Blaze.toMarkup @T.Text "Mensam API"

        --  needed for adaptive design
        Blaze.meta
          Blaze.! Blaze.Attributes.charset "utf-8"
        Blaze.meta
          Blaze.! Blaze.Attributes.name "viewport"
          Blaze.! Blaze.Attributes.content "width=device-width, initial-scale=1"
        -- TODO: Avoiding dependency for now. We should host fonts ourselves if we want this.
        -- Blaze.link
        --   Blaze.! Blaze.Attributes.href "https://fonts.googleapis.com/css?family=Montserrat:300,400,700|Roboto:300,400,700"
        --   Blaze.! Blaze.Attributes.rel "stylesheet"

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
        -- TODO: Use servant link instead of this hardcoded link.
        redoc Blaze.! specUrl "./api/openapi" $ ""
        -- TODO: We should host Redoc ourselves.
        Blaze.script Blaze.! Blaze.Attributes.src "https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js" $ ""
