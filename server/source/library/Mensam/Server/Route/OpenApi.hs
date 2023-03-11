module Mensam.Server.Route.OpenApi where

import Mensam.OpenApi qualified
import Mensam.Server.Route.OpenApi.Type

import Data.OpenApi
import Data.Text qualified as T
import Servant.Server.Generic
import Text.Blaze.Html qualified as Blaze
import Text.Blaze.Html5 qualified as Blaze
import Text.Blaze.Html5.Attributes qualified as Blaze.Attributes
import Text.Blaze.Internal qualified as Blaze.Internal

handler ::
  Monad m =>
  Routes (AsServerT m)
handler =
  Routes
    { routeRender = render
    , routeJson = specification
    }

specification :: Applicative m => m OpenApi
specification = pure Mensam.OpenApi.openapi

render :: Monad m => m Blaze.Html
render =
  pure $
    Blaze.docTypeHtml $ do
      Blaze.head $ Blaze.title $ Blaze.toMarkup @T.Text "Mensam API"
      -- TODO: Check if this is actually required.
      --     <!-- needed for adaptive design -->
      --     <meta charset="utf-8"/>
      --     <meta name="viewport" content="width=device-width, initial-scale=1">
      --     <link href="https://fonts.googleapis.com/css?family=Montserrat:300,400,700|Roboto:300,400,700" rel="stylesheet">
      --
      --     <!--
      --     Redoc doesn't change outer page styles
      --     -->
      --     <style>
      --       body {
      --         margin: 0;
      --         padding: 0;
      --       }
      --     </style>
      Blaze.body $ do
        let
          redoc = Blaze.Internal.customParent $ Blaze.textTag "redoc"
          specUrl = Blaze.customAttribute "spec-url"
        redoc Blaze.! specUrl "./openapi/json" $ ""
        Blaze.script Blaze.! Blaze.Attributes.src "https://cdn.redoc.ly/redoc/latest/bundles/redoc.standalone.js" $ ""
