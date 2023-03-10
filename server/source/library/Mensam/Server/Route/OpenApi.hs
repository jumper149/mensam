{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.Route.OpenApi where

import Mensam.Booking
import Mensam.Database.Extra qualified as Selda
import Mensam.Server.API
import Mensam.Server.Route.Booking.Type qualified as Booking
import Mensam.Server.Route.OpenApi.Type
import Mensam.Server.Route.User.Type qualified as User
import Mensam.User.Username

import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Data.Typeable
import Servant.API
import Servant.Auth qualified
import Servant.OpenApi
import Servant.RawM
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
specification = pure $ toOpenApi $ Proxy @API

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

-- TODO: Implement.
instance ToSchema OpenApi where
  declareNamedSchema Proxy = declareNamedSchema $ Proxy @()

-- TODO: Implement.
instance ToSchema Blaze.Markup where
  declareNamedSchema Proxy = declareNamedSchema $ Proxy @()

-- TODO: Implement.
instance HasOpenApi api => HasOpenApi (Servant.Auth.Auth auths a :> api) where
  toOpenApi Proxy = toOpenApi $ Proxy @api

instance HasOpenApi (RawM' a) where
  toOpenApi Proxy = toOpenApi $ Proxy @Raw

instance ToParamSchema Username
instance ToSchema Username
instance ToSchema Space
instance Typeable a => ToSchema (Selda.Identifier a)

instance ToSchema User.ResponseLogin
instance ToSchema User.RequestRegister
instance ToSchema User.ResponseProfile

instance ToSchema Booking.RequestSpaceCreate
instance ToSchema Booking.RequestSpaceList
instance ToSchema Booking.ResponseSpaceList
instance ToSchema Booking.RequestDeskCreate
