{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.OpenApi where

import Mensam.API.API
import Mensam.API.Aeson
import Mensam.API.Desk
import Mensam.API.Order
import Mensam.API.Route.Booking.Type qualified as Booking
import Mensam.API.Route.User.Type qualified as User
import Mensam.API.Space
import Mensam.API.User.Username

import Control.Lens
import Data.OpenApi
import Data.Proxy
import Deriving.Aeson qualified as A
import Deriving.Aeson.OrphanInstances qualified as A ()
import Servant.API
import Servant.Auth.OrphanInstances ()
import Servant.OpenApi
import Servant.RawM
import Text.Blaze.Html qualified as Blaze
import Text.Email.OrphanInstances ()

openapi :: OpenApi
openapi =
  generatedOpenApi
    & info . title .~ "Mensam API"
    & info . description ?~ "This is an API for hot desk booking."
    & info . license ?~ "GNU Affero General Public License v3.0"
 where
  generatedOpenApi = toOpenApi $ Proxy @API

-- TODO: Implement.
instance ToSchema OpenApi where
  declareNamedSchema Proxy = declareNamedSchema $ Proxy @()

-- TODO: Implement.
instance ToSchema Blaze.Markup where
  declareNamedSchema Proxy = declareNamedSchema $ Proxy @()

instance HasOpenApi (RawM' a) where
  toOpenApi Proxy = toOpenApi $ Proxy @Raw

instance ToParamSchema Username where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & minLength ?~ 4
      & maxLength ?~ 32
      & Data.OpenApi.pattern ?~ "^[a-zA-Z0-9]{4,32}$"
instance ToSchema Username where
  declareNamedSchema = pure . NamedSchema (Just "Username") . paramSchemaToSchema

deriving via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier) instance (ToSchema name, ToSchema identifier) => ToSchema (NameOrIdentifier name identifier)
deriving via A.CustomJSON (JSONSettings "Mk" "space") Space instance ToSchema Space
deriving anyclass instance ToSchema IdentifierSpace
deriving via A.CustomJSON (JSONSettings "Mk" "desk") Desk instance ToSchema Desk
deriving anyclass instance ToSchema IdentifierDesk

deriving via A.CustomJSON (JSONSettings "" "") Order instance ToSchema Order
deriving via A.CustomJSON (JSONSettings "Mk" "orderByCategory") (OrderByCategory a) instance ToSchema a => ToSchema (OrderByCategory a)

deriving via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory instance ToSchema SpaceOrderCategory
deriving newtype instance ToSchema a => ToSchema (OrderByCategories a)

deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") User.ResponseLogin instance ToSchema User.ResponseLogin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") User.RequestRegister instance ToSchema User.RequestRegister
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") User.ResponseProfile instance ToSchema User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Booking.RequestSpaceCreate instance ToSchema Booking.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Booking.RequestSpaceList instance ToSchema Booking.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Booking.ResponseSpaceList instance ToSchema Booking.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Booking.RequestDeskCreate instance ToSchema Booking.RequestDeskCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") Booking.RequestDeskList instance ToSchema Booking.RequestDeskList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") Booking.ResponseDeskList instance ToSchema Booking.ResponseDeskList
