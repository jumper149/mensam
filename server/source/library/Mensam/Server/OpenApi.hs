{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.OpenApi where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.API.Order
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User

import Control.Lens
import Data.Aeson qualified as A
import Data.OpenApi
import Data.Proxy
import Deriving.Aeson qualified as A
import Deriving.Aeson.OrphanInstances qualified as A ()
import GHC.TypeLits
import Servant.API
import Servant.Auth.OrphanInstances ()
import Servant.OpenApi
import Text.Email.OrphanInstances ()

openapi :: OpenApi
openapi =
  generatedOpenApi
    & info . title .~ "Mensam API"
    & info . description ?~ "This is an API for hot desk booking."
    & info . license ?~ "GNU Affero General Public License v3.0"
 where
  generatedOpenApi = toOpenApi $ Proxy @(NamedRoutes Route.Api.Routes)

instance ToSchema OpenApi where
  declareNamedSchema Proxy = pure $ NamedSchema (Just "OpenAPI Specification") mempty

instance ToParamSchema Username where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & minLength ?~ 4
      & maxLength ?~ 32
      & Data.OpenApi.pattern ?~ "^[a-zA-Z0-9]{4,32}$"
instance ToSchema Username where
  declareNamedSchema = pure . NamedSchema (Just "Username") . paramSchemaToSchema

instance KnownSymbol text => ToParamSchema (StaticText text) where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ [A.toJSON $ MkStaticText @text]
instance KnownSymbol text => ToSchema (StaticText text) where
  declareNamedSchema = pure . NamedSchema (Just "StaticText") . paramSchemaToSchema

deriving via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth instance ToSchema ErrorBasicAuth
deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJson") ErrorParseBodyJson instance ToSchema ErrorParseBodyJson
deriving via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier) instance (ToSchema name, ToSchema identifier) => ToSchema (NameOrIdentifier name identifier)
deriving via A.CustomJSON (JSONSettings "Mk" "user") User instance ToSchema User
deriving anyclass instance ToSchema IdentifierUser
deriving via A.CustomJSON (JSONSettings "Mk" "space") Space instance ToSchema Space
deriving anyclass instance ToSchema IdentifierSpace
deriving newtype instance ToSchema NameSpace
deriving via A.CustomJSON (JSONSettings "MkVisibilitySpace" "") VisibilitySpace instance ToSchema VisibilitySpace
deriving via A.CustomJSON (JSONSettings "Mk" "desk") Desk instance ToSchema Desk
deriving anyclass instance ToSchema IdentifierDesk
deriving newtype instance ToSchema NameDesk
deriving via A.CustomJSON (JSONSettings "Mk" "reservation") Reservation instance ToSchema Reservation
deriving anyclass instance ToSchema IdentifierReservation

deriving via A.CustomJSON (JSONSettings "" "") Order instance ToSchema Order
deriving via A.CustomJSON (JSONSettings "Mk" "orderByCategory") (OrderByCategory a) instance ToSchema a => ToSchema (OrderByCategory a)

deriving via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory instance ToSchema SpaceOrderCategory
deriving newtype instance ToSchema a => ToSchema (OrderByCategories a)

deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") Route.User.ResponseLogin instance ToSchema Route.User.ResponseLogin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") Route.User.RequestRegister instance ToSchema Route.User.RequestRegister
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") Route.User.ResponseProfile instance ToSchema Route.User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Route.Booking.RequestSpaceCreate instance ToSchema Route.Booking.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceCreate") Route.Booking.ResponseSpaceCreate instance ToSchema Route.Booking.ResponseSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Route.Booking.RequestSpaceList instance ToSchema Route.Booking.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Route.Booking.ResponseSpaceList instance ToSchema Route.Booking.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Route.Booking.RequestDeskCreate instance ToSchema Route.Booking.RequestDeskCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") Route.Booking.ResponseDeskCreate instance ToSchema Route.Booking.ResponseDeskCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") Route.Booking.RequestDeskList instance ToSchema Route.Booking.RequestDeskList
deriving via A.CustomJSON (JSONSettings "Mk" "deskWithInfo") Route.Booking.DeskWithInfo instance ToSchema Route.Booking.DeskWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") Route.Booking.ResponseDeskList instance ToSchema Route.Booking.ResponseDeskList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") Route.Booking.RequestReservationCreate instance ToSchema Route.Booking.RequestReservationCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") Route.Booking.ResponseReservationCreate instance ToSchema Route.Booking.ResponseReservationCreate
