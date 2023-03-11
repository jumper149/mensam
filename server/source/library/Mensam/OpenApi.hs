{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.OpenApi where

import Mensam.Aeson
import Mensam.Booking
import Mensam.Database.Extra qualified as Selda
import Mensam.Server.API
import Mensam.Server.Route.Booking.Type qualified as Booking
import Mensam.Server.Route.User.Type qualified as User
import Mensam.User.Username

import Control.Lens
import Data.OpenApi
import Data.OpenApi qualified as OpenApi (pattern)
import Data.Proxy
import Data.Typeable
import Deriving.Aeson qualified as A
import Deriving.Aeson.OrphanInstances qualified as A ()
import Servant.API
import Servant.Auth.OrphanInstances ()
import Servant.OpenApi
import Servant.RawM
import Text.Blaze.Html qualified as Blaze

openapi :: OpenApi
openapi = toOpenApi $ Proxy @API

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
      & OpenApi.pattern ?~ "^[a-zA-Z0-9]{4,32}$"
instance ToSchema Username where
  declareNamedSchema = pure . NamedSchema (Just "Username") . paramSchemaToSchema

deriving anyclass instance Typeable a => ToSchema (Selda.Identifier a)

deriving via A.CustomJSON (JSONSettings "Mk" "space") Space instance ToSchema Space

deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") User.ResponseLogin instance ToSchema User.ResponseLogin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") User.RequestRegister instance ToSchema User.RequestRegister
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") User.ResponseProfile instance ToSchema User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Booking.RequestSpaceCreate instance ToSchema Booking.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Booking.RequestSpaceList instance ToSchema Booking.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Booking.ResponseSpaceList instance ToSchema Booking.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Booking.RequestDeskCreate instance ToSchema Booking.RequestDeskCreate
