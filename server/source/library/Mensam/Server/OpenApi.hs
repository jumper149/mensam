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
import Mensam.API.Route.Api.Reservation qualified as Route.Reservation
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.API.Update

import Control.Lens
import Data.Aeson qualified as A
import Data.HashMap.Strict.InsOrd qualified as HMIO
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Typeable
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
  declareNamedSchema Proxy =
    pure $
      NamedSchema (Just "OpenAPI") $
        mempty
          & example ?~ A.toJSON openapiExample
   where
    openapiExample :: OpenApi
    openapiExample =
      mempty
        & info . title .~ "Example API"
        & info . version .~ "1.0.0"
        & paths
          .~ HMIO.singleton
            "/hello_world"
            ( mempty
                & get
                  ?~ ( mempty
                        & at 200 ?~ "OK"
                     )
            )

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
  declareNamedSchema = pure . NamedSchema (Just $ "StaticText: " <> text) . paramSchemaToSchema
   where
    text = T.pack $ show $ A.toJSON $ MkStaticText @text

instance ToSchema Route.User.Jwt where
  declareNamedSchema Proxy =
    pure $
      NamedSchema (Just "JWT") $
        mempty
          & type_ ?~ OpenApiString
          & format ?~ "jwt"
          & example ?~ A.String "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWQiOnsidW5JZGVudGlmaWVyVXNlciI6Mn19LCJleHAiOjEuNjgwMzAyMDY3Njc1OTUxMjYyZTl9.6RGNeoQC7nrA4O_IYfaMchojHLk-ScKSBi1f7-R1_qhUdttNIzVJzZfORvt5IzSfo9ye4OnHphDLDiU76qFxEQ"

instance Typeable p => ToParamSchema (ErrorInsufficientPermission p) where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ [A.toJSON $ MkErrorInsufficientPermission @p]
instance Typeable p => ToSchema (ErrorInsufficientPermission p) where
  declareNamedSchema = pure . NamedSchema (Just $ "ErrorInsufficientPermission: " <> text) . paramSchemaToSchema
   where
    text = T.pack $ show $ A.toJSON $ MkErrorInsufficientPermission @p

deriving via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth instance ToSchema ErrorBasicAuth
deriving newtype instance ToSchema ErrorBearerAuth
deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJson") ErrorParseBodyJson instance ToSchema ErrorParseBodyJson
deriving via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier) instance (ToSchema name, ToSchema identifier) => ToSchema (NameOrIdentifier name identifier)
deriving via A.CustomJSON (JSONSettings "Mk" "user") User instance ToSchema User
deriving newtype instance ToSchema IdentifierUser
deriving newtype instance ToSchema ConfirmationSecret
deriving via A.CustomJSON (JSONSettings "Mk" "spaceView") SpaceView instance ToSchema SpaceView
deriving via A.CustomJSON (JSONSettings "Mk" "space") Space instance ToSchema Space
deriving newtype instance ToSchema IdentifierSpace
deriving newtype instance ToSchema NameSpace
deriving via A.CustomJSON (JSONSettings "Mk" "spaceRole") SpaceRole instance ToSchema SpaceRole
deriving newtype instance ToSchema IdentifierSpaceRole
deriving newtype instance ToSchema NameSpaceRole
deriving via A.CustomJSON (JSONSettings "MkAccessibilitySpaceRole" "") AccessibilitySpaceRole instance ToSchema AccessibilitySpaceRole
deriving via A.CustomJSON (JSONSettings "MkVisibilitySpace" "") VisibilitySpace instance ToSchema VisibilitySpace
deriving via A.CustomJSON (JSONSettings "Mk" "spaceUser") SpaceUser instance ToSchema SpaceUser
deriving via A.CustomJSON (JSONSettings "MkPermissionSpace" "") PermissionSpace instance ToSchema PermissionSpace
deriving via A.CustomJSON (JSONSettings "Mk" "desk") Desk instance ToSchema Desk
deriving newtype instance ToSchema IdentifierDesk
deriving newtype instance ToSchema NameDesk
deriving via A.CustomJSON (JSONSettings "Mk" "deskNameWithContext") DeskNameWithContext instance ToSchema DeskNameWithContext
deriving via A.CustomJSON (JSONSettings "Mk" "reservation") Reservation instance ToSchema Reservation
deriving via A.CustomJSON (JSONSettings "MkStatusReservation" "") StatusReservation instance ToSchema StatusReservation
deriving newtype instance ToSchema IdentifierReservation
instance ToSchema a => ToSchema (Interval a) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval a)))
      >>= \x ->
        pure $
          x
            & schema . description ?~ "An ordered interval: `start <= end`"
instance ToSchema a => ToSchema (IntervalNonDegenerate a) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval a)))
      >>= \x ->
        pure $
          x
            & schema . description ?~ "An ordered and non-degenerate interval: `start < end`"

deriving via A.CustomJSON (JSONSettings "" "") Order instance ToSchema Order
deriving via A.CustomJSON (JSONSettings "Mk" "orderByCategory") (OrderByCategory a) instance ToSchema a => ToSchema (OrderByCategory a)

deriving via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory instance ToSchema SpaceOrderCategory
deriving newtype instance ToSchema a => ToSchema (OrderByCategories a)

instance ToSchema a => ToSchema (Updatable a) where
  declareNamedSchema Proxy = do
    aSchema <- declareSchemaRef $ Proxy @a
    pure $
      NamedSchema (Just $ T.pack ("Updatable_" ++ tyConName (typeRepTyCon (typeRep $ Proxy @a)))) $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["update"]
          & properties
            .~ HMIO.fromList
              [ ("update", Inline $ mempty & type_ ?~ OpenApiBoolean)
              , ("value", aSchema)
              ]
          & description ?~ "Whether to overwrite a value with a new one. When \"update\" is `true` the \"value\" field is required. When \"update\" is `false` the value field must be omitted."

deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") Route.User.ResponseLogin instance ToSchema Route.User.ResponseLogin
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogout") Route.User.ResponseLogout instance ToSchema Route.User.ResponseLogout
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") Route.User.RequestRegister instance ToSchema Route.User.RequestRegister
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRegister") Route.User.ResponseRegister instance ToSchema Route.User.ResponseRegister
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestConfirm") Route.User.RequestConfirm instance ToSchema Route.User.RequestConfirm
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseConfirm") Route.User.ResponseConfirm instance ToSchema Route.User.ResponseConfirm
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestProfile") Route.User.RequestProfile instance ToSchema Route.User.RequestProfile
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") Route.User.ResponseProfile instance ToSchema Route.User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Route.Booking.RequestSpaceCreate instance ToSchema Route.Booking.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceCreate") Route.Booking.ResponseSpaceCreate instance ToSchema Route.Booking.ResponseSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceDelete") Route.Booking.RequestSpaceDelete instance ToSchema Route.Booking.RequestSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceDelete") Route.Booking.ResponseSpaceDelete instance ToSchema Route.Booking.ResponseSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceEdit") Route.Booking.RequestSpaceEdit instance ToSchema Route.Booking.RequestSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceEdit") Route.Booking.ResponseSpaceEdit instance ToSchema Route.Booking.ResponseSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceJoin") Route.Booking.RequestSpaceJoin instance ToSchema Route.Booking.RequestSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceJoin") Route.Booking.ResponseSpaceJoin instance ToSchema Route.Booking.ResponseSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceLeave") Route.Booking.RequestSpaceLeave instance ToSchema Route.Booking.RequestSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceLeave") Route.Booking.ResponseSpaceLeave instance ToSchema Route.Booking.ResponseSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceView") Route.Booking.RequestSpaceView instance ToSchema Route.Booking.RequestSpaceView
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView") Route.Booking.ResponseSpaceView instance ToSchema Route.Booking.ResponseSpaceView
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Route.Booking.RequestSpaceList instance ToSchema Route.Booking.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Route.Booking.ResponseSpaceList instance ToSchema Route.Booking.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleCreate") Route.Booking.RequestRoleCreate instance ToSchema Route.Booking.RequestRoleCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleCreate") Route.Booking.ResponseRoleCreate instance ToSchema Route.Booking.ResponseRoleCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleEdit") Route.Booking.RequestRoleEdit instance ToSchema Route.Booking.RequestRoleEdit
deriving via A.CustomJSON (JSONSettings "RoleEdit" "roleEditAccessibilityAndPassword") Route.Booking.RoleEditAccessibilityAndPassword instance ToSchema Route.Booking.RoleEditAccessibilityAndPassword
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleEdit") Route.Booking.ResponseRoleEdit instance ToSchema Route.Booking.ResponseRoleEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleDelete") Route.Booking.RequestRoleDelete instance ToSchema Route.Booking.RequestRoleDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleDelete") Route.Booking.ResponseRoleDelete instance ToSchema Route.Booking.ResponseRoleDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Route.Booking.RequestDeskCreate instance ToSchema Route.Booking.RequestDeskCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") Route.Booking.ResponseDeskCreate instance ToSchema Route.Booking.ResponseDeskCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskDelete") Route.Booking.RequestDeskDelete instance ToSchema Route.Booking.RequestDeskDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskDelete") Route.Booking.ResponseDeskDelete instance ToSchema Route.Booking.ResponseDeskDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") Route.Booking.RequestDeskList instance ToSchema Route.Booking.RequestDeskList
deriving via A.CustomJSON (JSONSettings "Mk" "deskWithInfo") Route.Booking.DeskWithInfo instance ToSchema Route.Booking.DeskWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") Route.Booking.ResponseDeskList instance ToSchema Route.Booking.ResponseDeskList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") Route.Reservation.RequestReservationCreate instance ToSchema Route.Reservation.RequestReservationCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") Route.Reservation.ResponseReservationCreate instance ToSchema Route.Reservation.ResponseReservationCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCancel") Route.Reservation.RequestReservationCancel instance ToSchema Route.Reservation.RequestReservationCancel
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCancel") Route.Reservation.ResponseReservationCancel instance ToSchema Route.Reservation.ResponseReservationCancel
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationList") Route.Reservation.RequestReservationList instance ToSchema Route.Reservation.RequestReservationList
deriving via A.CustomJSON (JSONSettings "Mk" "reservationWithInfo") Route.Reservation.ReservationWithInfo instance ToSchema Route.Reservation.ReservationWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationList") Route.Reservation.ResponseReservationList instance ToSchema Route.Reservation.ResponseReservationList

openapiJsonStdout :: IO ()
openapiJsonStdout = TL.putStrLn $ TL.decodeUtf8 $ A.encode Mensam.Server.OpenApi.openapi
