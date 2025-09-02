{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Server.OpenApi where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Data.User.Password
import Mensam.API.Data.User.Username
import Mensam.API.Order
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.Reservation qualified as Route.Reservation
import Mensam.API.Route.Api.Space qualified as Route.Space
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.API.Update

import Control.Lens
import Data.Aeson qualified as A
import Data.HashMap.Strict.InsOrd qualified as HMIO
import Data.OpenApi qualified as OpenApi
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

openapi ::
  OpenApi.OpenApi
openapi = toOpenApi $ Proxy @(NamedRoutes Route.Api.Routes)

instance OpenApi.ToSchema OpenApi.OpenApi where
  declareNamedSchema Proxy =
    pure $
      OpenApi.NamedSchema (Just "OpenAPI") $
        mempty
          & OpenApi.example ?~ A.toJSON openapiExample
   where
    openapiExample :: OpenApi.OpenApi
    openapiExample =
      mempty
        & OpenApi.info . OpenApi.title .~ "Example API"
        & OpenApi.info . OpenApi.version .~ "1.0.0"
        & OpenApi.paths
          .~ HMIO.singleton
            "/hello_world"
            ( mempty
                & OpenApi.get
                  ?~ ( mempty
                        & at 200 ?~ "OK"
                     )
            )

instance OpenApi.ToParamSchema Password where
  toParamSchema Proxy =
    mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.minLength ?~ 4
      & OpenApi.maxLength ?~ 32
      & OpenApi.pattern ?~ ("^[a-zA-Z0-9" <> T.pack (escapeCharsForPatternCharacterSet passwordValidSymbols) <> "]{4,32}$")
instance OpenApi.ToSchema Password where
  declareNamedSchema = pure . OpenApi.NamedSchema (Just "Password") . OpenApi.paramSchemaToSchema

instance OpenApi.ToParamSchema Username where
  toParamSchema Proxy =
    mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.minLength ?~ 4
      & OpenApi.maxLength ?~ 32
      & OpenApi.pattern ?~ "^[a-zA-Z0-9]{4,32}$"
instance OpenApi.ToSchema Username where
  declareNamedSchema = pure . OpenApi.NamedSchema (Just "Username") . OpenApi.paramSchemaToSchema

instance KnownSymbol text => OpenApi.ToParamSchema (StaticText text) where
  toParamSchema Proxy =
    mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.enum_ ?~ [A.toJSON $ MkStaticText @text]
instance KnownSymbol text => OpenApi.ToSchema (StaticText text) where
  declareNamedSchema = pure . OpenApi.NamedSchema (Just $ "StaticText: " <> text) . OpenApi.paramSchemaToSchema
   where
    text = T.pack $ show $ A.toJSON $ MkStaticText @text

instance OpenApi.ToParamSchema (StaticTexts '[]) where
  toParamSchema Proxy =
    mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.enum_ ?~ []
instance (KnownSymbol text, OpenApi.ToParamSchema (StaticTexts texts)) => OpenApi.ToParamSchema (StaticTexts (text : texts)) where
  toParamSchema Proxy =
    recParamSchema
      & OpenApi.enum_ .~ ((A.toJSON (MkStaticText @text) :) <$> (recParamSchema ^. OpenApi.enum_))
   where
    recParamSchema = OpenApi.toParamSchema (Proxy @(StaticTexts texts))
instance OpenApi.ToSchema (StaticTexts '[]) where
  declareNamedSchema = pure . OpenApi.NamedSchema (Just "StaticTexts: ") . OpenApi.paramSchemaToSchema
instance (KnownSymbol text, OpenApi.ToSchema (StaticTexts texts), Typeable texts) => OpenApi.ToSchema (StaticTexts (text : texts)) where
  declareNamedSchema Proxy = do
    namedSchema <- OpenApi.declareNamedSchema (Proxy @(StaticTexts texts))
    pure $
      namedSchema
        & OpenApi.enum_ .~ ((A.toJSON (MkStaticText @text) :) <$> (namedSchema ^. OpenApi.enum_))
        & OpenApi.name .~ ((<> (T.pack $ show $ A.toJSON $ MkStaticText @text)) <$> (namedSchema ^. OpenApi.name))

instance OpenApi.ToSchema Route.User.Jwt where
  declareNamedSchema Proxy =
    pure $
      OpenApi.NamedSchema (Just "JWT") $
        mempty
          & OpenApi.type_ ?~ OpenApi.OpenApiString
          & OpenApi.format ?~ "jwt"
          & OpenApi.example ?~ A.String "eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiaWQiOnsidW5JZGVudGlmaWVyVXNlciI6Mn19LCJleHAiOjEuNjgwMzAyMDY3Njc1OTUxMjYyZTl9.6RGNeoQC7nrA4O_IYfaMchojHLk-ScKSBi1f7-R1_qhUdttNIzVJzZfORvt5IzSfo9ye4OnHphDLDiU76qFxEQ"

instance Typeable p => OpenApi.ToParamSchema (ErrorInsufficientPermission p) where
  toParamSchema Proxy =
    mempty
      & OpenApi.type_ ?~ OpenApi.OpenApiString
      & OpenApi.enum_ ?~ [A.toJSON $ MkErrorInsufficientPermission @p]
instance Typeable p => OpenApi.ToSchema (ErrorInsufficientPermission p) where
  declareNamedSchema = pure . OpenApi.NamedSchema (Just $ "ErrorInsufficientPermission: " <> text) . OpenApi.paramSchemaToSchema
   where
    text = T.pack $ show $ A.toJSON $ MkErrorInsufficientPermission @p

deriving via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth instance OpenApi.ToSchema ErrorBasicAuth
deriving newtype instance OpenApi.ToSchema ErrorBearerAuth
deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJson") ErrorParseBodyJson instance OpenApi.ToSchema ErrorParseBodyJson
deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJpeg") ErrorParseBodyJpeg instance OpenApi.ToSchema ErrorParseBodyJpeg
deriving via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier) instance (OpenApi.ToSchema name, OpenApi.ToSchema identifier) => OpenApi.ToSchema (NameOrIdentifier name identifier)
deriving newtype instance OpenApi.ToParamSchema IdentifierUser
deriving newtype instance OpenApi.ToSchema IdentifierUser
deriving newtype instance OpenApi.ToSchema ConfirmationSecret
deriving via A.CustomJSON (JSONSettings "Mk" "space") Space instance OpenApi.ToSchema Space
deriving newtype instance OpenApi.ToSchema IdentifierSpace
deriving newtype instance OpenApi.ToParamSchema IdentifierSpace
deriving newtype instance OpenApi.ToSchema NameSpace
deriving via A.CustomJSON (JSONSettings "Mk" "role") Role instance OpenApi.ToSchema Role
deriving newtype instance OpenApi.ToSchema IdentifierRole
deriving newtype instance OpenApi.ToSchema NameRole
deriving via A.CustomJSON (JSONSettings "MkAccessibilityRole" "") AccessibilityRole instance OpenApi.ToSchema AccessibilityRole
deriving via A.CustomJSON (JSONSettings "MkDiscoverabilitySpace" "") DiscoverabilitySpace instance OpenApi.ToSchema DiscoverabilitySpace
deriving via A.CustomJSON (JSONSettings "Mk" "spaceUser") SpaceUser instance OpenApi.ToSchema SpaceUser
deriving via A.CustomJSON (JSONSettings "MkPermission" "") Permission instance OpenApi.ToSchema Permission
deriving via A.CustomJSON (JSONSettings "Mk" "desk") Desk instance OpenApi.ToSchema Desk
deriving newtype instance OpenApi.ToSchema IdentifierDesk
deriving newtype instance OpenApi.ToSchema NameDesk
deriving via A.CustomJSON (JSONSettings "Mk" "deskNameWithContext") DeskNameWithContext instance OpenApi.ToSchema DeskNameWithContext
deriving via A.CustomJSON (JSONSettings "Mk" "locationDesk") LocationDesk instance OpenApi.ToSchema LocationDesk
deriving via A.CustomJSON (JSONSettings "Mk" "positionDesk") PositionDesk instance OpenApi.ToSchema PositionDesk
deriving newtype instance OpenApi.ToSchema DirectionDesk
instance OpenApi.ToSchema Direction where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @Double)
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description ?~ "Direction on a 2D plane given as the angle relativ to North using degrees. The angle `x` must satisfy `0 <= x < 360`."
deriving via A.CustomJSON (JSONSettings "Mk" "sizeDesk") SizeDesk instance OpenApi.ToSchema SizeDesk
instance OpenApi.ToSchema (ConstrainedDouble '[]) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @Double)
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description ?~ "A constrained number:"
instance (KnownNat n, OpenApi.ToSchema (ConstrainedDouble cs), Typeable cs) => OpenApi.ToSchema (ConstrainedDouble (MkConstraintDoubleGreaterEqual n : cs)) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description %~ ((<> (" `n >= " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, OpenApi.ToSchema (ConstrainedDouble cs), Typeable cs) => OpenApi.ToSchema (ConstrainedDouble (MkConstraintDoubleGreaterThan n : cs)) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description %~ ((<> (" `n > " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, OpenApi.ToSchema (ConstrainedDouble cs), Typeable cs) => OpenApi.ToSchema (ConstrainedDouble (MkConstraintDoubleLessEqual n : cs)) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description %~ ((<> (" `n <= " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, OpenApi.ToSchema (ConstrainedDouble cs), Typeable cs) => OpenApi.ToSchema (ConstrainedDouble (MkConstraintDoubleLessThan n : cs)) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description %~ ((<> (" `n < " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
deriving via A.CustomJSON (JSONSettings "Mk" "reservation") Reservation instance OpenApi.ToSchema Reservation
deriving via A.CustomJSON (JSONSettings "MkStatusReservation" "") StatusReservation instance OpenApi.ToSchema StatusReservation
deriving newtype instance OpenApi.ToSchema IdentifierReservation
instance OpenApi.ToSchema a => OpenApi.ToSchema (Interval a) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval a)))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description ?~ "An ordered interval: `start <= end`"
instance OpenApi.ToSchema a => OpenApi.ToSchema (IntervalNonDegenerate a) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval a)))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description ?~ "An ordered and non-degenerate interval: `start < end`"
instance OpenApi.ToSchema a => OpenApi.ToSchema (IntervalUnbounded a) where
  declareNamedSchema Proxy =
    OpenApi.declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval (Maybe a))))
      >>= \x ->
        pure $
          x
            & OpenApi.schema . OpenApi.description ?~ "An ordered and potentially unbounded interval: `start < end`"

deriving via A.CustomJSON (JSONSettings "" "") Order instance OpenApi.ToSchema Order
deriving via A.CustomJSON (JSONSettings "Mk" "orderByCategory") (OrderByCategory a) instance OpenApi.ToSchema a => OpenApi.ToSchema (OrderByCategory a)

deriving via A.CustomJSON (JSONSettings "SpaceOrderCategory" "") SpaceOrderCategory instance OpenApi.ToSchema SpaceOrderCategory
deriving newtype instance OpenApi.ToSchema a => OpenApi.ToSchema (OrderByCategories a)

instance OpenApi.ToSchema a => OpenApi.ToSchema (Updatable a) where
  declareNamedSchema Proxy = do
    aSchema <- OpenApi.declareSchemaRef $ Proxy @a
    pure $
      OpenApi.NamedSchema (Just $ T.pack ("Updatable_" ++ tyConName (typeRepTyCon (typeRep $ Proxy @a)))) $
        mempty
          & OpenApi.type_ ?~ OpenApi.OpenApiObject
          & OpenApi.required .~ ["update"]
          & OpenApi.properties
            .~ HMIO.fromList
              [ ("update", OpenApi.Inline $ mempty & OpenApi.type_ ?~ OpenApi.OpenApiBoolean)
              , ("value", aSchema)
              ]
          & OpenApi.description ?~ "Whether to overwrite a value with a new one. When \"update\" is `true` the \"value\" field is required. When \"update\" is `false` the value field must be omitted."

deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") Route.User.ResponseLogin instance OpenApi.ToSchema Route.User.ResponseLogin
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseLogout") Route.User.ResponseLogout instance OpenApi.ToSchema Route.User.ResponseLogout
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") Route.User.RequestRegister instance OpenApi.ToSchema Route.User.RequestRegister
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRegister") Route.User.ResponseRegister instance OpenApi.ToSchema Route.User.ResponseRegister
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestPasswordChange") Route.User.RequestPasswordChange instance OpenApi.ToSchema Route.User.RequestPasswordChange
deriving via A.CustomJSON (JSONSettings "MkResponse" "responsePasswordChange") Route.User.ResponsePasswordChange instance OpenApi.ToSchema Route.User.ResponsePasswordChange
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseConfirmationRequest") Route.User.ResponseConfirmationRequest instance OpenApi.ToSchema Route.User.ResponseConfirmationRequest
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestConfirm") Route.User.RequestConfirm instance OpenApi.ToSchema Route.User.RequestConfirm
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseConfirm") Route.User.ResponseConfirm instance OpenApi.ToSchema Route.User.ResponseConfirm
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestNotifications") Route.User.RequestNotifications instance OpenApi.ToSchema Route.User.RequestNotifications
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseNotifications") Route.User.ResponseNotifications instance OpenApi.ToSchema Route.User.ResponseNotifications
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestProfile") Route.User.RequestProfile instance OpenApi.ToSchema Route.User.RequestProfile
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") Route.User.ResponseProfile instance OpenApi.ToSchema Route.User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Route.Space.RequestSpaceCreate instance OpenApi.ToSchema Route.Space.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceCreate") Route.Space.ResponseSpaceCreate instance OpenApi.ToSchema Route.Space.ResponseSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceDelete") Route.Space.RequestSpaceDelete instance OpenApi.ToSchema Route.Space.RequestSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceDelete") Route.Space.ResponseSpaceDelete instance OpenApi.ToSchema Route.Space.ResponseSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceEdit") Route.Space.RequestSpaceEdit instance OpenApi.ToSchema Route.Space.RequestSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceEdit") Route.Space.ResponseSpaceEdit instance OpenApi.ToSchema Route.Space.ResponseSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceJoin") Route.Space.RequestSpaceJoin instance OpenApi.ToSchema Route.Space.RequestSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceJoin") Route.Space.ResponseSpaceJoin instance OpenApi.ToSchema Route.Space.ResponseSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceLeave") Route.Space.RequestSpaceLeave instance OpenApi.ToSchema Route.Space.RequestSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceLeave") Route.Space.ResponseSpaceLeave instance OpenApi.ToSchema Route.Space.ResponseSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceKick") Route.Space.RequestSpaceKick instance OpenApi.ToSchema Route.Space.RequestSpaceKick
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceKick") Route.Space.ResponseSpaceKick instance OpenApi.ToSchema Route.Space.ResponseSpaceKick
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceUserRole") Route.Space.RequestSpaceUserRole instance OpenApi.ToSchema Route.Space.RequestSpaceUserRole
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceUserRole") Route.Space.ResponseSpaceUserRole instance OpenApi.ToSchema Route.Space.ResponseSpaceUserRole
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceView") Route.Space.RequestSpaceView instance OpenApi.ToSchema Route.Space.RequestSpaceView
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView") Route.Space.ResponseSpaceView instance OpenApi.ToSchema Route.Space.ResponseSpaceView
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView403") Route.Space.ResponseSpaceView403 instance OpenApi.ToSchema Route.Space.ResponseSpaceView403
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Route.Space.RequestSpaceList instance OpenApi.ToSchema Route.Space.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Route.Space.ResponseSpaceList instance OpenApi.ToSchema Route.Space.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "Mk" "spaceListSpace") Route.Space.SpaceListSpace instance OpenApi.ToSchema Route.Space.SpaceListSpace
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleCreate") Route.Space.RequestRoleCreate instance OpenApi.ToSchema Route.Space.RequestRoleCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleCreate") Route.Space.ResponseRoleCreate instance OpenApi.ToSchema Route.Space.ResponseRoleCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleEdit") Route.Space.RequestRoleEdit instance OpenApi.ToSchema Route.Space.RequestRoleEdit
deriving via A.CustomJSON (JSONSettings "RoleEdit" "roleEditAccessibilityAndPassword") Route.Space.RoleEditAccessibilityAndPassword instance OpenApi.ToSchema Route.Space.RoleEditAccessibilityAndPassword
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleEdit") Route.Space.ResponseRoleEdit instance OpenApi.ToSchema Route.Space.ResponseRoleEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleDelete") Route.Space.RequestRoleDelete instance OpenApi.ToSchema Route.Space.RequestRoleDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleDelete") Route.Space.ResponseRoleDelete instance OpenApi.ToSchema Route.Space.ResponseRoleDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Route.Space.RequestDeskCreate instance OpenApi.ToSchema Route.Space.RequestDeskCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") Route.Space.ResponseDeskCreate instance OpenApi.ToSchema Route.Space.ResponseDeskCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskDelete") Route.Space.RequestDeskDelete instance OpenApi.ToSchema Route.Space.RequestDeskDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskDelete") Route.Space.ResponseDeskDelete instance OpenApi.ToSchema Route.Space.ResponseDeskDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskEdit") Route.Space.RequestDeskEdit instance OpenApi.ToSchema Route.Space.RequestDeskEdit
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskEdit") Route.Space.ResponseDeskEdit instance OpenApi.ToSchema Route.Space.ResponseDeskEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") Route.Space.RequestDeskList instance OpenApi.ToSchema Route.Space.RequestDeskList
deriving via A.CustomJSON (JSONSettings "Mk" "deskWithInfo") Route.Space.DeskWithInfo instance OpenApi.ToSchema Route.Space.DeskWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") Route.Space.ResponseDeskList instance OpenApi.ToSchema Route.Space.ResponseDeskList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") Route.Reservation.RequestReservationCreate instance OpenApi.ToSchema Route.Reservation.RequestReservationCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") Route.Reservation.ResponseReservationCreate instance OpenApi.ToSchema Route.Reservation.ResponseReservationCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCancel") Route.Reservation.RequestReservationCancel instance OpenApi.ToSchema Route.Reservation.RequestReservationCancel
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCancel") Route.Reservation.ResponseReservationCancel instance OpenApi.ToSchema Route.Reservation.ResponseReservationCancel
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationList") Route.Reservation.RequestReservationList instance OpenApi.ToSchema Route.Reservation.RequestReservationList
deriving via A.CustomJSON (JSONSettings "Mk" "reservationWithInfo") Route.Reservation.ReservationWithInfo instance OpenApi.ToSchema Route.Reservation.ReservationWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationList") Route.Reservation.ResponseReservationList instance OpenApi.ToSchema Route.Reservation.ResponseReservationList

openapiJsonStdout :: IO ()
openapiJsonStdout = TL.putStrLn $ TL.decodeUtf8 $ A.encode Mensam.Server.OpenApi.openapi

escapeCharsForPatternCharacterSet :: [Char] -> String
escapeCharsForPatternCharacterSet = \case
  [] -> []
  '-' : chars -> '\\' : '-' : escapeCharsForPatternCharacterSet chars
  '[' : chars -> '\\' : '[' : escapeCharsForPatternCharacterSet chars
  ']' : chars -> '\\' : ']' : escapeCharsForPatternCharacterSet chars
  '\\' : chars -> '\\' : '\\' : escapeCharsForPatternCharacterSet chars
  char : chars -> char : escapeCharsForPatternCharacterSet chars
