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

openapi ::
  -- | source URL
  Maybe T.Text ->
  OpenApi
openapi maybeSourceUrl =
  generatedOpenApi
    & info . title .~ "Mensam API"
    & info . description
      ?~ T.concat
        [ "This is the API for Mensam Desk-Booking.\n\
          \\n\
          \- [User Interface](..)\n"
        , case maybeSourceUrl of
            Nothing -> ""
            Just sourceUrl -> "- [GitHub](" <> sourceUrl <> ")\n"
        , "- [OpenAPI]()\n\
          \- [Haddock (server source)](./haddock/index.html)\n\
          \\n"
        ]
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

instance ToParamSchema Password where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & minLength ?~ 4
      & maxLength ?~ 32
      & Data.OpenApi.pattern ?~ ("^[a-zA-Z0-9" <> T.pack (escapeCharsForPatternCharacterSet passwordValidSymbols) <> "]{4,32}$")
instance ToSchema Password where
  declareNamedSchema = pure . NamedSchema (Just "Password") . paramSchemaToSchema

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

instance ToParamSchema (StaticTexts '[]) where
  toParamSchema Proxy =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ []
instance (KnownSymbol text, ToParamSchema (StaticTexts texts)) => ToParamSchema (StaticTexts (text : texts)) where
  toParamSchema Proxy =
    recParamSchema
      & enum_ .~ ((A.toJSON (MkStaticText @text) :) <$> (recParamSchema ^. enum_))
   where
    recParamSchema = toParamSchema (Proxy @(StaticTexts texts))
instance ToSchema (StaticTexts '[]) where
  declareNamedSchema = pure . NamedSchema (Just "StaticTexts: ") . paramSchemaToSchema
instance (KnownSymbol text, ToSchema (StaticTexts texts), Typeable texts) => ToSchema (StaticTexts (text : texts)) where
  declareNamedSchema Proxy = do
    namedSchema <- declareNamedSchema (Proxy @(StaticTexts texts))
    pure $
      namedSchema
        & enum_ .~ ((A.toJSON (MkStaticText @text) :) <$> (namedSchema ^. enum_))
        & name .~ ((<> (T.pack $ show $ A.toJSON $ MkStaticText @text)) <$> (namedSchema ^. name))

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

deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJpeg") Route.User.ErrorParseBodyJpeg instance ToSchema Route.User.ErrorParseBodyJpeg

deriving via A.CustomJSON (JSONSettings "MkErrorBasicAuth" "") ErrorBasicAuth instance ToSchema ErrorBasicAuth
deriving newtype instance ToSchema ErrorBearerAuth
deriving via A.CustomJSON (JSONSettings "Mk" "errorParseBodyJson") ErrorParseBodyJson instance ToSchema ErrorParseBodyJson
deriving via A.CustomJSON (JSONSettings "" "") (NameOrIdentifier name identifier) instance (ToSchema name, ToSchema identifier) => ToSchema (NameOrIdentifier name identifier)
deriving newtype instance ToParamSchema IdentifierUser
deriving newtype instance ToSchema IdentifierUser
deriving newtype instance ToSchema ConfirmationSecret
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
deriving via A.CustomJSON (JSONSettings "Mk" "locationDesk") LocationDesk instance ToSchema LocationDesk
deriving via A.CustomJSON (JSONSettings "Mk" "positionDesk") PositionDesk instance ToSchema PositionDesk
deriving newtype instance ToSchema DirectionDesk
instance ToSchema Direction where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @Double)
      >>= \x ->
        pure $
          x
            & schema . description ?~ "Direction on a 2D plane given as the angle relativ to North using degrees. The angle `x` must satisfy `0 <= x < 360`."
deriving via A.CustomJSON (JSONSettings "Mk" "sizeDesk") SizeDesk instance ToSchema SizeDesk
instance ToSchema (ConstrainedDouble '[]) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @Double)
      >>= \x ->
        pure $
          x
            & schema . description ?~ "A constrained number:"
instance (KnownNat n, ToSchema (ConstrainedDouble cs), Typeable cs) => ToSchema (ConstrainedDouble (MkConstraintDoubleGreaterEqual n : cs)) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & schema . description %~ ((<> (" `n >= " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, ToSchema (ConstrainedDouble cs), Typeable cs) => ToSchema (ConstrainedDouble (MkConstraintDoubleGreaterThan n : cs)) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & schema . description %~ ((<> (" `n > " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, ToSchema (ConstrainedDouble cs), Typeable cs) => ToSchema (ConstrainedDouble (MkConstraintDoubleLessEqual n : cs)) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & schema . description %~ ((<> (" `n <= " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
instance (KnownNat n, ToSchema (ConstrainedDouble cs), Typeable cs) => ToSchema (ConstrainedDouble (MkConstraintDoubleLessThan n : cs)) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(ConstrainedDouble cs))
      >>= \x ->
        pure $
          x
            & schema . description %~ ((<> (" `n < " <> T.pack (show (natVal (Proxy @n))) <> "`")) <$>)
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
instance ToSchema a => ToSchema (IntervalUnbounded a) where
  declareNamedSchema Proxy =
    declareNamedSchema (Proxy @(A.CustomJSON (JSONSettings "Mk" "interval") (Interval (Maybe a))))
      >>= \x ->
        pure $
          x
            & schema . description ?~ "An ordered and potentially unbounded interval: `start < end`"

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
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestPasswordChange") Route.User.RequestPasswordChange instance ToSchema Route.User.RequestPasswordChange
deriving via A.CustomJSON (JSONSettings "MkResponse" "responsePasswordChange") Route.User.ResponsePasswordChange instance ToSchema Route.User.ResponsePasswordChange
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseConfirmationRequest") Route.User.ResponseConfirmationRequest instance ToSchema Route.User.ResponseConfirmationRequest
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestConfirm") Route.User.RequestConfirm instance ToSchema Route.User.RequestConfirm
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseConfirm") Route.User.ResponseConfirm instance ToSchema Route.User.ResponseConfirm
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestNotifications") Route.User.RequestNotifications instance ToSchema Route.User.RequestNotifications
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseNotifications") Route.User.ResponseNotifications instance ToSchema Route.User.ResponseNotifications
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestProfile") Route.User.RequestProfile instance ToSchema Route.User.RequestProfile
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") Route.User.ResponseProfile instance ToSchema Route.User.ResponseProfile

deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceCreate") Route.Space.RequestSpaceCreate instance ToSchema Route.Space.RequestSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceCreate") Route.Space.ResponseSpaceCreate instance ToSchema Route.Space.ResponseSpaceCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceDelete") Route.Space.RequestSpaceDelete instance ToSchema Route.Space.RequestSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceDelete") Route.Space.ResponseSpaceDelete instance ToSchema Route.Space.ResponseSpaceDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceEdit") Route.Space.RequestSpaceEdit instance ToSchema Route.Space.RequestSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceEdit") Route.Space.ResponseSpaceEdit instance ToSchema Route.Space.ResponseSpaceEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceJoin") Route.Space.RequestSpaceJoin instance ToSchema Route.Space.RequestSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceJoin") Route.Space.ResponseSpaceJoin instance ToSchema Route.Space.ResponseSpaceJoin
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceLeave") Route.Space.RequestSpaceLeave instance ToSchema Route.Space.RequestSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceLeave") Route.Space.ResponseSpaceLeave instance ToSchema Route.Space.ResponseSpaceLeave
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceKick") Route.Space.RequestSpaceKick instance ToSchema Route.Space.RequestSpaceKick
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceKick") Route.Space.ResponseSpaceKick instance ToSchema Route.Space.ResponseSpaceKick
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceUserRole") Route.Space.RequestSpaceUserRole instance ToSchema Route.Space.RequestSpaceUserRole
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceUserRole") Route.Space.ResponseSpaceUserRole instance ToSchema Route.Space.ResponseSpaceUserRole
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceView") Route.Space.RequestSpaceView instance ToSchema Route.Space.RequestSpaceView
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceView") Route.Space.ResponseSpaceView instance ToSchema Route.Space.ResponseSpaceView
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestSpaceList") Route.Space.RequestSpaceList instance ToSchema Route.Space.RequestSpaceList
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseSpaceList") Route.Space.ResponseSpaceList instance ToSchema Route.Space.ResponseSpaceList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleCreate") Route.Space.RequestRoleCreate instance ToSchema Route.Space.RequestRoleCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleCreate") Route.Space.ResponseRoleCreate instance ToSchema Route.Space.ResponseRoleCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleEdit") Route.Space.RequestRoleEdit instance ToSchema Route.Space.RequestRoleEdit
deriving via A.CustomJSON (JSONSettings "RoleEdit" "roleEditAccessibilityAndPassword") Route.Space.RoleEditAccessibilityAndPassword instance ToSchema Route.Space.RoleEditAccessibilityAndPassword
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleEdit") Route.Space.ResponseRoleEdit instance ToSchema Route.Space.ResponseRoleEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestRoleDelete") Route.Space.RequestRoleDelete instance ToSchema Route.Space.RequestRoleDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseRoleDelete") Route.Space.ResponseRoleDelete instance ToSchema Route.Space.ResponseRoleDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskCreate") Route.Space.RequestDeskCreate instance ToSchema Route.Space.RequestDeskCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskCreate") Route.Space.ResponseDeskCreate instance ToSchema Route.Space.ResponseDeskCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskDelete") Route.Space.RequestDeskDelete instance ToSchema Route.Space.RequestDeskDelete
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskDelete") Route.Space.ResponseDeskDelete instance ToSchema Route.Space.ResponseDeskDelete
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskEdit") Route.Space.RequestDeskEdit instance ToSchema Route.Space.RequestDeskEdit
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskEdit") Route.Space.ResponseDeskEdit instance ToSchema Route.Space.ResponseDeskEdit
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestDeskList") Route.Space.RequestDeskList instance ToSchema Route.Space.RequestDeskList
deriving via A.CustomJSON (JSONSettings "Mk" "deskWithInfo") Route.Space.DeskWithInfo instance ToSchema Route.Space.DeskWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseDeskList") Route.Space.ResponseDeskList instance ToSchema Route.Space.ResponseDeskList
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCreate") Route.Reservation.RequestReservationCreate instance ToSchema Route.Reservation.RequestReservationCreate
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCreate") Route.Reservation.ResponseReservationCreate instance ToSchema Route.Reservation.ResponseReservationCreate
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationCancel") Route.Reservation.RequestReservationCancel instance ToSchema Route.Reservation.RequestReservationCancel
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationCancel") Route.Reservation.ResponseReservationCancel instance ToSchema Route.Reservation.ResponseReservationCancel
deriving via A.CustomJSON (JSONSettings "MkRequest" "requestReservationList") Route.Reservation.RequestReservationList instance ToSchema Route.Reservation.RequestReservationList
deriving via A.CustomJSON (JSONSettings "Mk" "reservationWithInfo") Route.Reservation.ReservationWithInfo instance ToSchema Route.Reservation.ReservationWithInfo
deriving via A.CustomJSON (JSONSettings "MkResponse" "responseReservationList") Route.Reservation.ResponseReservationList instance ToSchema Route.Reservation.ResponseReservationList

openapiJsonStdout :: IO ()
openapiJsonStdout = TL.putStrLn $ TL.decodeUtf8 $ A.encode $ Mensam.Server.OpenApi.openapi Nothing

escapeCharsForPatternCharacterSet :: [Char] -> String
escapeCharsForPatternCharacterSet = \case
  [] -> []
  '-' : chars -> '\\' : '-' : escapeCharsForPatternCharacterSet chars
  '[' : chars -> '\\' : '[' : escapeCharsForPatternCharacterSet chars
  ']' : chars -> '\\' : ']' : escapeCharsForPatternCharacterSet chars
  '\\' : chars -> '\\' : '\\' : escapeCharsForPatternCharacterSet chars
  char : chars -> char : escapeCharsForPatternCharacterSet chars
