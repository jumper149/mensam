{-# LANGUAGE GADTs #-}
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
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.Kind
import Data.OpenApi
import Data.OpenApi qualified as OpenApi (pattern)
import Data.Proxy
import Data.Text qualified as T
import Data.Typeable
import Deriving.Aeson qualified as A
import Deriving.Aeson.OrphanInstances qualified as A ()
import Servant.API
import Servant.Auth qualified
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

type AuthMethodList :: [Type] -> Type
data AuthMethodList xs :: Type where
  MethodBasicAuth :: AuthMethodList auths -> AuthMethodList (Servant.Auth.BasicAuth ': auths)
  MethodJWT :: AuthMethodList auths -> AuthMethodList (Servant.Auth.JWT ': auths)
  MethodCookie :: AuthMethodList auths -> AuthMethodList (Servant.Auth.Cookie ': auths)
  MethodNone :: AuthMethodList '[]

type AuthMethods :: [Type] -> Constraint
class AuthMethods auths where
  authMethods :: Proxy auths -> AuthMethodList auths

instance AuthMethods '[] where
  authMethods Proxy = MethodNone

instance AuthMethods auths => AuthMethods (Servant.Auth.BasicAuth ': auths) where
  authMethods Proxy = MethodBasicAuth $ authMethods Proxy

instance AuthMethods auths => AuthMethods (Servant.Auth.JWT ': auths) where
  authMethods Proxy = MethodJWT $ authMethods Proxy

instance AuthMethods auths => AuthMethods (Servant.Auth.Cookie ': auths) where
  authMethods Proxy = MethodCookie $ authMethods Proxy

addAuthMethods :: AuthMethodList auths -> OpenApi -> OpenApi
addAuthMethods = \case
  MethodBasicAuth nextMethod ->
    let
      identifier :: T.Text = "BasicAuth"
      addThisScheme =
        addSecurityScheme identifier $
          SecurityScheme
            { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
            , _securitySchemeDescription = Just "Basic Authentication"
            }
      addThisRequirement = addSecurityRequirement $ SecurityRequirement $ HM.singleton identifier []
     in
      addAuthMethods nextMethod . addThisRequirement . addThisScheme
  MethodJWT nextMethod ->
    let
      identifier :: T.Text = "JWT"
      addThisScheme =
        addSecurityScheme identifier $
          SecurityScheme
            { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
            , _securitySchemeDescription = Just "Bearer Authentication"
            }
      addThisRequirement = addSecurityRequirement $ SecurityRequirement $ HM.singleton identifier []
     in
      addAuthMethods nextMethod . addThisRequirement . addThisScheme
  MethodCookie nextMethod ->
    let
      identifier :: T.Text = "Cookie"
      addThisScheme =
        addSecurityScheme identifier $
          SecurityScheme
            { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
            , _securitySchemeDescription = Just "Cookie Authentication"
            }
      addThisRequirement = addSecurityRequirement $ SecurityRequirement $ HM.singleton identifier []
     in
      addAuthMethods nextMethod . addThisRequirement . addThisScheme
  MethodNone -> id

addSecurityScheme :: T.Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
  openApi
    { _openApiComponents =
        (_openApiComponents openApi)
          { _componentsSecuritySchemes =
              _componentsSecuritySchemes (_openApiComponents openApi)
                <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
          }
    }

addSecurityRequirement :: SecurityRequirement -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement = allOperations . security %~ (securityRequirement :)

instance (HasOpenApi api, AuthMethods auths) => HasOpenApi (Servant.Auth.Auth auths a :> api) where
  toOpenApi Proxy = addAuthMethods (authMethods (Proxy @auths)) $ toOpenApi $ Proxy @api

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
