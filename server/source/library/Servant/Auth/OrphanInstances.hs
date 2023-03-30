{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.OrphanInstances () where

import Control.Lens
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.Kind
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Servant.API
import Servant.Auth qualified
import Servant.OpenApi

instance (HasOpenApi api, AuthMethods auths) => HasOpenApi (Servant.Auth.Auth auths a :> api) where
  toOpenApi Proxy = addAuthMethods (authMethods (Proxy @auths)) $ toOpenApi $ Proxy @api

addAuthMethods :: AuthMethodList auths -> OpenApi -> OpenApi
addAuthMethods = \case
  MethodBasicAuth nextMethod ->
    let
      identifier :: T.Text = "BasicAuth"
      securityScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
          , _securitySchemeDescription = Just "Basic Authentication"
          }
     in
      addAuthMethods nextMethod
        . addSecurityRequirement identifier
        . addSecurityScheme identifier securityScheme
  MethodJWT nextMethod ->
    let
      identifier :: T.Text = "JWT"
      securityScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
          , _securitySchemeDescription = Just "Bearer Authentication"
          }
     in
      addAuthMethods nextMethod
        . addSecurityRequirement identifier
        . addSecurityScheme identifier securityScheme
  MethodCookie nextMethod ->
    let
      identifier :: T.Text = "Cookie"
      securityScheme =
        SecurityScheme
          { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
          , _securitySchemeDescription = Just "Cookie Authentication"
          }
     in
      addAuthMethods nextMethod
        . addSecurityRequirement identifier
        . addSecurityScheme identifier securityScheme
  MethodNone -> id

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

addSecurityRequirement :: T.Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
  allOperations
    . security
    %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)
