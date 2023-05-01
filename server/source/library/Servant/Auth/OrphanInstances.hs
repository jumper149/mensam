{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.OrphanInstances () where

import Control.Lens
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.OpenApi
import Data.Proxy
import Data.Text qualified as T
import Servant.API
import Servant.Auth qualified
import Servant.Auth.JWT.WithSession
import Servant.OpenApi

instance (HasOpenApi api) => HasOpenApi (Servant.Auth.Auth '[] a :> api) where
  toOpenApi Proxy = toOpenApi $ Proxy @api

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.BasicAuth : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "BasicAuth"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
        , _securitySchemeDescription = Just "Basic Authentication"
        }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.JWT : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "JWT"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Bearer Authentication"
        }

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (JWTWithSession : auths) a :> api) where
  toOpenApi Proxy = toOpenApi $ Proxy @(Servant.Auth.Auth (Servant.Auth.JWT : auths) a :> api)

instance (HasOpenApi (Servant.Auth.Auth auths a :> api)) => HasOpenApi (Servant.Auth.Auth (Servant.Auth.Cookie : auths) a :> api) where
  toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Servant.Auth.Auth auths a :> api)
   where
    addSecurity = addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
    identifier :: T.Text = "Cookie"
    securityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
        , _securitySchemeDescription = Just "Cookie Authentication"
        }

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
