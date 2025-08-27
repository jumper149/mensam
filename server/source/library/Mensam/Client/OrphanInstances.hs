{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Client.OrphanInstances where

import Mensam.API.Route.Api.User

import Data.Base64.Types qualified as Base64
import Data.ByteString qualified as B
import Data.Kind
import Data.Proxy
import Data.Sequence
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Base64 qualified as T
import GHC.Generics
import Network.HTTP.Types
import Servant.API hiding (BasicAuth, Header)
import Servant.Auth
import Servant.Auth.JWT.WithSession
import Servant.Client
import Servant.Client.Core qualified as Core

type AuthData :: [Type] -> Type
data AuthData xs :: Type where
  DataBasicAuth :: Credentials -> AuthData (BasicAuth ': auths)
  DataJWT :: Jwt -> AuthData (JWT ': auths)
  DataJWTWithSession :: Jwt -> AuthData (JWTWithSession ': auths)
  DataCookie :: Cookies -> AuthData (Cookie ': auths)
  DataNextAuth :: AuthData xs -> AuthData (x ': xs)

instance HasClient m api => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = AuthData auths -> Client m api
  clientWithRoute Proxy Proxy req = \case
    DataBasicAuth credentials ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = credentialsAuthorizationHeader credentials <| Core.requestHeaders req}
    DataJWT token ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = jwTokenAuthorizationHeader token <| Core.requestHeaders req}
    DataJWTWithSession token ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = jwTokenAuthorizationHeader token <| Core.requestHeaders req}
    DataCookie cookies ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = cookiesCookieHeader cookies <| Core.requestHeaders req}
    DataNextAuth (otherAuthData :: AuthData otherAuths) ->
      clientWithRoute (Proxy @m) (Proxy @(Auth otherAuths a :> api)) req otherAuthData
  hoistClientMonad Proxy Proxy f cl arg =
    hoistClientMonad (Proxy @m) (Proxy :: Proxy api) f (cl arg)

type Credentials :: Type
data Credentials = MkCredentials {credentialsUsername :: T.Text, credentialsPassword :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

credentialsAuthorizationHeader :: Credentials -> Header
credentialsAuthorizationHeader MkCredentials {credentialsUsername, credentialsPassword} =
  (hAuthorization,) $ ("Basic " <>) $ T.encodeUtf8 $ Base64.extractBase64 $ T.encodeBase64 $ credentialsUsername <> ":" <> credentialsPassword

jwTokenAuthorizationHeader :: Jwt -> Header
jwTokenAuthorizationHeader MkJwt {unJwt = jwt} =
  (hAuthorization,) $ ("Bearer " <>) $ T.encodeUtf8 jwt

type Cookies :: Type
newtype Cookies = MkCookies {unCookies :: B.ByteString}
  deriving stock (Eq, Generic, Ord, Read, Show)

cookiesCookieHeader :: Cookies -> Header
cookiesCookieHeader MkCookies {unCookies} = (hCookie, unCookies)
