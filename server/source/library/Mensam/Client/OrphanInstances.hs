{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mensam.Client.OrphanInstances where

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
import Servant.Client
import Servant.Client.Core qualified as Core
import Servant.HTML.Blaze
import Text.Blaze.Html

instance MimeUnrender HTML Markup where
  mimeUnrender Proxy = error "blaze-html doesn't support parsing"

type AuthData :: [Type] -> Type
data AuthData xs :: Type where
  DataBasicAuth :: Credentials -> AuthData (BasicAuth ': auths)
  DataJWT :: JWToken -> AuthData (JWT ': auths)
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
    DataNextAuth (otherAuthData :: AuthData otherAuths) ->
      clientWithRoute (Proxy @m) (Proxy @(Auth otherAuths a :> api)) req otherAuthData
  hoistClientMonad Proxy Proxy f cl arg =
    hoistClientMonad (Proxy @m) (Proxy :: Proxy api) f (cl arg)

type Credentials :: Type
data Credentials = MkCredentials {credentialsUsername :: T.Text, credentialsPassword :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

credentialsAuthorizationHeader :: Credentials -> Header
credentialsAuthorizationHeader MkCredentials {credentialsUsername, credentialsPassword} =
  (hAuthorization,) $ ("Basic " <>) $ T.encodeUtf8 $ T.encodeBase64 $ credentialsUsername <> ":" <> credentialsPassword

type JWToken :: Type
newtype JWToken = MkJWToken {unJWToken :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

jwTokenAuthorizationHeader :: JWToken -> Header
jwTokenAuthorizationHeader MkJWToken {unJWToken} =
  (hAuthorization,) $ ("Bearer " <>) $ T.encodeUtf8 unJWToken
