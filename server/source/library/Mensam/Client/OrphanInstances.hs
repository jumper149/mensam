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
  Credentials :: Credentials -> AuthData (BasicAuth ': auths)
  BearerToken :: JWToken -> AuthData (JWT ': auths)
  NextAuthData :: AuthData xs -> AuthData (x ': xs)

instance HasClient m api => HasClient m (Auth auths a :> api) where
  type Client m (Auth auths a :> api) = AuthData auths -> Client m api
  clientWithRoute Proxy Proxy req = \case
    Credentials credentials ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = credentialsAuthorizationHeader credentials <| Core.requestHeaders req}
    BearerToken token ->
      clientWithRoute (Proxy @m) (Proxy @api) $
        req {Core.requestHeaders = jwTokenAuthorizationHeader token <| Core.requestHeaders req}
    NextAuthData (otherAuthData :: AuthData otherAuths) ->
      clientWithRoute (Proxy @m) (Proxy @(Auth otherAuths a :> api)) req otherAuthData
  hoistClientMonad Proxy Proxy f cl arg =
    hoistClientMonad (Proxy @m) (Proxy :: Proxy api) f (cl arg)

type Credentials :: Type
data Credentials = MkCredentials {username :: T.Text, password :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

credentialsAuthorizationHeader :: Credentials -> Header
credentialsAuthorizationHeader MkCredentials {username, password} =
  ("Authorization",) $ ("Basic " <>) $ T.encodeUtf8 $ T.encodeBase64 $ username <> ":" <> password

type JWToken :: Type
newtype JWToken = MkJWToken {unJWToken :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)

jwTokenAuthorizationHeader :: JWToken -> Header
jwTokenAuthorizationHeader MkJWToken {unJWToken} =
  ("Authorization",) $ ("Bearer " <>) $ T.encodeUtf8 unJWToken
