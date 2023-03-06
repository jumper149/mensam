{-# LANGUAGE MonoLocalBinds #-}

module Mensam.Client where

import Mensam.Client.OrphanInstances
import Mensam.Server.Route.Type qualified as Route
import Mensam.Server.Route.User.Type qualified as Route.User

import Control.Monad.IO.Class
import Data.Proxy
import Data.SOP
import Data.Text qualified as T
import Network.HTTP.Client qualified as Network
import Servant
import Servant.Client
import Servant.RawM.Client ()

routes :: Route.Routes (AsClientT ClientM)
routes = client $ Proxy @(NamedRoutes Route.Routes)

runF :: IO ()
runF = do
  httpManager <- Network.newManager Network.defaultManagerSettings
  let baseUrl =
        BaseUrl
          { baseUrlScheme = Http
          , baseUrlHost = "localhost"
          , baseUrlPort = 8177
          , baseUrlPath = ""
          }
  let clientEnv = mkClientEnv httpManager baseUrl
  result <- runClientM f clientEnv
  print result

f :: ClientM ()
f = do
  let
    name :: T.Text = "maxmustermann5"
    pw :: T.Text = "asdf"
    email :: T.Text = "maxmustermann@gmail.com"
  let requestRegister =
        Route.User.MkRequestRegister
          { Route.User.requestRegisterName = name
          , Route.User.requestRegisterPassword = pw
          , Route.User.requestRegisterEmail = email
          }
  resultRegister <- routes // Route.routeUser // Route.User.routeRegister $ requestRegister
  liftIO $ print resultRegister
  let credentials =
        MkCredentials
          { username = name
          , password = "asdf"
          }
  resultLogin <- routes // Route.routeUser // Route.User.routeLogin $ Credentials credentials
  liftIO $ print resultLogin
  Route.User.MkResponseLogin {Route.User.responseLoginJWT} <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined
  let token = MkJWToken {unJWToken = responseLoginJWT}
  nextLoginResult <- routes // Route.routeUser // Route.User.routeLogin $ NextAuthData $ BearerToken token
  liftIO $ print nextLoginResult
  pure ()
