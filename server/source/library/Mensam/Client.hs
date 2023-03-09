{-# LANGUAGE MonoLocalBinds #-}

module Mensam.Client where

import Mensam.Client.OrphanInstances
import Mensam.Server.Route.Booking.Type qualified as Route.Booking
import Mensam.Server.Route.Type qualified as Route
import Mensam.Server.Route.User.Type qualified as Route.User
import Mensam.User (Username (MkUsernameUnsafe))

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
    spacename :: T.Text = "universum"

  liftIO $ putStrLn "Register."
  let requestRegister =
        Route.User.MkRequestRegister
          { Route.User.requestRegisterName = MkUsernameUnsafe name
          , Route.User.requestRegisterPassword = pw
          , Route.User.requestRegisterEmail = email
          }
  resultRegister <- routes // Route.routeUser // Route.User.routeRegister $ requestRegister
  liftIO $ print resultRegister

  liftIO $ putStrLn "Login with BasicAuth."
  let credentials =
        MkCredentials
          { credentialsUsername = name
          , credentialsPassword = pw
          }
  resultLogin <- routes // Route.routeUser // Route.User.routeLogin $ DataBasicAuth credentials
  liftIO $ print resultLogin
  responseLogin <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined

  liftIO $ putStrLn "Login with JWT."
  let token = MkJWToken {unJWToken = Route.User.responseLoginJWT responseLogin}
  nextLoginResult <- routes // Route.routeUser // Route.User.routeLogin $ DataNextAuth $ DataJWT token
  liftIO $ print nextLoginResult
  nextResponseLogin <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined
  let nextToken = MkJWToken {unJWToken = Route.User.responseLoginJWT nextResponseLogin}

  liftIO $ putStrLn "Create space."
  let requestSpaceCreate =
        Route.Booking.MkRequestSpaceCreate
          { Route.Booking.requestSpaceCreateName = spacename
          , Route.Booking.requestSpaceCreateVisible = True
          }
  resultSpaceCreate <- (routes // Route.routeBooking // Route.Booking.routeSpaceCreate) (DataJWT nextToken) requestSpaceCreate
  liftIO $ print resultSpaceCreate

  pure ()
