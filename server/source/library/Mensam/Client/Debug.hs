module Mensam.Client.Debug where

import Mensam.API.Aeson
import Mensam.API.Route.Booking.Type qualified as Route.Booking
import Mensam.API.Route.Type qualified as Route
import Mensam.API.Route.User.Type qualified as Route.User
import Mensam.API.User.Username
import Mensam.Client.OrphanInstances

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
    name :: Username = MkUsernameUnsafe "maxmustermann7"
    pw :: T.Text = "asdf"
    email :: T.Text = "maxmustermann@gmail.com"
    spacename :: T.Text = "solarsystem"

  liftIO $ putStrLn "Register."
  let requestRegister =
        Route.User.MkRequestRegister
          { Route.User.requestRegisterName = name
          , Route.User.requestRegisterPassword = pw
          , Route.User.requestRegisterEmail = email
          , Route.User.requestRegisterEmailVisible = True
          }
  resultRegister <- routes // Route.routeUser // Route.User.routeRegister $ requestRegister
  liftIO $ print resultRegister

  liftIO $ putStrLn "Profile."
  resultProfile <- routes // Route.routeUser // Route.User.routeProfile $ name
  liftIO $ print resultProfile

  liftIO $ putStrLn "Login with BasicAuth."
  let credentials =
        MkCredentials
          { credentialsUsername = unUsername name
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

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "neptune"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate <- (routes // Route.routeBooking // Route.Booking.routeDeskCreate) (DataJWT nextToken) requestDeskCreate
  liftIO $ print resultDeskCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate2 =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "saturn"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate2 <- (routes // Route.routeBooking // Route.Booking.routeDeskCreate) (DataJWT nextToken) requestDeskCreate2
  liftIO $ print resultDeskCreate2

  liftIO $ putStrLn "List desks."
  let requestDeskList =
        Route.Booking.MkRequestDeskList
          { Route.Booking.requestDeskListSpace = Name spacename
          }
  resultDeskList <- (routes // Route.routeBooking // Route.Booking.routeDeskList) (DataJWT nextToken) requestDeskList
  liftIO $ print resultDeskList

  pure ()
