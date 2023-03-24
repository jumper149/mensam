module Mensam.Client.Debug where

import Mensam.API.Aeson
import Mensam.API.Data.User.Username
import Mensam.API.Route qualified as Route
import Mensam.API.Route.Api qualified as Route.Api
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.OrphanInstances

import Control.Monad.IO.Class
import Data.Proxy
import Data.SOP
import Data.Text qualified as T
import Network.HTTP.Client qualified as Network
import Servant
import Servant.Client
import Servant.RawM.Client ()
import Text.Email.Parser
import Text.Email.Text

api :: Route.Api.Routes (AsClientT ClientM)
api = client (Proxy @(NamedRoutes Route.Routes)) // Route.routeApi

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
    email :: EmailAddress = fromTextUnsafe "maxmustermann@gmail.com"
    spacename :: T.Text = "solarsystem"

  liftIO $ putStrLn "Register."
  let requestRegister =
        Route.User.MkRequestRegister
          { Route.User.requestRegisterName = name
          , Route.User.requestRegisterPassword = pw
          , Route.User.requestRegisterEmail = email
          , Route.User.requestRegisterEmailVisible = True
          }
  resultRegister <- api // Route.Api.routeUser // Route.User.routeRegister $ requestRegister
  liftIO $ print resultRegister

  liftIO $ putStrLn "Profile."
  resultProfile <- api // Route.Api.routeUser // Route.User.routeProfile $ name
  liftIO $ print resultProfile

  liftIO $ putStrLn "Login with BasicAuth."
  let credentials =
        MkCredentials
          { credentialsUsername = unUsername name
          , credentialsPassword = pw
          }
  resultLogin <- api // Route.Api.routeUser // Route.User.routeLogin $ DataBasicAuth credentials
  liftIO $ print resultLogin
  responseLogin <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined

  liftIO $ putStrLn "Login with JWT."
  let token = MkJWToken {unJWToken = Route.User.responseLoginJWT responseLogin}
  nextLoginResult <- api // Route.Api.routeUser // Route.User.routeLogin $ DataNextAuth $ DataJWT token
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
  resultSpaceCreate <- (api // Route.Api.routeBooking // Route.Booking.routeSpaceCreate) (DataJWT nextToken) requestSpaceCreate
  liftIO $ print resultSpaceCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "neptune"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate <- (api // Route.Api.routeBooking // Route.Booking.routeDeskCreate) (DataJWT nextToken) requestDeskCreate
  liftIO $ print resultDeskCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate2 =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "saturn"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate2 <- (api // Route.Api.routeBooking // Route.Booking.routeDeskCreate) (DataJWT nextToken) requestDeskCreate2
  liftIO $ print resultDeskCreate2

  liftIO $ putStrLn "List desks."
  let requestDeskList =
        Route.Booking.MkRequestDeskList
          { Route.Booking.requestDeskListSpace = Name spacename
          }
  resultDeskList <- (api // Route.Api.routeBooking // Route.Booking.routeDeskList) (DataJWT nextToken) requestDeskList
  liftIO $ print resultDeskList

  pure ()
