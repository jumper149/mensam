module Mensam.Client.Debug where

import Mensam.API.Aeson
import Mensam.API.Data.User.Username
import Mensam.API.Route.Api.Booking qualified as Route.Booking
import Mensam.API.Route.Api.User qualified as Route.User
import Mensam.Client.Application
import Mensam.Client.Application.MensamClient.Class
import Mensam.Client.OrphanInstances

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.SOP
import Data.Text qualified as T
import Servant
import Servant.Client
import Servant.RawM.Client ()
import Text.Email.Parser
import Text.Email.Text

runF :: IO ()
runF = runApplicationT $ do
  result <- mensamCall f
  lift $ print result

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
  resultRegister <- endpointRegister requestRegister
  liftIO $ print resultRegister

  liftIO $ putStrLn "Profile."
  resultProfile <- endpointProfile name
  liftIO $ print resultProfile

  liftIO $ putStrLn "Login with BasicAuth."
  let credentials =
        MkCredentials
          { credentialsUsername = unUsername name
          , credentialsPassword = pw
          }
  resultLogin <- endpointLogin $ DataBasicAuth credentials
  liftIO $ print resultLogin
  responseLogin <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined

  liftIO $ putStrLn "Login with JWT."
  let token = MkJWToken {unJWToken = Route.User.responseLoginJWT responseLogin}
  nextLoginResult <- endpointLogin $ DataNextAuth $ DataJWT token
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
  resultSpaceCreate <- endpointSpaceCreate (DataJWT nextToken) requestSpaceCreate
  liftIO $ print resultSpaceCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "neptune"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate <- endpointDeskCreate (DataJWT nextToken) requestDeskCreate
  liftIO $ print resultDeskCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate2 =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = "saturn"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate2 <- endpointDeskCreate (DataJWT nextToken) requestDeskCreate2
  liftIO $ print resultDeskCreate2

  liftIO $ putStrLn "List desks."
  let requestDeskList =
        Route.Booking.MkRequestDeskList
          { Route.Booking.requestDeskListSpace = Name spacename
          , Route.Booking.requestDeskListTimeBegin = Nothing
          , Route.Booking.requestDeskListTimeEnd = Nothing
          }
  resultDeskList <- endpointDeskList (DataJWT nextToken) requestDeskList
  liftIO $ print resultDeskList

  pure ()
