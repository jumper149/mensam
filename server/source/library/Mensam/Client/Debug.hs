module Mensam.Client.Debug where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
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
import Data.Time qualified as T
import Data.Time.Zones.All qualified as T
import Servant
import Servant.Client
import Text.Email.Parser
import Text.Email.Text

runF :: IO ()
runF = runApplicationT undefined $ do
  result <- mensamCall f
  lift $ print result

f :: ClientM ()
f = do
  let
    name :: Username = MkUsernameUnsafe "maxmustermann7"
    pw :: T.Text = "asdf"
    email :: EmailAddress = fromTextUnsafe "maxmustermann@gmail.com"
    spacename :: NameSpace = MkNameSpace "solarsystem"

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
  let token = Route.User.responseLoginJwt responseLogin
  nextLoginResult <- endpointLogin $ DataNextAuth $ DataJWTWithSession token
  liftIO $ print nextLoginResult
  nextResponseLogin <-
    case resultLogin of
      Z (I (WithStatus @200 x)) -> pure x
      _ -> undefined
  let nextToken = Route.User.responseLoginJwt nextResponseLogin

  liftIO $ putStrLn "Profile."
  let requestProfile =
        Route.User.MkRequestProfile
          { Route.User.requestProfileUser = Name name
          }
  resultProfile <- endpointProfile (DataJWTWithSession nextToken) requestProfile
  liftIO $ print resultProfile

  liftIO $ putStrLn "Create space."
  let requestSpaceCreate =
        Route.Booking.MkRequestSpaceCreate
          { Route.Booking.requestSpaceCreateName = spacename
          , Route.Booking.requestSpaceCreateTimezone = T.Europe__Paris
          , Route.Booking.requestSpaceCreateVisibility = MkVisibilitySpaceVisible
          }
  resultSpaceCreate <- endpointSpaceCreate (DataJWTWithSession nextToken) requestSpaceCreate
  liftIO $ print resultSpaceCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = MkNameDesk "neptune"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate <- endpointDeskCreate (DataJWTWithSession nextToken) requestDeskCreate
  liftIO $ print resultDeskCreate

  liftIO $ putStrLn "Create desk."
  let requestDeskCreate2 =
        Route.Booking.MkRequestDeskCreate
          { Route.Booking.requestDeskCreateName = MkNameDesk "saturn"
          , Route.Booking.requestDeskCreateSpace = Name spacename
          }
  resultDeskCreate2 <- endpointDeskCreate (DataJWTWithSession nextToken) requestDeskCreate2
  liftIO $ print resultDeskCreate2

  liftIO $ putStrLn "List desks."
  let requestDeskList =
        Route.Booking.MkRequestDeskList
          { Route.Booking.requestDeskListSpace = Name spacename
          , Route.Booking.requestDeskListTimeBegin = Nothing
          , Route.Booking.requestDeskListTimeEnd = Nothing
          }
  resultDeskList <- endpointDeskList (DataJWTWithSession nextToken) requestDeskList
  liftIO $ print resultDeskList

  liftIO $ putStrLn "Create reservation."
  currentTime <- liftIO T.getCurrentTime
  let requestReservationCreate =
        Route.Booking.MkRequestReservationCreate
          { Route.Booking.requestReservationCreateDesk = Name $ MkDeskNameWithContext (MkNameDesk "saturn") spacename
          , Route.Booking.requestReservationCreateTimeWindow =
              MkIntervalNonDegenerateUnsafe $
                MkIntervalUnsafe
                  currentTime
                  (T.secondsToNominalDiffTime (60 * 60) `T.addUTCTime` currentTime)
          }
  resultReservationCreate <- endpointReservationCreate (DataJWTWithSession nextToken) requestReservationCreate
  liftIO $ print resultReservationCreate

  pure ()
