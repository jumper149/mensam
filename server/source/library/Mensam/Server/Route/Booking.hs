module Mensam.Server.Route.Booking where

import Mensam.Application.SeldaPool.Class
import Mensam.Booking
import Mensam.Server.Route.Booking.Type
import Mensam.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeSpaceCreate = createSpace
    , routeDeskCreate = createDesk
    }

createSpace ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  AuthResult User ->
  Either String RequestSpaceCreate ->
  m (Union [WithStatus 200 (), WithStatus 400 ()])
createSpace authUser eitherRequest =
  case authUser of
    Authenticated user -> do
      request <- case eitherRequest of
        Left _err -> undefined
        Right request -> pure request
      logDebug $ "Received request to create space: " <> T.pack (show request)
      runSeldaTransactionT $ do
        spaceCreate $ requestSpaceCreateName request
        spaceUserAdd (Left $ requestSpaceCreateName request) (Left $ userName user) True
      respond $ WithStatus @200 ()
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> undefined -- respond $ WithStatus @401 ()
        NoSuchUser -> undefined -- respond $ WithStatus @401 ()
        Indefinite -> respond $ WithStatus @400 ()

createDesk ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  AuthResult User ->
  Either String RequestDeskCreate ->
  m (Union [WithStatus 200 (), WithStatus 400 ()])
createDesk authUser eitherRequest =
  case authUser of
    Authenticated user -> do
      request <- case eitherRequest of
        Left _err -> undefined
        Right request -> pure request
      logDebug $ "Received request to create desk: " <> T.pack (show request)
      -- TODO: Check if user has permission to create this desk.
      runSeldaTransactionT $
        deskCreate (requestDeskCreateName request) (requestDeskCreateSpaceName request)
      respond $ WithStatus @200 ()
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> undefined -- respond $ WithStatus @401 ()
        NoSuchUser -> undefined -- respond $ WithStatus @401 ()
        Indefinite -> respond $ WithStatus @400 ()
