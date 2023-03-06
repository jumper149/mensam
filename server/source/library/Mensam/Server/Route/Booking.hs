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
    { routeSpaceCreate = createNewSpace
    }

createNewSpace ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  AuthResult User ->
  Either String RequestSpaceCreate ->
  m (Union [WithStatus 200 (), WithStatus 400 ()])
createNewSpace authUser eitherRequest =
  case authUser of
    Authenticated user -> do
      request <- case eitherRequest of
        Left _err -> undefined
        Right request -> pure request
      logDebug $ "Received request to create space: " <> T.pack (show request)
      let space = MkSpace {spaceName = requestSpaceCreateName request}
      logDebug $ "Creating space: " <> T.pack (show space)
      spaceCreate space
      logInfo "Created space."
      logDebug $ "Setting user to access this space: " <> T.pack (show user)
      spaceAddUser (requestSpaceCreateName request) (userName user) True
      logInfo "Requesting user can now access the space."
      respond $ WithStatus @200 ()
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> undefined -- respond $ WithStatus @401 ()
        NoSuchUser -> undefined -- respond $ WithStatus @401 ()
        Indefinite -> respond $ WithStatus @400 ()
