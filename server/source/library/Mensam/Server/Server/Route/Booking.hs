module Mensam.Server.Server.Route.Booking where

import Mensam.API.Route.Booking.Type
import Mensam.API.User
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Booking
import Mensam.Server.Server.Auth

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
    , routeSpaceList = listSpaces
    , routeDeskCreate = createDesk
    , routeDeskList = listDesks
    }

createSpace ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ()) responses
  , IsMember (WithStatus 400 ()) responses
  , IsMember (WithStatus 401 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult User ->
  Either String RequestSpaceCreate ->
  m (Union responses)
createSpace authUser eitherRequest =
  handleAuth authUser $ \user -> do
    request <- case eitherRequest of
      Left _err -> undefined
      Right request -> pure request
    logDebug $ "Received request to create space: " <> T.pack (show request)
    seldaResult <- runSeldaTransactionT $ do
      spaceCreate (requestSpaceCreateName request) (requestSpaceCreateVisible request)
      spaceUserAdd (Left $ requestSpaceCreateName request) (Right $ userId user) True
    case seldaResult of
      SeldaFailure _err -> do
        -- TODO: Here we can theoretically return a more accurate error
        logWarn "Failed to create space."
        respond $ WithStatus @500 ()
      SeldaSuccess () -> do
        logInfo "Created space."
        respond $ WithStatus @200 ()

listSpaces ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceList) responses
  , IsMember (WithStatus 400 ()) responses
  , IsMember (WithStatus 401 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult User ->
  Either String RequestSpaceList ->
  m (Union responses)
listSpaces authUser eitherRequest =
  handleAuth authUser $ \user -> do
    request <- case eitherRequest of
      Left _err -> undefined
      Right request -> pure request
    logDebug $ "Received request to list spaces: " <> T.pack (show request)
    seldaResult <- runSeldaTransactionT $ do
      spaceList (userId user)
    case seldaResult of
      SeldaFailure _err -> do
        -- TODO: Here we can theoretically return a more accurate error
        logWarn "Failed to list spaces."
        respond $ WithStatus @500 ()
      SeldaSuccess spaces -> do
        logInfo "Listed spaces."
        respond $ WithStatus @200 MkResponseSpaceList {responseSpaceListSpaces = spaces}

createDesk ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ()) responses
  , IsMember (WithStatus 400 ()) responses
  , IsMember (WithStatus 401 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult User ->
  Either String RequestDeskCreate ->
  m (Union responses)
createDesk authUser eitherRequest =
  handleAuth authUser $ \user -> do
    request <- case eitherRequest of
      Left _err -> undefined
      Right request -> pure request
    logDebug $ "Received request to create desk: " <> T.pack (show request)
    seldaResult <- runSeldaTransactionT $ do
      permission <- spaceUserLookup (requestDeskCreateSpace request) (Right $ userId user)
      case permission of
        Nothing -> error "No permission."
        Just False -> error "No admin permission."
        Just True -> deskCreate (requestDeskCreateName request) (requestDeskCreateSpace request)
    case seldaResult of
      SeldaFailure _err -> do
        -- TODO: Here we can theoretically return a more accurate error
        logWarn "Failed to create desk."
        respond $ WithStatus @500 ()
      SeldaSuccess () -> do
        logInfo "Created desk."
        respond $ WithStatus @200 ()

listDesks ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskList) responses
  , IsMember (WithStatus 400 ()) responses
  , IsMember (WithStatus 401 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult User ->
  Either String RequestDeskList ->
  m (Union responses)
listDesks authUser eitherRequest =
  handleAuth authUser $ \user -> do
    request <- case eitherRequest of
      Left _err -> undefined
      Right request -> pure request
    logDebug $ "Received request to list desks: " <> T.pack (show request)
    seldaResult <- runSeldaTransactionT $ do
      spaceIdentifier <-
        case requestDeskListSpace request of
          Left spaceName ->
            spaceLookupId spaceName >>= \case
              Nothing -> undefined
              Just spaceId -> pure spaceId
          Right spaceId -> pure spaceId
      deskList spaceIdentifier (userId user)
    case seldaResult of
      SeldaFailure _err -> do
        -- TODO: Here we can theoretically return a more accurate error
        logWarn "Failed to list desks."
        respond $ WithStatus @500 ()
      SeldaSuccess desks -> do
        logInfo "Listed desks."
        respond $ WithStatus @200 MkResponseDeskList {responseDeskListDesks = desks}
