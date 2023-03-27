module Mensam.Server.Server.Route.Api.Booking where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.User
import Mensam.API.Route.Api.Booking
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Booking
import Mensam.Server.Server.Auth

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Text qualified as T
import Data.Traversable
import Database.Selda qualified as Selda
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
    , routeReservationCreate = createReservation
    }

createSpace ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseSpaceCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceCreate ->
  m (Union responses)
createSpace auth eitherRequest =
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <- spaceCreate (requestSpaceCreateName request) (requestSpaceCreateVisibility request)
        spaceUserAdd spaceIdentifier (userAuthenticatedId authenticated) True
        pure spaceIdentifier
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to create space."
          respond $ WithStatus @500 ()
        SeldaSuccess spaceIdentifier -> do
          logInfo "Created space."
          respond $ WithStatus @201 MkResponseSpaceCreate {responseSpaceCreateId = spaceIdentifier}

listSpaces ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceList ->
  m (Union responses)
listSpaces auth eitherRequest =
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to list spaces: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceList (userAuthenticatedId authenticated) (requestSpaceListOrder request)
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
  , IsMember (WithStatus 201 ResponseDeskCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskCreate ->
  m (Union responses)
createDesk auth eitherRequest =
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create desk: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestDeskCreateSpace request of
            Identifier spaceId -> pure spaceId
            Name name ->
              spaceLookupId name >>= \case
                Just identifier -> pure identifier
                Nothing -> do
                  let msg :: T.Text = "No matching space."
                  lift $ logWarn msg
                  throwM $ Selda.SqlError $ show msg
        permission <- spaceUserLookup spaceIdentifier (userAuthenticatedId authenticated)
        case permission of
          Nothing -> error "No permission."
          Just False -> error "No admin permission."
          Just True -> deskCreate (requestDeskCreateName request) spaceIdentifier
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to create desk."
          respond $ WithStatus @500 ()
        SeldaSuccess deskIdentifier -> do
          logInfo "Created desk."
          respond $ WithStatus @201 MkResponseDeskCreate {responseDeskCreateId = deskIdentifier}

listDesks ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskList ->
  m (Union responses)
listDesks auth eitherRequest =
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to list desks: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestDeskListSpace request of
            Name spaceName ->
              spaceLookupId spaceName >>= \case
                Nothing -> undefined
                Just spaceId -> pure spaceId
            Identifier spaceId -> pure spaceId
        desks <- deskList spaceIdentifier (userAuthenticatedId authenticated)
        for desks $ \desk -> do
          reservations <-
            reservationList
              (deskId desk)
              (requestDeskListTimeBegin request)
              (requestDeskListTimeEnd request)
              Nothing
          pure
            MkDeskWithInfo
              { deskWithInfoDesk = desk
              , deskWithInfoReservations = reservations
              }
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to list desks."
          respond $ WithStatus @500 ()
        SeldaSuccess desksWithInfo -> do
          logInfo "Listed desks."
          respond $ WithStatus @200 MkResponseDeskList {responseDeskListDesks = desksWithInfo}

createReservation ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseReservationCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 409 (StaticText "Desk is not available within the given time window.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestReservationCreate ->
  m (Union responses)
createReservation auth eitherRequest = do
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create reservation: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        deskIdentifier <-
          case requestReservationCreateDesk request of
            Name (spaceName, deskName) -> do
              spaceIdentifier <-
                spaceLookupId spaceName >>= \case
                  Nothing -> undefined
                  Just spaceId -> pure spaceId
              deskLookupId spaceIdentifier deskName >>= \case
                Nothing -> undefined
                Just deskId -> pure deskId
            Identifier deskId -> pure deskId
        desk <- deskGet deskIdentifier
        permission <- spaceUserLookup (deskSpace desk) (userAuthenticatedId authenticated)
        case permission of
          Nothing -> error "No permission."
          Just _ -> do
            reservationCreate
              deskIdentifier
              (userAuthenticatedId authenticated)
              (requestReservationCreateTimeBegin request)
              (requestReservationCreateTimeEnd request)
      case seldaResult of
        SeldaFailure someException -> do
          logWarn "Failed to create reservation."
          case fromException someException of
            Just MkSqlErrorMensamDeskAlreadyReserved ->
              respond $ WithStatus @409 $ MkStaticText @"Desk is not available within the given time window."
            Nothing -> do
              -- TODO: Here we can theoretically return a more accurate error
              respond $ WithStatus @500 ()
        SeldaSuccess reservationIdentifier -> do
          logInfo "Created reservation."
          respond $ WithStatus @201 MkResponseReservationCreate {responseReservationCreateId = reservationIdentifier}

handleBadRequestBody ::
  ( MonadLogger m
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  ) =>
  Either String a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleBadRequestBody parsedRequestBody handler' =
  -- TODO: Rename arguments `handler'` to `handler`
  case parsedRequestBody of
    Right a -> handler' a
    Left err -> respond $ WithStatus @400 $ MkErrorParseBodyJson err
