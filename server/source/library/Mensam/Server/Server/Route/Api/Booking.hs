module Mensam.Server.Server.Route.Api.Booking where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Route.Api.Booking
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Booking
import Mensam.Server.Server.Auth

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Set qualified as S
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
    , routeSpaceJoin = joinSpace
    , routeSpaceList = listSpaces
    , routeDeskCreate = createDesk
    , routeDeskList = listDesks
    , routeReservationCreate = createReservation
    , routeReservationCancel = cancelReservation
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
        spaceIdentifier <- spaceCreate (requestSpaceCreateName request) (requestSpaceCreateVisibility request) (requestSpaceCreateAccessibility request)
        spaceUserPermissionGive spaceIdentifier (userAuthenticatedId authenticated) MkPermissionSpaceUserEditDesk
        spaceUserPermissionGive spaceIdentifier (userAuthenticatedId authenticated) MkPermissionSpaceUserCreateReservation
        spaceUserPermissionGive spaceIdentifier (userAuthenticatedId authenticated) MkPermissionSpaceUserCancelReservation
        pure spaceIdentifier
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to create space."
          respond $ WithStatus @500 ()
        SeldaSuccess spaceIdentifier -> do
          logInfo "Created space."
          respond $ WithStatus @201 MkResponseSpaceCreate {responseSpaceCreateId = spaceIdentifier}

joinSpace ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceJoin) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceJoin ->
  m (Union responses)
joinSpace auth eitherRequest =
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to join space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestSpaceJoinSpace request of
            Identifier spaceId -> pure spaceId
            Name name ->
              spaceLookupId name >>= \case
                Just identifier -> pure identifier
                Nothing -> do
                  let msg :: T.Text = "No matching space."
                  lift $ logWarn msg
                  throwM $ Selda.SqlError $ show msg
        spaceUserPermissionGive spaceIdentifier (userAuthenticatedId authenticated) MkPermissionSpaceUserCreateReservation
        spaceUserPermissionGive spaceIdentifier (userAuthenticatedId authenticated) MkPermissionSpaceUserCancelReservation
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to create space."
          respond $ WithStatus @500 ()
        SeldaSuccess () -> do
          logInfo "Created space."
          respond $ WithStatus @200 MkResponseSpaceJoin {responseSpaceJoinUnit = ()}

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
        permissions <- spaceUserPermissions spaceIdentifier (userAuthenticatedId authenticated)
        if MkPermissionSpaceUserEditDesk `S.member` permissions
          then deskCreate (requestDeskCreateName request) spaceIdentifier
          else error "No permission"
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
        permissions <- spaceUserPermissions (deskSpace desk) (userAuthenticatedId authenticated)
        if MkPermissionSpaceUserCreateReservation `S.member` permissions
          then
            reservationCreate
              deskIdentifier
              (userAuthenticatedId authenticated)
              (requestReservationCreateTimeWindow request)
          else error "No permission"
      case seldaResult of
        SeldaFailure err -> do
          logWarn "Failed to create reservation."
          case fromException err of
            Just MkSqlErrorMensamDeskAlreadyReserved ->
              respond $ WithStatus @409 $ MkStaticText @"Desk is not available within the given time window."
            Nothing -> do
              -- TODO: Here we can theoretically return a more accurate error
              respond $ WithStatus @500 ()
        SeldaSuccess reservationIdentifier -> do
          logInfo "Created reservation."
          respond $ WithStatus @201 MkResponseReservationCreate {responseReservationCreateId = reservationIdentifier}

cancelReservation ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseReservationCancel) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestReservationCancel ->
  m (Union responses)
cancelReservation auth eitherRequest = do
  handleAuth auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to cancel reservation: " <> T.pack (show request)
      let reservationIdentifier = requestReservationCancelId request
      seldaResult <- runSeldaTransactionT $ do
        reservation <- reservationGet reservationIdentifier
        permissions <- do
          desk <- deskGet $ reservationDesk reservation
          let spaceIdentifier :: IdentifierSpace = deskSpace desk
          spaceUserPermissions spaceIdentifier $ userAuthenticatedId authenticated
        if MkPermissionSpaceUserCancelReservation `S.member` permissions
          then case reservationStatus reservation of
            MkStatusReservationCancelled -> error "Already cancelled."
            MkStatusReservationPlanned -> reservationCancel reservationIdentifier
          else error "No permission."
      case seldaResult of
        SeldaFailure _err -> do
          logWarn "Failed to cancel reservation."
          respond $ WithStatus @500 ()
        SeldaSuccess () -> do
          logInfo "Cancelled reservation."
          respond $ WithStatus @200 MkResponseReservationCancel {responseReservationCancelUnit = ()}

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
