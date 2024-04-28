module Mensam.Server.Server.Route.Api.Reservation where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Route.Api.Reservation
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Booking
import Mensam.Server.Server.Auth

import Control.Monad.Logger.CallStack
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Data.Typeable
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadLogger m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeReservationCreate = createReservation
    , routeReservationCancel = cancelReservation
    , routeReservationList = listReservations
    }

createReservation ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseReservationCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 409 (StaticText "Desk is not available within the given time window.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestReservationCreate ->
  m (Union responses)
createReservation auth eitherRequest = do
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create reservation: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        deskIdentifier <-
          case requestReservationCreateDesk request of
            Name MkDeskNameWithContext {deskNameWithContextSpace = spaceName, deskNameWithContextDesk = deskName} -> do
              spaceIdentifier <- spaceLookupId spaceName
              deskLookupId spaceIdentifier deskName >>= \case
                Nothing -> undefined
                Just deskId -> pure deskId
            Identifier deskId -> pure deskId
        desk <- deskGetFromId deskIdentifier
        permissions <- spaceUserPermissions (deskSpace desk) (userAuthenticatedId authenticated)
        if MkPermissionSpaceCreateReservation `S.member` permissions
          then
            reservationCreate
              deskIdentifier
              (userAuthenticatedId authenticated)
              (requestReservationCreateTimeWindow request)
          else error "No permission"
      handleSeldaException
        (Proxy @SqlErrorMensamDeskAlreadyReserved)
        (WithStatus @409 $ MkStaticText @"Desk is not available within the given time window.")
        seldaResult
        $ \seldaResultAfter409 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter409 $ \reservationIdentifier -> do
            logInfo "Created reservation."
            respond $ WithStatus @201 MkResponseReservationCreate {responseReservationCreateId = reservationIdentifier}

cancelReservation ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseReservationCancel) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestReservationCancel ->
  m (Union responses)
cancelReservation auth eitherRequest = do
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to cancel reservation: " <> T.pack (show request)
      let reservationIdentifier = requestReservationCancelId request
      seldaResult <- runSeldaTransactionT $ do
        reservation <- reservationGet reservationIdentifier
        permissions <- do
          desk <- deskGetFromId $ reservationDesk reservation
          let spaceIdentifier :: IdentifierSpace = deskSpace desk
          spaceUserPermissions spaceIdentifier $ userAuthenticatedId authenticated
        if MkPermissionSpaceCancelReservation `S.member` permissions
          then case reservationStatus reservation of
            MkStatusReservationCancelled -> error "Already cancelled."
            MkStatusReservationPlanned -> reservationCancel reservationIdentifier
          else error "No permission."
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
        logInfo "Cancelled reservation."
        respond $ WithStatus @200 MkResponseReservationCancel {responseReservationCancelUnit = ()}

listReservations ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseReservationList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestReservationList ->
  m (Union responses)
listReservations auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to list a user's reservations: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        reservations <- reservationListUser (userAuthenticatedId authenticated) (requestReservationListTimeBegin request) (requestReservationListTimeEnd request)
        for reservations $ \reservation -> do
          desk <- deskGetFromId $ reservationDesk reservation
          space <- spaceGetFromId $ deskSpace desk
          pure
            MkReservationWithInfo
              { reservationWithInfoReservation = reservation
              , reservationWithInfoDesk = desk
              , reservationWithInfoSpace = space
              }
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \reservationsWithInfo -> do
        logInfo "Listed user's reservations."
        respond $ WithStatus @200 MkResponseReservationList {responseReservationListReservations = reservationsWithInfo}

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

handleSeldaException403InsufficientPermission ::
  forall (p :: PermissionSpace) m responses a.
  ( Typeable p
  , Applicative m
  , IsMember (WithStatus 403 (ErrorInsufficientPermission p)) responses
  ) =>
  Proxy p ->
  SeldaResult a ->
  (SeldaResult a -> m (Union responses)) ->
  m (Union responses)
handleSeldaException403InsufficientPermission Proxy =
  handleSeldaException
    (Proxy @(SqlErrorMensamSpacePermissionNotSatisfied p))
    (WithStatus @403 $ MkErrorInsufficientPermission @p)
