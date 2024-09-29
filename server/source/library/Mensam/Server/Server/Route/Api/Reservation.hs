module Mensam.Server.Server.Route.Api.Reservation where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Route.Api.Reservation
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Reservation
import Mensam.Server.Server.Auth
import Mensam.Server.Space
import Mensam.Server.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Traversable
import Data.Typeable
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.Server
import Servant.Server.Generic
import Text.Blaze.Html.Renderer.Text qualified as T
import Text.Blaze.Html5 qualified as H

handler ::
  (MonadEmail m, MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeReservationCreate = createReservation
    , routeReservationCancel = cancelReservation
    , routeReservationList = listReservations
    }

createReservation ::
  ( MonadEmail m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseReservationCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceCreateReservation)) responses
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
        checkPermission
          SMkPermissionSpaceCreateReservation
          (userAuthenticatedId authenticated)
          (deskSpace desk)
        reservationIdentifier <-
          reservationCreate
            deskIdentifier
            (userAuthenticatedId authenticated)
            (requestReservationCreateTimeWindow request)
        user <- userGet $ userAuthenticatedId authenticated
        pure (reservationIdentifier, userEmail user)
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionSpaceCreateReservation)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaException
            (Proxy @SqlErrorMensamDeskAlreadyReserved)
            (WithStatus @409 $ MkStaticText @"Desk is not available within the given time window.")
            seldaResultAfter403
            $ \seldaResultAfter409 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter409 $ \(reservationIdentifier, emailAddress) -> do
                logInfo "Created reservation."
                logDebug "Sending notification email."
                sendEmailResult <-
                  sendEmail
                    MkEmail
                      { emailRecipient = emailAddress
                      , emailTitle = "Created Reservation: #" <> T.pack (show reservationIdentifier)
                      , emailBodyHtml = TL.toStrict $ T.renderHtml $ H.docTypeHtml $ do
                          H.head $ do
                            H.title $ H.text $ "Created Reservation: #" <> T.pack (show reservationIdentifier)
                          H.body $ do
                            H.p $ H.text "Your reservation was created successfully."
                      }
                let emailSent =
                      case sendEmailResult of
                        EmailSent -> True
                        EmailFailedToSend -> False
                respond $
                  WithStatus @201
                    MkResponseReservationCreate
                      { responseReservationCreateId = reservationIdentifier
                      , responseReservationEmailSent = emailSent
                      }

cancelReservation ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseReservationCancel) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceCancelReservation)) responses
  , IsMember (WithStatus 409 (StaticText "Already cancelled.")) responses
  , IsMember (WithStatus 410 (StaticText "Already happened.")) responses
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
        desk <- deskGetFromId $ reservationDesk reservation
        checkPermission
          SMkPermissionSpaceCancelReservation
          (userAuthenticatedId authenticated)
          (deskSpace desk)
        reservationCancel reservationIdentifier
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionSpaceCancelReservation)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaException
            (Proxy @SqlErrorMensamReservationAlreadyCancelled)
            (WithStatus @409 $ MkStaticText @"Already cancelled.")
            seldaResultAfter403
            $ \seldaResultAfter409 ->
              handleSeldaException
                (Proxy @SqlErrorMensamReservationIsInThePast)
                (WithStatus @410 $ MkStaticText @"Already happened.")
                seldaResultAfter409
                $ \seldaResultAfter410 ->
                  handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter410 $ \() -> do
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
        reservations <- reservationListUser (userAuthenticatedId authenticated) (requestReservationListTimeWindow request)
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
