module Mensam.Server.Server.Route.Api.Booking where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Data.User
import Mensam.API.Route.Api.Booking
import Mensam.API.Update
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Booking
import Mensam.Server.Server.Auth

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Password.Bcrypt
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Data.Typeable
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
    , routeSpaceDelete = deleteSpace
    , routeSpaceEdit = editSpace
    , routeSpaceJoin = joinSpace
    , routeSpaceLeave = leaveSpace
    , routeSpaceList = listSpaces
    , routeSpaceView = viewSpace
    , routeRoleCreate = createRole
    , routeRoleEdit = editRole
    , routeRoleDelete = deleteRole
    , routeDeskCreate = createDesk
    , routeDeskDelete = deleteDesk
    , routeDeskList = listDesks
    }

createSpace ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseSpaceCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceCreate ->
  m (Union responses)
createSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        lift $ logInfo "Create space."
        spaceIdentifier <- spaceCreate (requestSpaceCreateName request) (userAuthenticatedId authenticated) (requestSpaceCreateTimezone request) (requestSpaceCreateVisibility request)

        do
          lift $ logInfo "Create admin role and add user."
          spaceRoleIdentifier <- spaceRoleCreate spaceIdentifier (MkNameSpaceRole "Admin") MkAccessibilitySpaceRoleInaccessible Nothing
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceViewSpace
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceEditDesk
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceEditRole
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceEditSpace
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceCreateReservation
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceCancelReservation
          spaceUserAdd spaceIdentifier (userAuthenticatedId authenticated) spaceRoleIdentifier

        do
          lift $ logInfo "Create member role."
          spaceRoleIdentifier <- spaceRoleCreate spaceIdentifier (MkNameSpaceRole "Member") MkAccessibilitySpaceRoleJoinable Nothing
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceViewSpace
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceCreateReservation
          spaceRolePermissionGive spaceRoleIdentifier MkPermissionSpaceCancelReservation

        pure spaceIdentifier
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \spaceIdentifier ->
        respond $ WithStatus @201 MkResponseSpaceCreate {responseSpaceCreateId = spaceIdentifier}

deleteSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceDelete) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceDelete ->
  m (Union responses)
deleteSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to delete space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        isOwner <- spaceUserIsOwner (requestSpaceDeleteId request) (userAuthenticatedId authenticated)
        if isOwner
          then do
            lift $ logInfo "Delete space."
            spaceDelete $ requestSpaceDeleteId request
            pure $ Just ()
          else pure Nothing
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter404 $ \case
            Nothing -> do
              logInfo "Didn't delete space because of insufficient permission. User needs to the owner."
              respond $ WithStatus @403 $ MkErrorInsufficientPermission @MkPermissionSpaceEditSpace
            Just () -> do
              logInfo "Deleted space."
              respond $ WithStatus @200 MkResponseSpaceDelete {responseSpaceDeleteUnit = ()}

editSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceEdit) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditSpace)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceEdit ->
  m (Union responses)
editSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to edit space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        permissions <- spaceUserPermissions (requestSpaceEditId request) (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditSpace `S.member` permissions
          then do
            case requestSpaceEditName request of
              Preserve -> pure ()
              Overwrite name -> spaceNameSet (requestSpaceEditId request) name
            case requestSpaceEditTimezone request of
              Preserve -> pure ()
              Overwrite timezone -> spaceTimezoneSet (requestSpaceEditId request) timezone
            case requestSpaceEditVisibility request of
              Preserve -> pure ()
              Overwrite visibility -> spaceVisibilitySet (requestSpaceEditId request) visibility
            spaceInternalGetFromId (requestSpaceEditId request)
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditSpace
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionSpaceEditSpace)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \spaceInternal -> do
                logInfo "Edited space."
                respond $
                  WithStatus @200
                    MkResponseSpaceEdit
                      { responseSpaceEditId = spaceInternalId spaceInternal
                      , responseSpaceEditName = spaceInternalName spaceInternal
                      , responseSpaceEditTimezone = spaceInternalTimezone spaceInternal
                      , responseSpaceEditVisibility = spaceInternalVisibility spaceInternal
                      }

joinSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceJoin) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (StaticText "Wrong space password.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceJoin ->
  m (Union responses)
joinSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to join space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestSpaceJoinSpace request of
            Identifier spaceId -> pure spaceId
            Name name -> spaceLookupId name
        spaceRoleIdentifier <-
          case requestSpaceJoinRole request of
            Identifier spaceId -> pure spaceId
            Name name ->
              spaceRoleLookupId spaceIdentifier name >>= \case
                Just identifier -> pure identifier
                Nothing -> do
                  let msg :: T.Text = "No matching space-role."
                  lift $ logWarn msg
                  throwM $ Selda.SqlError $ show msg
        spaceRole <- spaceRoleGet spaceRoleIdentifier
        case spaceRoleAccessibility spaceRole of
          MkAccessibilitySpaceRoleInaccessible -> do
            let msg :: T.Text = "Space-role is inaccessible. Cannot join."
            lift $ logWarn msg
            throwM $ Selda.SqlError $ show msg
          MkAccessibilitySpaceRoleJoinableWithPassword -> do
            lift $ logDebug "Space-role is joinable with password. Checking password."
            spaceRolePasswordCheck' spaceRoleIdentifier (mkPassword <$> requestSpaceJoinPassword request)
          MkAccessibilitySpaceRoleJoinable -> do
            lift $ logDebug "Space-role is joinable. Joining."
        spaceUserAdd spaceIdentifier (userAuthenticatedId authenticated) spaceRoleIdentifier
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceRolePasswordCheckFail)
        (WithStatus @403 $ MkStaticText @"Wrong space password.")
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
            logInfo "Joined space."
            respond $ WithStatus @200 MkResponseSpaceJoin {responseSpaceJoinUnit = ()}

leaveSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceLeave) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (StaticText "Owner cannot leave space.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceLeave ->
  m (Union responses)
leaveSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to leave space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestSpaceLeaveSpace request of
            Identifier spaceId -> pure spaceId
            Name name -> spaceLookupId name
        isOwner <- spaceUserIsOwner spaceIdentifier (userAuthenticatedId authenticated)
        if isOwner
          then do
            lift $ logInfo "User is the owner of the space and can therefore not be removed."
            pure False
          else do
            lift $ logInfo "Removing user from space."
            spaceUserRemove spaceIdentifier (userAuthenticatedId authenticated)
            pure True
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \removed ->
        if removed
          then do
            logInfo "Left space."
            respond $ WithStatus @200 MkResponseSpaceLeave {responseSpaceLeaveUnit = ()}
          else do
            logInfo "Failed to leave space as owner."
            respond $ WithStatus @403 $ MkStaticText @"Owner cannot leave space."

viewSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceView) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceViewSpace)) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceView ->
  m (Union responses)
viewSpace auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to view space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceView (userAuthenticatedId authenticated) (requestSpaceViewId request)
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \case
        Nothing -> do
          logInfo "User not permitted to view space."
          respond $ WithStatus @403 $ MkErrorInsufficientPermission @MkPermissionSpaceViewSpace
        Just space -> do
          logInfo "Viewed space."
          respond $ WithStatus @200 MkResponseSpaceView {responseSpaceViewSpace = space}

listSpaces ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceList ->
  m (Union responses)
listSpaces auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to list spaces: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceListVisible (userAuthenticatedId authenticated) (requestSpaceListOrder request)
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \spaces -> do
        logInfo "Listed spaces."
        respond $ WithStatus @200 MkResponseSpaceList {responseSpaceListSpaces = spaces}

createRole ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseRoleCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestRoleCreate ->
  m (Union responses)
createRole auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create role: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        permissions <- spaceUserPermissions (requestRoleCreateSpace request) (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditRole `S.member` permissions
          then do
            spaceRoleId <-
              spaceRoleCreate
                (requestRoleCreateSpace request)
                (requestRoleCreateName request)
                (requestRoleCreateAccessibility request)
                (mkPassword <$> requestRoleCreatePassword request)
            traverse_ (spaceRolePermissionGive spaceRoleId) (requestRoleCreatePermissions request)
            pure spaceRoleId
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditRole
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionSpaceEditRole)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaException
                (Proxy @SqlErrorMensamSpaceRoleAccessibilityAndPasswordDontMatch)
                (WithStatus @400 $ MkErrorParseBodyJson {errorParseBodyJsonError = "accessibility and password don't match"})
                seldaResultAfter403
                $ \seldaResultAfter500 ->
                  handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter500 $ \roleIdentifier -> do
                    logInfo "Created role."
                    respond $ WithStatus @201 MkResponseRoleCreate {responseRoleCreateId = roleIdentifier}

editRole ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseRoleEdit) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestRoleEdit ->
  m (Union responses)
editRole auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to delete role: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <- spaceRoleSpace <$> spaceRoleGet (requestRoleEditId request)
        permissionsRequestingUser <- spaceUserPermissions spaceIdentifier (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditRole `S.member` permissionsRequestingUser
          then do
            case requestRoleEditName request of
              Preserve -> pure ()
              Overwrite name -> spaceRoleNameSet (requestRoleEditId request) name
            case requestRoleEditAccessibilityAndPassword request of
              Preserve -> pure ()
              Overwrite accessibilityAndPassword ->
                spaceRoleAccessibilityAndPasswordSet
                  (requestRoleEditId request)
                  (roleEditAccessibilityAndPasswordAccessibility accessibilityAndPassword)
                  (mkPassword <$> roleEditAccessibilityAndPasswordPassword accessibilityAndPassword)
            case requestRoleEditPermissions request of
              Preserve -> pure ()
              Overwrite permissions -> spaceRolePermissionsSet (requestRoleEditId request) permissions
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditRole
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionSpaceEditRole)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
            logInfo "Editeded role."
            respond $ WithStatus @200 MkResponseRoleEdit {responseRoleEditUnit = ()}

deleteRole ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseRoleDelete) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditRole)) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestRoleDelete ->
  m (Union responses)
deleteRole auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to delete role: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <- spaceRoleSpace <$> spaceRoleGet (requestRoleDeleteId request)
        permissions <- spaceUserPermissions spaceIdentifier (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditRole `S.member` permissions
          then spaceRoleDeleteWithFallback (requestRoleDeleteId request) (requestRoleDeleteFallbackId request)
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditRole
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionSpaceEditRole)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
            logInfo "Deleted role."
            respond $ WithStatus @200 MkResponseRoleDelete {responseRoleDeleteUnit = ()}

createDesk ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseDeskCreate) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskCreate ->
  m (Union responses)
createDesk auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to create desk: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestDeskCreateSpace request of
            Identifier spaceId -> pure spaceId
            Name name -> spaceLookupId name
        permissions <- spaceUserPermissions spaceIdentifier (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditDesk `S.member` permissions
          then deskCreate (requestDeskCreateName request) spaceIdentifier
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditDesk
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionSpaceEditDesk)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \deskIdentifier -> do
                logInfo "Created desk."
                respond $ WithStatus @201 MkResponseDeskCreate {responseDeskCreateId = deskIdentifier}

deleteDesk ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskDelete) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionSpaceEditDesk)) responses
  , IsMember (WithStatus 404 (StaticText "Desk not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskDelete ->
  m (Union responses)
deleteDesk auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to delete desk: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        desk <- deskGetFromId $ requestDeskDeleteId request
        permissions <- spaceUserPermissions (deskSpace desk) (userAuthenticatedId authenticated)
        if MkPermissionSpaceEditDesk `S.member` permissions
          then deskDelete $ deskId desk
          else throwM $ MkSqlErrorMensamSpacePermissionNotSatisfied @MkPermissionSpaceEditDesk
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionSpaceEditDesk)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaException
            (Proxy @SqlErrorMensamDeskNotFound)
            (WithStatus @404 $ MkStaticText @"Desk not found.")
            seldaResultAfter403
            $ \seldaResultAfter404 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter404 $ \() ->
                respond $ WithStatus @200 MkResponseDeskDelete {responseDeskDeleteUnit = ()}

listDesks ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskList ->
  m (Union responses)
listDesks auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to list desks: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        spaceIdentifier <-
          case requestDeskListSpace request of
            Name spaceName -> spaceLookupId spaceName
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
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \desksWithInfo -> do
        logInfo "Listed desks."
        respond $ WithStatus @200 MkResponseDeskList {responseDeskListDesks = desksWithInfo}

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
