module Mensam.Server.Server.Route.Api.Space where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.Desk
import Mensam.API.Data.Space
import Mensam.API.Data.Space.Permission
import Mensam.API.Data.User
import Mensam.API.Route.Api.Space
import Mensam.API.Update
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Configuration
import Mensam.Server.Jpeg
import Mensam.Server.Reservation
import Mensam.Server.Server.Auth
import Mensam.Server.Space

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.ByteString qualified as B
import Data.Foldable
import Data.Password.Bcrypt
import Data.SOP qualified as SOP
import Data.Text qualified as T
import Data.Traversable
import Data.Typeable
import Database.Selda qualified as Selda
import Servant hiding (BasicAuthResult (..))
import Servant.API.ImageJpeg
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadConfigured m, MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeSpaceCreate = createSpace
    , routeSpaceDelete = deleteSpace
    , routeSpaceEdit = editSpace
    , routePictureUpload = pictureUpload
    , routePictureDelete = pictureDelete
    , routePictureDownload = pictureDownload
    , routeSpaceJoin = joinSpace
    , routeSpaceLeave = leaveSpace
    , routeSpaceKick = kickUser
    , routeSpaceUserRole = setUserRole
    , routeSpaceList = listSpaces
    , routeSpaceView = viewSpace
    , routeRoleCreate = createRole
    , routeRoleEdit = editRole
    , routeRoleDelete = deleteRole
    , routeDeskCreate = createDesk
    , routeDeskDelete = deleteDesk
    , routeDeskEdit = editDesk
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
          roleIdentifier <- roleCreate spaceIdentifier (MkNameRole "Admin") MkAccessibilityRoleInaccessible Nothing
          rolePermissionGive roleIdentifier MkPermissionViewSpace
          rolePermissionGive roleIdentifier MkPermissionEditDesk
          rolePermissionGive roleIdentifier MkPermissionEditUser
          rolePermissionGive roleIdentifier MkPermissionEditRole
          rolePermissionGive roleIdentifier MkPermissionEditSpace
          rolePermissionGive roleIdentifier MkPermissionCreateReservation
          rolePermissionGive roleIdentifier MkPermissionCancelReservation
          spaceUserAdd spaceIdentifier (userAuthenticatedId authenticated) roleIdentifier

        do
          lift $ logInfo "Create member role."
          roleIdentifier <- roleCreate spaceIdentifier (MkNameRole "Member") MkAccessibilityRoleJoinable Nothing
          rolePermissionGive roleIdentifier MkPermissionViewSpace
          rolePermissionGive roleIdentifier MkPermissionCreateReservation
          rolePermissionGive roleIdentifier MkPermissionCancelReservation

        pure spaceIdentifier
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \spaceIdentifier ->
        respond $ WithStatus @201 MkResponseSpaceCreate {responseSpaceCreateId = spaceIdentifier}

deleteSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceDelete) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditSpace)) responses
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
        checkPermission
          SMkPermissionEditSpace
          (userAuthenticatedId authenticated)
          (requestSpaceDeleteId request)
        lift $ logInfo "Delete space."
        spaceDelete $ requestSpaceDeleteId request
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditSpace)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
                logInfo "Deleted space."
                respond $ WithStatus @200 MkResponseSpaceDelete {responseSpaceDeleteUnit = ()}

editSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceEdit) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditSpace)) responses
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
        checkPermission
          SMkPermissionEditSpace
          (userAuthenticatedId authenticated)
          (requestSpaceEditId request)
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
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditSpace)
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

pictureUpload ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 (StaticText "Uploaded space picture.")) responses
  , IsMember (WithStatus 400 ErrorParseBodyJpeg) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditSpace)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either T.Text IdentifierSpace ->
  Either String ImageJpegBytes ->
  m (Union responses)
pictureUpload auth eitherQueryParamIdentifierSpace eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBodyJpeg eitherRequest $ \request -> do
      case eitherQueryParamIdentifierSpace of
        Left _ -> do
          logInfo "Unable to parse space identifier."
          respond $ WithStatus @404 $ MkStaticText @"Space not found."
        Right identifierSpace -> do
          logInfo "Changing space picture."
          case jpegConvertSpacePicture request of
            Left err -> do
              logWarn $ "Failed to resize picture: " <> T.pack (show err)
              respond $ WithStatus @400 $ MkErrorParseBodyJpeg "Unable to read picture."
            Right picture -> do
              logInfo "Successfully verified and potentially resized picture."
              seldaResult <-
                runSeldaTransactionT $ do
                  checkPermission
                    SMkPermissionEditSpace
                    (userAuthenticatedId authenticated)
                    identifierSpace
                  spaceSetPicture identifierSpace (Just picture)
              handleSeldaException403InsufficientPermission
                (Proxy @MkPermissionEditSpace)
                seldaResult
                $ \seldaResultAfter403 ->
                  handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
                    logInfo "Changed space picture successfully."
                    respond $ WithStatus @200 $ MkStaticText @"Uploaded space picture."

pictureDelete ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 (StaticText "Deleted space picture.")) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditSpace)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either T.Text IdentifierSpace ->
  m (Union responses)
pictureDelete auth eitherQueryParamIdentifierSpace =
  handleAuthBearer auth $ \authenticated -> do
    case eitherQueryParamIdentifierSpace of
      Left _ -> do
        logInfo "Unable to parse space identifier."
        respond $ WithStatus @404 $ MkStaticText @"Space not found."
      Right identifierSpace -> do
        logInfo "Deleting space picture."
        seldaResult <-
          runSeldaTransactionT $ do
            checkPermission
              SMkPermissionEditSpace
              (userAuthenticatedId authenticated)
              identifierSpace
            spaceSetPicture identifierSpace Nothing
        handleSeldaException403InsufficientPermission
          (Proxy @MkPermissionEditSpace)
          seldaResult
          $ \seldaResultAfter403 ->
            handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
              logInfo "Deleted space picture successfully."
              respond $ WithStatus @200 $ MkStaticText @"Deleted space picture."

pictureDownload ::
  ( MonadConfigured m
  , MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  ) =>
  AuthResult UserAuthenticated ->
  Either T.Text IdentifierSpace ->
  m ImageJpegBytes
pictureDownload auth eitherQueryParamIdentifierSpace = do
  handledResult <- do
    handleAuthBearer auth $ \authenticated -> do
      case eitherQueryParamIdentifierSpace of
        Left _ -> do
          logInfo "Unable to parse space identifier."
          respond $ WithStatus @404 $ MkStaticText @"Space not found."
        Right identifierSpace -> do
          logDebug $ "Requesting a space picture from " <> T.pack (show identifierSpace)
          seldaResult <-
            runSeldaTransactionT $ do
              checkPermission
                SMkPermissionViewSpace
                (userAuthenticatedId authenticated)
                identifierSpace
              spaceGetPicture identifierSpace
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionViewSpace)
            seldaResult
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \maybePicture -> do
                logInfo "Checked out space picture successfully."
                case maybePicture of
                  Nothing -> do
                    logInfo "No picture set for this space. Returning default picture."
                    defaultProfilePictureFilePath <- (<> "/default-space-picture.jpeg") . configDirectoryStatic <$> configuration
                    picture <- liftIO $ B.readFile defaultProfilePictureFilePath
                    respond $ WithStatus @200 $ MkImageJpegBytes . B.fromStrict $ picture
                  Just picture -> do
                    logInfo "Answering with space picture."
                    respond $ WithStatus @200 $ MkImageJpegBytes . unByteStringJpeg $ picture
  logDebug $ "Handling multi-mimetype response manually: " <> T.pack (show handledResult) -- TODO: Logging pictures right now.
  -- TODO: Add all these HTTP statuses to the API definition. Requires different output types.
  case handledResult :: Union [WithStatus 200 ImageJpegBytes, WithStatus 401 ErrorBearerAuth, WithStatus 403 (ErrorInsufficientPermission MkPermissionViewSpace), WithStatus 404 (StaticText "Space not found."), WithStatus 500 ()] of
    SOP.Z (SOP.I (WithStatus result)) -> pure result
    SOP.S (SOP.Z (SOP.I (WithStatus errorBearerAuth))) -> error $ show errorBearerAuth
    SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus MkErrorInsufficientPermission)))) -> undefined
    SOP.S (SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus MkStaticText))))) -> undefined
    SOP.S (SOP.S (SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus ())))))) -> undefined
    SOP.S (SOP.S (SOP.S (SOP.S (SOP.S impossibleCase)))) -> case impossibleCase of {}

joinSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceJoin) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (StaticTexts ["Role is inaccessible.", "Wrong role password."])) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
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
            Identifier spaceId -> spaceGetFromId spaceId >> pure spaceId
            Name name -> spaceLookupId name
        roleIdentifier <-
          case requestSpaceJoinRole request of
            Identifier spaceId -> pure spaceId
            Name name ->
              roleLookupId spaceIdentifier name >>= \case
                Just identifier -> pure identifier
                Nothing -> do
                  let msg :: T.Text = "No matching space-role."
                  lift $ logWarn msg
                  throwM $ Selda.SqlError $ show msg
        role <- roleGet roleIdentifier
        case roleAccessibility role of
          MkAccessibilityRoleInaccessible -> do
            lift $ logInfo "Space-role is inaccessible. Cannot join."
            throwM MkSqlErrorMensamRoleInaccessible
          MkAccessibilityRoleJoinableWithPassword -> do
            lift $ logDebug "Space-role is joinable with password. Checking password."
            rolePasswordCheck' roleIdentifier (mkPassword <$> requestSpaceJoinPassword request)
          MkAccessibilityRoleJoinable -> do
            lift $ logDebug "Space-role is joinable. Joining."
        spaceUserAdd spaceIdentifier (userAuthenticatedId authenticated) roleIdentifier
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException
            (Proxy @SqlErrorMensamRoleInaccessible)
            (WithStatus @403 $ specificStaticText @["Role is inaccessible.", "Wrong role password."] $ MkStaticText @"Role is inaccessible.")
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaException
                (Proxy @SqlErrorMensamRolePasswordCheckFail)
                (WithStatus @403 $ specificStaticText @["Role is inaccessible.", "Wrong role password."] $ MkStaticText @"Wrong role password.")
                seldaResultAfter403
                $ \seldaResultAfter403' ->
                  handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403' $ \() -> do
                    logInfo "Joined space."
                    respond $ WithStatus @200 MkResponseSpaceJoin {responseSpaceJoinUnit = ()}

leaveSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceLeave) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (StaticText "Owner cannot leave space.")) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
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
            Identifier spaceId -> spaceGetFromId spaceId >> pure spaceId
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
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter404 $ \removed ->
            if removed
              then do
                logInfo "Left space."
                respond $ WithStatus @200 MkResponseSpaceLeave {responseSpaceLeaveUnit = ()}
              else do
                logInfo "Failed to leave space as owner."
                respond $ WithStatus @403 $ MkStaticText @"Owner cannot leave space."

kickUser ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceKick) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditUser)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceKick ->
  m (Union responses)
kickUser auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to kick user from space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        checkPermission
          SMkPermissionEditUser
          (userAuthenticatedId authenticated)
          (requestSpaceKickSpace request)
        isOwner <- spaceUserIsOwner (requestSpaceKickSpace request) (requestSpaceKickUser request)
        if isOwner
          then do
            lift $ logInfo "User is the owner of the space and can therefore not be kicked."
            pure False
          else do
            lift $ logInfo "Removing user from space."
            spaceUserRemove (requestSpaceKickSpace request) (requestSpaceKickUser request)
            pure True
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditUser)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \removed ->
                if removed
                  then do
                    logInfo "Kicked from space."
                    respond $ WithStatus @200 MkResponseSpaceKick {responseSpaceKickUnit = ()}
                  else do
                    logInfo "Failed to kick owner from space."
                    respond $ WithStatus @500 () -- TODO: Use HTTP 403

setUserRole ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceUserRole) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditUser)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestSpaceUserRole ->
  m (Union responses)
setUserRole auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to change user role for space: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        checkPermission
          SMkPermissionEditUser
          (userAuthenticatedId authenticated)
          (requestSpaceUserRoleSpace request)
        spaceUserRoleEdit
          (requestSpaceUserRoleSpace request)
          (requestSpaceUserRoleUser request)
          (requestSpaceUserRoleRole request)
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditUser)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
                logInfo "Set new role successfully."
                respond $ WithStatus @200 MkResponseSpaceUserRole {responseSpaceUserRoleUnit = ()}

viewSpace ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseSpaceView) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 ResponseSpaceView403) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
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
        _ <- spaceGetFromId $ requestSpaceViewId request -- Generate error when space doesn't exist
        spaceViewResult <- spaceView (userAuthenticatedId authenticated) (requestSpaceViewId request)
        let permissionCheck =
              checkPermission
                SMkPermissionViewSpace
                (userAuthenticatedId authenticated)
                (requestSpaceViewId request)
        catch (permissionCheck >> pure (Right spaceViewResult)) $ \case
          MkSqlErrorMensamPermissionNotSatisfied @MkPermissionViewSpace ->
            pure $ Left spaceViewResult
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter404 $ \case
            Left spaceViewResult -> do
              logInfo "User not permitted to view space fully. Creating reduced space view."
              respond $
                WithStatus @403 $
                  MkResponseSpaceView403
                    { responseSpaceView403Id = spaceViewId spaceViewResult
                    , responseSpaceView403Name = spaceViewName spaceViewResult
                    , responseSpaceView403Timezone = spaceViewTimezone spaceViewResult
                    , responseSpaceView403Visibility = spaceViewVisibility spaceViewResult
                    , responseSpaceView403Roles = spaceViewRoles spaceViewResult
                    , responseSpaceView403YourRole = spaceViewYourRole spaceViewResult
                    }
            Right spaceViewResult -> do
              logInfo "Viewed space."
              respond $
                WithStatus @200
                  MkResponseSpaceView
                    { responseSpaceViewId = spaceViewId spaceViewResult
                    , responseSpaceViewName = spaceViewName spaceViewResult
                    , responseSpaceViewTimezone = spaceViewTimezone spaceViewResult
                    , responseSpaceViewVisibility = spaceViewVisibility spaceViewResult
                    , responseSpaceViewOwner = spaceViewOwner spaceViewResult
                    , responseSpaceViewRoles = spaceViewRoles spaceViewResult
                    , responseSpaceViewUsers = spaceViewUsers spaceViewResult
                    , responseSpaceViewYourRole = spaceViewYourRole spaceViewResult
                    }

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
        spaces <- spaceListVisible (userAuthenticatedId authenticated) (requestSpaceListOrder request) (requestSpaceListMember request)
        for spaces $ \space -> do
          userCount <- spaceCountUsers $ spaceId space
          deskCount <- spaceCountDesks $ spaceId space
          pure
            MkSpaceListSpace
              { spaceListSpaceId = spaceId space
              , spaceListSpaceName = spaceName space
              , spaceListSpaceTimezone = spaceTimezone space
              , spaceListSpaceOwner = spaceOwner space
              , spaceListSpaceUsers = userCount
              , spaceListSpaceDesks = deskCount
              }
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
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditRole)) responses
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
        checkPermission
          SMkPermissionEditRole
          (userAuthenticatedId authenticated)
          (requestRoleCreateSpace request)
        roleId <-
          roleCreate
            (requestRoleCreateSpace request)
            (requestRoleCreateName request)
            (requestRoleCreateAccessibility request)
            (mkPassword <$> requestRoleCreatePassword request)
        traverse_ (rolePermissionGive roleId) (requestRoleCreatePermissions request)
        pure roleId
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditRole)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaException
                (Proxy @SqlErrorMensamRoleAccessibilityAndPasswordDontMatch)
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
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditRole)) responses
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
        spaceIdentifier <- roleSpace <$> roleGet (requestRoleEditId request)
        checkPermission
          SMkPermissionEditRole
          (userAuthenticatedId authenticated)
          spaceIdentifier
        case requestRoleEditName request of
          Preserve -> pure ()
          Overwrite name -> roleNameSet (requestRoleEditId request) name
        case requestRoleEditAccessibilityAndPassword request of
          Preserve -> pure ()
          Overwrite accessibilityAndPassword ->
            roleAccessibilityAndPasswordSet
              (requestRoleEditId request)
              (roleEditAccessibilityAndPasswordAccessibility accessibilityAndPassword)
              (mkPassword <$> roleEditAccessibilityAndPasswordPassword accessibilityAndPassword)
        case requestRoleEditPermissions request of
          Preserve -> pure ()
          Overwrite permissions -> rolePermissionsSet (requestRoleEditId request) permissions
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionEditRole)
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
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditRole)) responses
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
        spaceIdentifier <- roleSpace <$> roleGet (requestRoleDeleteId request)
        checkPermission
          SMkPermissionEditRole
          (userAuthenticatedId authenticated)
          spaceIdentifier
        roleDeleteWithFallback (requestRoleDeleteId request) (requestRoleDeleteFallbackId request)
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionEditRole)
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
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditDesk)) responses
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
        checkPermission
          SMkPermissionEditDesk
          (userAuthenticatedId authenticated)
          spaceIdentifier
        deskCreate (requestDeskCreateName request) spaceIdentifier (requestDeskCreateLocation request)
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditDesk)
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
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditDesk)) responses
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
        checkPermission
          SMkPermissionEditDesk
          (userAuthenticatedId authenticated)
          (deskSpace desk)
        deskDelete $ deskId desk
      handleSeldaException403InsufficientPermission
        (Proxy @MkPermissionEditDesk)
        seldaResult
        $ \seldaResultAfter403 ->
          handleSeldaException
            (Proxy @SqlErrorMensamDeskNotFound)
            (WithStatus @404 $ MkStaticText @"Desk not found.")
            seldaResultAfter403
            $ \seldaResultAfter404 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter404 $ \() ->
                respond $ WithStatus @200 MkResponseDeskDelete {responseDeskDeleteUnit = ()}

editDesk ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskEdit) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionEditDesk)) responses
  , IsMember (WithStatus 404 (StaticText "Desk not found.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestDeskEdit ->
  m (Union responses)
editDesk auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logDebug $ "Received request to edit desk: " <> T.pack (show request)
      seldaResult <- runSeldaTransactionT $ do
        desk <- deskGetFromId $ requestDeskEditId request
        checkPermission
          SMkPermissionEditDesk
          (userAuthenticatedId authenticated)
          (deskSpace desk)
        case requestDeskEditName request of
          Preserve -> pure ()
          Overwrite name -> deskNameSet (requestDeskEditId request) name
        case requestDeskEditLocation request of
          Preserve -> pure ()
          Overwrite location -> deskLocationSet (requestDeskEditId request) location
      handleSeldaException
        (Proxy @SqlErrorMensamDeskNotFound)
        (WithStatus @404 $ MkStaticText @"Desk not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionEditDesk)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \() -> do
                logInfo "Edited space."
                respond $
                  WithStatus @200
                    MkResponseDeskEdit {responseDeskEditUnit = ()}

listDesks ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseDeskList) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (ErrorInsufficientPermission MkPermissionViewSpace)) responses
  , IsMember (WithStatus 404 (StaticText "Space not found.")) responses
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
            Identifier spaceId -> spaceGetFromId spaceId >> pure spaceId
            Name spaceName -> spaceLookupId spaceName
        checkPermission
          SMkPermissionViewSpace
          (userAuthenticatedId authenticated)
          spaceIdentifier
        desks <- deskList spaceIdentifier (userAuthenticatedId authenticated)
        for desks $ \desk -> do
          reservations <-
            reservationList
              (deskId desk)
              (requestDeskListTimeWindow request)
          pure
            MkDeskWithInfo
              { deskWithInfoDesk = desk
              , deskWithInfoReservations = reservations
              }
      handleSeldaException
        (Proxy @SqlErrorMensamSpaceNotFound)
        (WithStatus @404 $ MkStaticText @"Space not found.")
        seldaResult
        $ \seldaResultAfter404 ->
          handleSeldaException403InsufficientPermission
            (Proxy @MkPermissionViewSpace)
            seldaResultAfter404
            $ \seldaResultAfter403 ->
              handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \desksWithInfo -> do
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

handleBadRequestBodyJpeg ::
  ( MonadLogger m
  , IsMember (WithStatus 400 ErrorParseBodyJpeg) responses
  ) =>
  Either String a ->
  (a -> m (Union responses)) ->
  m (Union responses)
handleBadRequestBodyJpeg parsedRequestBody handler' =
  -- TODO: Rename arguments `handler'` to `handler`
  case parsedRequestBody of
    Right a -> handler' a
    Left err -> respond $ WithStatus @400 $ MkErrorParseBodyJpeg err

handleSeldaException403InsufficientPermission ::
  forall (p :: Permission) m responses a.
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
    (Proxy @(SqlErrorMensamPermissionNotSatisfied p))
    (WithStatus @403 $ MkErrorInsufficientPermission @p)
