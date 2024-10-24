module Mensam.Server.Server.Route.Api.User where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.User
import Mensam.API.Data.User.Password
import Mensam.API.Data.User.Username
import Mensam.API.Pretty
import Mensam.API.Route.Api.User
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Configuration
import Mensam.Server.Configuration.BaseUrl
import Mensam.Server.Jpeg
import Mensam.Server.Secrets
import Mensam.Server.Server.Auth
import Mensam.Server.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.ByteString qualified as B
import Data.Password.Bcrypt qualified as Bcrypt
import Data.SOP qualified as SOP
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time qualified as T
import Servant hiding (BasicAuthResult (..))
import Servant.API.ImageJpeg
import Servant.Auth.Server
import Servant.Server.Generic
import Text.Blaze.Html.Renderer.Text qualified as T
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as H.A

handler ::
  (MonadConfigured m, MonadEmail m, MonadIO m, MonadLogger m, MonadSecret m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
    , routeLogout = logout
    , routeRegister = register
    , routePasswordChange = passwordChange
    , routePictureUpload = pictureUpload
    , routePictureDelete = pictureDelete
    , routePictureDownload = pictureDownload
    , routeConfirmationRequest = confirmationRequest
    , routeConfirm = confirm
    , routeNotificationPreferences = notificationPreferences
    , routeProfile = profile
    }

login ::
  ( MonadConfigured m
  , MonadIO m
  , MonadLogger m
  , MonadSecret m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseLogin) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  m (Union responses)
login auth =
  handleAuthBasic auth $ \authenticatedWithoutSession -> do
    logInfo "Logging in user."
    timeCurrent <- liftIO T.getCurrentTime
    maybeTimeout <- do
      durationValid <- authTimeoutSeconds . configAuth <$> configuration
      let maybeTimeExpiration =
            (`T.addUTCTime` timeCurrent)
              <$> (T.secondsToNominalDiffTime . fromInteger <$> durationValid)
      pure maybeTimeExpiration
    logDebug "Creating session."
    seldaResult <-
      runSeldaTransactionT $
        userSessionCreate (userAuthenticatedId authenticatedWithoutSession) timeCurrent maybeTimeout
    handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \sessionIdentifier -> do
      logInfo "Created session successfully."
      let authenticatedWithSession = authenticatedWithoutSession {userAuthenticatedSession = Just sessionIdentifier}
      logDebug $ "Creating JWT for user: " <> T.pack (show authenticatedWithSession)
      logDebug $ "JWT timeout has been set: " <> T.pack (show maybeTimeout)
      eitherJwt <- do
        jwk <- secretsJwk <$> secrets
        let jwtSettings = mkJwtSettings jwk
        liftIO $ makeJWT authenticatedWithSession jwtSettings maybeTimeout
      case eitherJwt of
        Left err -> do
          logError $ "Failed to create JWT: " <> T.pack (show err)
          respond $ WithStatus @500 ()
        Right jwtByteString ->
          case T.decodeUtf8' $ B.toStrict jwtByteString of
            Left err -> do
              logError $ "Failed to decode JWT as UTF-8: " <> T.pack (show err)
              respond $ WithStatus @500 ()
            Right jwtText -> do
              let jwt = MkJwt jwtText
              logInfo "Created JWT successfully."
              logInfo "User login successful."
              respond $
                WithStatus @200
                  MkResponseLogin
                    { responseLoginJwt = jwt
                    , responseLoginExpiration = maybeTimeout
                    , responseLoginId = userAuthenticatedId authenticatedWithSession
                    }

logout ::
  ( MonadConfigured m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseLogout) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  m (Union responses)
logout auth =
  handleAuthBearer auth $ \authenticated -> do
    logInfo "Logging out user."
    case userAuthenticatedSession authenticated of
      Nothing -> do
        logWarn "Tried to logout even though there is no session associated with this authentication."
        respond $ WithStatus @500 ()
      Just sessionIdentifier -> do
        logDebug $ "Deleting session: " <> T.pack (show sessionIdentifier)
        seldaResult <-
          runSeldaTransactionT $
            userSessionDelete sessionIdentifier
        handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
          logInfo "User logged out successfully."
          respond $
            WithStatus @200
              MkResponseLogout
                { responseLogoutUnit = ()
                }

register ::
  ( MonadConfigured m
  , MonadEmail m
  , MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ResponseRegister) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 409 (StaticText "Username is taken.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  Either String RequestRegister ->
  m (Union responses)
register eitherRequest =
  case eitherRequest of
    Left err -> do
      logInfo $ "Failed to parse request: " <> T.pack (show err)
      respond $ WithStatus @400 $ MkErrorParseBodyJson err
    Right request@MkRequestRegister {requestRegisterName, requestRegisterPassword, requestRegisterEmail, requestRegisterEmailVisible, requestRegisterEmailNotifications} -> do
      logDebug $ "Registering new user: " <> T.pack (show request)
      seldaResult <-
        runSeldaTransactionT $ do
          userIdentifier <-
            userCreate
              requestRegisterName
              (Bcrypt.mkPassword $ unPassword requestRegisterPassword)
              requestRegisterEmail
              requestRegisterEmailVisible
              requestRegisterEmailNotifications
          let effect = MkConfirmationEffectEmailValidation requestRegisterEmail
          expirationTime <- lift . liftIO $ (T.secondsToNominalDiffTime (60 * 60) `T.addUTCTime`) <$> T.getCurrentTime
          userConfirmationCreate userIdentifier effect expirationTime
      handleSeldaException
        (Proxy @SqlErrorMensamUsernameIsTaken)
        (WithStatus @409 $ MkStaticText @"Username is taken.")
        seldaResult
        $ \seldaResultAfter409 -> do
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter409 $ \confirmationSecret -> do
            logInfo "Registered new user."
            logDebug "Sending confirmation email."
            config <- configuration
            sendEmailResult <-
              sendEmail
                MkEmail
                  { emailRecipient = requestRegisterEmail
                  , emailTitle = "Account Verification: " <> unUsername requestRegisterName
                  , emailBodyHtml = TL.toStrict $ T.renderHtml $ H.docTypeHtml $ do
                      H.head $ do
                        H.title $ H.text $ "Account Verification: " <> unUsername requestRegisterName
                      H.body $ do
                        H.p $ H.text $ "Welcome " <> unUsername requestRegisterName <> "!"
                        H.p $ H.text "You have been registered successfully. Click the link to confirm your email address."
                        let confirmLink :: T.Text = displayBaseUrl (configBaseUrl config) <> "register/confirm/" <> unConfirmationSecret confirmationSecret
                        H.p $ H.a H.! H.A.href (H.textValue confirmLink) $ H.text confirmLink
                        H.div $ H.small $ H.text "If you did not register this account, feel free to ignore this message."
                  }
            let emailSent =
                  case sendEmailResult of
                    EmailSent -> True
                    EmailFailedToSend -> False
            respond $
              WithStatus @201
                MkResponseRegister
                  { responseRegisterEmailSent = emailSent
                  }

passwordChange ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponsePasswordChange) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestPasswordChange ->
  m (Union responses)
passwordChange auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logInfo "Changing user password."
      seldaResult <-
        runSeldaTransactionT $
          userSetPassword
            (userAuthenticatedId authenticated)
            (Bcrypt.mkPassword $ unPassword $ requestPasswordChangeNewPassword request)
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
        logInfo "Changed user password successfully."
        respond $ WithStatus @200 MkResponsePasswordChange {responsePasswordChangeUnit = ()}

pictureUpload ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 (StaticText "Uploaded profile picture.")) responses
  , IsMember (WithStatus 400 ErrorParseBodyJpeg) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String ImageJpegBytes ->
  m (Union responses)
pictureUpload auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBodyJpeg eitherRequest $ \request -> do
      logInfo "Changing profile picture."
      case jpegConvertProfilePicture request of
        Left err -> do
          logWarn $ "Failed to resize picture: " <> T.pack (show err)
          respond $ WithStatus @400 $ MkErrorParseBodyJpeg "Unable to read picture."
        Right picture -> do
          logInfo "Successfully verified and potentially resized picture."
          seldaResult <-
            runSeldaTransactionT $
              userSetPicture (userAuthenticatedId authenticated) (Just picture)
          handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
            logInfo "Changed profile picture successfully."
            respond $ WithStatus @200 $ MkStaticText @"Uploaded profile picture."

pictureDelete ::
  ( MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 (StaticText "Deleted profile picture.")) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  m (Union responses)
pictureDelete auth =
  handleAuthBearer auth $ \authenticated -> do
    logInfo "Deleting profile picture."
    seldaResult <-
      runSeldaTransactionT $
        userSetPicture (userAuthenticatedId authenticated) Nothing
    handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
      logInfo "Deleted profile picture successfully."
      respond $ WithStatus @200 $ MkStaticText @"Deleted profile picture."

pictureDownload ::
  ( MonadConfigured m
  , MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  ) =>
  AuthResult UserAuthenticated ->
  Either T.Text IdentifierUser ->
  m ImageJpegBytes
pictureDownload auth eitherQueryParamIdentifierUser = do
  handledResult <- do
    handleAuthBearer auth $ \_ -> do
      case eitherQueryParamIdentifierUser of
        Left err -> do
          logInfo "Unable to parse user identifier."
          respond $ WithStatus @400 err -- TODO: Handle with proper type for error. `ErrorBadQueryParam`
        Right identifierUser -> do
          logDebug $ "Requesting a profile picture from " <> T.pack (show identifierUser)
          seldaResult <-
            runSeldaTransactionT $
              userGetPicture identifierUser
          handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \maybePicture -> do
            logInfo "Checked out profile picture successfully."
            case maybePicture of
              Nothing -> do
                logInfo "No profile picture set for this user. Returning default picture."
                defaultProfilePictureFilePath <- (<> "/default-profile-picture.jpeg") . configDirectoryStatic <$> configuration
                picture <- liftIO $ B.readFile defaultProfilePictureFilePath
                respond $ WithStatus @200 $ MkImageJpegBytes . B.fromStrict $ picture
              Just picture -> do
                logInfo "Answering with profile picture."
                respond $ WithStatus @200 $ MkImageJpegBytes . unByteStringJpeg $ picture
  logDebug $ "Handling multi-mimetype response manually: " <> T.pack (show handledResult) -- TODO: Logging pictures right now.
  -- TODO: Add all these HTTP statuses to the API definition. Requires different output types.
  case handledResult :: Union [WithStatus 401 ErrorBearerAuth, WithStatus 500 (), WithStatus 400 T.Text, WithStatus 200 ImageJpegBytes] of
    SOP.Z (SOP.I (WithStatus errorBearerAuth)) -> error $ show errorBearerAuth
    SOP.S (SOP.Z (SOP.I (WithStatus ()))) -> undefined
    SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus errorBadQueryParam)))) -> error $ show errorBadQueryParam
    SOP.S (SOP.S (SOP.S (SOP.Z (SOP.I (WithStatus result))))) -> pure result
    SOP.S (SOP.S (SOP.S (SOP.S impossibleCase))) -> case impossibleCase of {}

confirmationRequest ::
  ( MonadConfigured m
  , MonadEmail m
  , MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseConfirmationRequest) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  m (Union responses)
confirmationRequest auth =
  handleAuthBearer auth $ \authenticated -> do
    logDebug $ "Requesting email confirmation for user: " <> T.pack (show authenticated)
    seldaResult <- runSeldaTransactionT $ do
      user <- userGet (userAuthenticatedId authenticated)
      let effect = MkConfirmationEffectEmailValidation $ userEmail user
      expirationTime <- lift . liftIO $ (T.secondsToNominalDiffTime (60 * 60) `T.addUTCTime`) <$> T.getCurrentTime
      confirmationSecret <- userConfirmationCreate (userId user) effect expirationTime
      lift $ logDebug "Sending confirmation email."
      config <- lift configuration
      sendEmailResult <-
        lift $
          sendEmail
            MkEmail
              { emailRecipient = userEmail user
              , emailTitle = "Email Address Verification: " <> toPrettyText (userName user)
              , emailBodyHtml = TL.toStrict $ T.renderHtml $ H.docTypeHtml $ do
                  H.head $ do
                    H.title $ H.text "Email Address Verification: " <> toPrettyHtml5 (userName user)
                  H.body $ do
                    H.p $ H.text "Click the link to confirm your email address."
                    let confirmLink :: T.Text = displayBaseUrl (configBaseUrl config) <> "register/confirm/" <> unConfirmationSecret confirmationSecret
                    H.p $ H.a H.! H.A.href (H.textValue confirmLink) $ H.text confirmLink
                    H.div $ H.small $ H.text "If you did not register this account, feel free to ignore this message."
              }
      case sendEmailResult of
        EmailSent -> lift $ logDebug "Sent confirmation email."
        EmailFailedToSend -> error "Failed to send email."
    handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
      respond $
        WithStatus @200
          MkResponseConfirmationRequest
            { responseConfirmationRequestUnit = ()
            }

confirm ::
  ( MonadEmail m
  , MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseConfirm) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 410 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestConfirm ->
  m (Union responses)
confirm auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    case eitherRequest of
      Left err -> do
        logInfo $ "Failed to parse request: " <> T.pack (show err)
        respond $ WithStatus @400 $ MkErrorParseBodyJson err
      Right request@MkRequestConfirm {requestConfirmSecret} -> do
        logDebug $ "Running confirmation: " <> T.pack (show request)
        seldaResult <-
          runSeldaTransactionT $
            userConfirmationConfirm (userAuthenticatedId authenticated) requestConfirmSecret
        handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \case
          Left err -> do
            logInfo "Failed to confirm."
            case err of
              MkConfirmationErrorExpired ->
                respond $ WithStatus @410 ()
              MkConfirmationErrorEffectInvalid ->
                respond $ WithStatus @500 ()
          Right () -> do
            logInfo "Ran confirmation."
            respond $
              WithStatus @200
                MkResponseConfirm
                  { responseConfirmUnit = ()
                  }

notificationPreferences ::
  ( MonadEmail m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseNotifications) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 403 (StaticText "Email address is not verified.")) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestNotifications ->
  m (Union responses)
notificationPreferences auth eitherRequest =
  handleAuthBearer auth $ \authenticated ->
    handleBadRequestBody eitherRequest $ \request -> do
      logInfo "Getting/Setting user notification preferences."
      seldaResult <-
        runSeldaTransactionT $ do
          let getNotificationSettings =
                userNotificationsPreferencesEmailGet (userAuthenticatedId authenticated) >>= \case
                  MkEmailPreferencesSend _ -> pure True
                  MkEmailPreferencesDontSend -> pure False
          case requestNotificationsReceiveEmailNotifications request of
            Nothing -> do
              lift $ logDebug "Just getting user notification preferences."
              getNotificationSettings
            Just newPreferences -> do
              lift $ logDebug "Setting new user notification preferences."
              userNotificationsPreferencesEmailSet (userAuthenticatedId authenticated) newPreferences
              getNotificationSettings
      handleSeldaException
        (Proxy @SqlErrorMensamEmailNotVerified)
        (WithStatus @403 $ MkStaticText @"Email address is not verified.")
        seldaResult
        $ \seldaResultAfter403 -> do
          handleSeldaSomeException (WithStatus @500 ()) seldaResultAfter403 $ \receiveEmailNotifications -> do
            respond $
              WithStatus @200
                MkResponseNotifications
                  { responseNotificationsReceiveEmailNotifications = receiveEmailNotifications
                  }

profile ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseProfile) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
  , IsMember (WithStatus 401 ErrorBearerAuth) responses
  , IsMember (WithStatus 404 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  Either String RequestProfile ->
  m (Union responses)
profile auth eitherRequest =
  handleAuthBearer auth $ \_authenticated ->
    case eitherRequest of
      Left err -> do
        logInfo $ "Failed to parse request: " <> T.pack (show err)
        respond $ WithStatus @400 $ MkErrorParseBodyJson err
      Right request@MkRequestProfile {requestProfileUser} -> do
        logDebug $ "Looking up user profile: " <> T.pack (show request)
        seldaResult <- runSeldaTransactionT $ do
          maybeUserIdentifier <- case requestProfileUser of
            Name name -> userLookupId name
            Identifier identifier -> pure $ Just identifier
          case maybeUserIdentifier of
            Nothing -> pure Nothing
            Just userIdentifier -> Just <$> userGet userIdentifier
        handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \case
          Nothing -> do
            logWarn "No such user profile."
            respond $ WithStatus @404 ()
          Just user ->
            respond $
              WithStatus @200 $
                MkResponseProfile
                  { responseProfileId = userId user
                  , responseProfileName = userName user
                  , responseProfileEmail =
                      if userEmailVisible user
                        then Just $ userEmail user
                        else Nothing
                  , responseProfileEmailVerified = userEmailValidated user
                  }

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
