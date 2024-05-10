module Mensam.Server.Server.Route.Api.User where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.API.Route.Api.User
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Email.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Application.SeldaPool.Servant
import Mensam.Server.Configuration
import Mensam.Server.Configuration.BaseUrl
import Mensam.Server.Secrets
import Mensam.Server.Server.Auth
import Mensam.Server.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Class
import Data.ByteString qualified as B
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time qualified as T
import Servant hiding (BasicAuthResult (..))
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
    , routeConfirm = confirm
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
    Right request@MkRequestRegister {requestRegisterName, requestRegisterPassword, requestRegisterEmail, requestRegisterEmailVisible} -> do
      logDebug $ "Registering new user: " <> T.pack (show request)
      seldaResult <-
        runSeldaTransactionT $ do
          userIdentifier <-
            userCreate
              requestRegisterName
              (mkPassword requestRegisterPassword)
              requestRegisterEmail
              requestRegisterEmailVisible
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
            (mkPassword $ requestPasswordChangeNewPassword request)
      handleSeldaSomeException (WithStatus @500 ()) seldaResult $ \() -> do
        logInfo "Changed user password successfully."
        respond $ WithStatus @200 MkResponsePasswordChange {responsePasswordChangeUnit = ()}

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
