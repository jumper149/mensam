module Mensam.Server.Server.Route.Api.User where

import Mensam.API.Aeson
import Mensam.API.Data.User
import Mensam.API.Data.User.Username
import Mensam.API.Route.Api.User
import Mensam.Server.Application.Configured.Class
import Mensam.Server.Application.Secret.Class
import Mensam.Server.Application.SeldaPool.Class
import Mensam.Server.Configuration
import Mensam.Server.Secrets
import Mensam.Server.Server.Auth
import Mensam.Server.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.ByteString qualified as B
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time qualified as T
import Servant hiding (BasicAuthResult (..))
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadConfigured m, MonadIO m, MonadLogger m, MonadSecret m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
    , routeLogout = logout
    , routeRegister = register
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
  handleAuth auth $ \authenticatedWithoutSession -> do
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
    case seldaResult of
      SeldaFailure _ -> do
        -- TODO: Here we can theoretically return a more accurate error
        logWarn "Failed to create new session."
        respond $ WithStatus @500 ()
      SeldaSuccess sessionIdentifier -> do
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
                logInfo "User logged in successfully."
                respond $
                  WithStatus @200
                    MkResponseLogin
                      { responseLoginJwt = jwt
                      , responseLoginExpiration = maybeTimeout
                      }

logout ::
  ( MonadConfigured m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseLogout) responses
  , IsMember (WithStatus 401 ErrorBasicAuth) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  AuthResult UserAuthenticated ->
  m (Union responses)
logout auth =
  handleAuth auth $ \authenticated -> do
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
        case seldaResult of
          SeldaFailure _ -> do
            -- TODO: Here we can theoretically return a more accurate error
            logWarn "Failed to delete session."
            respond $ WithStatus @500 ()
          SeldaSuccess () -> do
            logInfo "User loggout out successfully"
            respond $
              WithStatus @200
                MkResponseLogout
                  { responseLogoutUnit = ()
                  }

register ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 201 ()) responses
  , IsMember (WithStatus 400 ErrorParseBodyJson) responses
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
        runSeldaTransactionT $
          userCreate
            requestRegisterName
            (mkPassword requestRegisterPassword)
            requestRegisterEmail
            requestRegisterEmailVisible
      case seldaResult of
        SeldaFailure _err -> do
          -- TODO: Here we can theoretically return a more accurate error
          logWarn "Failed to register new user."
          respond $ WithStatus @500 ()
        SeldaSuccess _userIdentifier -> do
          logInfo "Registered new user."
          respond $ WithStatus @201 ()

profile ::
  ( MonadIO m
  , MonadLogger m
  , MonadSeldaPool m
  , IsMember (WithStatus 200 ResponseProfile) responses
  , IsMember (WithStatus 400 ()) responses
  , IsMember (WithStatus 500 ()) responses
  ) =>
  Either T.Text Username ->
  m (Union responses)
profile eitherUsername =
  case eitherUsername of
    Left err -> do
      logInfo $ "Failed to parse username: " <> T.pack (show err)
      respond $ WithStatus @400 ()
    Right username -> do
      logDebug $ "Looking up user profile: " <> T.pack (show username)
      seldaResultMaybeUser <- runSeldaTransactionT $ do
        maybeUserIdentifier <- userLookupId username
        case maybeUserIdentifier of
          Nothing -> pure Nothing
          Just userIdentifier -> Just <$> userGet userIdentifier
      case seldaResultMaybeUser of
        SeldaFailure _err -> do
          logError "Failed to look up user profile."
          respond $ WithStatus @500 ()
        SeldaSuccess Nothing -> do
          logWarn "Failed to look up user profile."
          respond $ WithStatus @400 ()
        SeldaSuccess (Just user) -> do
          let response =
                MkResponseProfile
                  { responseProfileId = T.pack $ show $ userId user
                  , responseProfileName = userName user
                  , responseProfileEmail = userEmail user
                  }
          logInfo "Looked up user profile."
          respond $ WithStatus @200 response
