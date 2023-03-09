module Mensam.Server.Route.User where

import Mensam.Application.SeldaPool.Class
import Mensam.Server.Auth
import Mensam.Server.Route.User.Type
import Mensam.User
import Mensam.User.Username

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
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
    , routeRegister = register
    , routeProfile = profile
    }

login ::
  (MonadIO m, MonadLogger m) =>
  AuthResult User ->
  m (Union [WithStatus 200 ResponseLogin, WithStatus 400 (), WithStatus 401 (), WithStatus 500 ()])
login authUser =
  case authUser of
    Authenticated user -> do
      logDebug $ "Creating JWT for user: " <> T.pack (show user)
      let timeout :: Maybe T.UTCTime = Nothing
      logDebug $ "JWT timeout has been set: " <> T.pack (show timeout)
      eitherJwt <- liftIO $ makeJWT user jwtSettings timeout
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
              logInfo "Created JWT successfully."
              logInfo "User logged in successfully."
              respond $ WithStatus @200 MkResponseLogin {responseLoginJWT = jwtText}
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> respond $ WithStatus @401 ()
        NoSuchUser -> respond $ WithStatus @401 ()
        Indefinite -> respond $ WithStatus @400 ()

register ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Either String RequestRegister ->
  m (Union [WithStatus 200 (), WithStatus 400 (), WithStatus 500 ()])
register eitherRequest =
  case eitherRequest of
    Left err -> do
      logInfo $ "Failed to parse request: " <> T.pack (show err)
      respond $ WithStatus @400 ()
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
        SeldaSuccess () -> do
          logInfo "Registered new user."
          respond $ WithStatus @200 ()

profile ::
  (MonadIO m, MonadLogger m, MonadSeldaPool m) =>
  Either T.Text Username ->
  m (Union '[WithStatus 200 ResponseProfile, WithStatus 400 (), WithStatus 404 (), WithStatus 500 ()])
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
