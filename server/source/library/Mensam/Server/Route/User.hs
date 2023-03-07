module Mensam.Server.Route.User where

import Mensam.Application.SeldaPool.Class
import Mensam.Server.Route.User.Type
import Mensam.User

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
  m (Union [WithStatus 200 (), WithStatus 400 ()])
register eitherRequest =
  case eitherRequest of
    Left err -> do
      logInfo $ "Failed to parse request: " <> T.pack (show err)
      respond $ WithStatus @400 ()
    Right request@MkRequestRegister {requestRegisterName, requestRegisterPassword, requestRegisterEmail} -> do
      logDebug $ "Registering new user: " <> T.pack (show request)
      runSeldaTransactionT $ userCreate requestRegisterName requestRegisterEmail (mkPassword requestRegisterPassword)
      logInfo "Registered new user."
      respond $ WithStatus @200 ()
