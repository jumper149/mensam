module Mensam.Server.Route.User where

import Mensam.Application.SeldaConnection.Class
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
  (MonadIO m, MonadLogger m, MonadSeldaConnection m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
    , routeRegister = register
    }

-- TODO: Fix `undefined` branches with HTTP failure.
login :: (MonadIO m, MonadLogger m) => AuthResult User -> m ResponseLogin
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
          undefined
        Right jwtByteString ->
          case T.decodeUtf8' $ B.toStrict jwtByteString of
            Left err -> do
              logError $ "Failed to decode JWT as UTF-8: " <> T.pack (show err)
              undefined
            Right jwtText -> do
              logInfo "Created JWT successfully."
              logInfo "User logged in successfully."
              pure MkResponseLogin {responseLoginJWT = jwtText}
    failedAuthentication ->
      case failedAuthentication of
        BadPassword -> undefined
        NoSuchUser -> undefined
        Indefinite -> undefined

register :: (MonadIO m, MonadLogger m, MonadSeldaConnection m) => Either String RequestRegister -> m NoContent
register eitherRequest =
  case eitherRequest of
    Left err -> do
      logInfo $ "Failed to parse request: " <> T.pack (show err)
      undefined
    Right request@MkRequestRegister {requestRegisterName, requestRegisterPassword, requestRegisterEmail} -> do
      logDebug $ "Registering new user: " <> T.pack (show request)
      let
        user =
          MkUser
            { userName = requestRegisterName
            , userEmail = requestRegisterEmail
            }
        password = mkPassword requestRegisterPassword
      userCreate user password
      logInfo "Registered new user."
      pure NoContent
