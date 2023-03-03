module Mensam.Server.Route.User where

import Mensam.Server.Route.User.Type
import Mensam.User

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time qualified as T
import Servant.Auth.Server
import Servant.Server.Generic

handler ::
  (MonadIO m, MonadLogger m) =>
  Routes (AsServerT m)
handler =
  Routes
    { routeLogin = login
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
          case TL.decodeUtf8' jwtByteString of
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
